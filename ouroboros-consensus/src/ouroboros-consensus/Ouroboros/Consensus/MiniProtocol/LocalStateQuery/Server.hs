{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Ouroboros.Consensus.MiniProtocol.LocalStateQuery.Server (localStateQueryServer) where

import Debug.Trace (traceM)
import qualified Data.Map.Strict as Map
import Ouroboros.Consensus.HeaderValidation (HeaderWithTime (..))
import Ouroboros.Network.AnchoredFragment (AnchoredFragment)
import qualified Ouroboros.Network.AnchoredFragment as AF
import Ouroboros.Consensus.Storage.ChainDB.API (LeashingState)
import Ouroboros.Consensus.Block
import Ouroboros.Consensus.Ledger.Extended
import Ouroboros.Consensus.Ledger.Query
  ( BlockSupportsLedgerQuery
  , Query
  )
import qualified Ouroboros.Consensus.Ledger.Query as Query
import Ouroboros.Consensus.Ledger.SupportsProtocol
  ( LedgerSupportsProtocol
  )
import Ouroboros.Consensus.Storage.LedgerDB
import Ouroboros.Consensus.Util.IOLike
import Ouroboros.Network.Protocol.LocalStateQuery.Server
import Ouroboros.Network.Protocol.LocalStateQuery.Type
  ( AcquireFailure (..)
  , Target (..)
  , LeashID
  )

localStateQueryServer ::
  forall m blk.
  ( IOLike m
  , BlockSupportsLedgerQuery blk
  , Query.ConfigSupportsNode blk
  , LedgerSupportsProtocol blk
  ) =>
  ExtLedgerCfg blk ->
  ( StrictTVar m (LeashingState blk)) ->
  ( STM m (AnchoredFragment (HeaderWithTime blk)) ) ->
  ( Target (Point blk) ->
    m (Either GetForkerError (ReadOnlyForker' m blk))
  ) ->
  LocalStateQueryServer blk (Point blk) (Query blk) m ()
localStateQueryServer cfg leashingStateVar getCurrentChain getView =
  LocalStateQueryServer $ return idle
 where
  idle :: ServerStIdle blk (Point blk) (Query blk) m ()
  idle =
    ServerStIdle
      { recvMsgAcquire = \tgt leashId -> do
          traceM $ "idle: handle acquire" 
          handleAcquire tgt leashId
      , recvMsgDone = return ()
      }

  handleAcquire :: Target (Point blk)
                -> Maybe LeashID
                -> m (ServerStAcquiring blk (Point blk) (Query blk) m ())
  handleAcquire mpt mLeashId = do
    traceM $ "handleAcquire: start " <> show mLeashId 
    getView mpt >>= \case 
      -- case if we want to leash and there is a leashing state var
      Right forker
        | Just leashId <- mLeashId -> do
          traceM $ "My leash id " <> show leashId 
          atomically $ do
            leashingState <- readTVar leashingStateVar
            case Map.lookup leashId leashingState of
              Nothing -> do
                currentChain <- getCurrentChain 
                let
                  leashingFragment = case mpt of
                      ImmutableTip -> AF.Empty $ AF.anchor currentChain
                      SpecificPoint p -> AF.takeWhileOldest (\(HeaderWithTime h _) -> headerPoint h <= p) currentChain
                      VolatileTip -> currentChain
                let newState = Map.insert leashId leashingFragment leashingState 
                writeTVar leashingStateVar newState
                pure $ SendMsgAcquired $ acquired mLeashId forker
              Just _ -> pure $ SendMsgFailure AcquireFailurePointStateIsBusy idle
      Right forker -> pure $ SendMsgAcquired $ acquired Nothing forker
      Left PointTooOld{} -> pure $ SendMsgFailure AcquireFailurePointTooOld idle
      Left PointNotOnChain -> pure $ SendMsgFailure AcquireFailurePointNotOnChain idle

  acquired :: Maybe LeashID
           -> ReadOnlyForker' m blk
           -> ServerStAcquired blk (Point blk) (Query blk) m ()
  acquired clientLeashId forker = ServerStAcquired {
        recvMsgQuery     = do
          traceM "acquire: query"
          handleQuery clientLeashId forker
      , recvMsgReAcquire = \mp -> do
          traceM $ "acquired: re acquire, leash " <> show clientLeashId
          close
          handleAcquire mp clientLeashId
      , recvMsgRelease   = \msgLeashId -> do
          traceM $ "acquired: release, leash " <> show msgLeashId
          close
          _ <- traverse releaseLeash msgLeashId
          return idle
      }
    where
      close = roforkerClose forker

  handleQuery ::
    Maybe LeashID ->
    ReadOnlyForker' m blk ->
    Query blk result ->
    m (ServerStQuerying blk (Point blk) (Query blk) m () result)
  handleQuery leashId forker query = do
    result <- Query.answerQuery cfg forker query
    return $ SendMsgResult result (acquired leashId forker)

  releaseLeash :: LeashID -> m ()
  releaseLeash leashId = atomically $ do
    leashingState <- readTVar leashingStateVar
    let newState = Map.delete leashId leashingState 
    writeTVar leashingStateVar newState
