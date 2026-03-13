{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Ouroboros.Consensus.MiniProtocol.LocalStateQuery.Server (localStateQueryServer) where

import Debug.Trace (traceM)
import Control.Monad (void)
import qualified Data.Map.Strict as Map
import Ouroboros.Consensus.HeaderValidation (HeaderWithTime (..))
import Ouroboros.Network.AnchoredFragment (AnchoredFragment)
import qualified Ouroboros.Network.AnchoredFragment as AF
import Ouroboros.Consensus.Storage.ChainDB.API (LsqLeashingState)
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
  , LeashId
  )

localStateQueryServer ::
  forall m blk.
  ( IOLike m
  , BlockSupportsLedgerQuery blk
  , Query.ConfigSupportsNode blk
  , LedgerSupportsProtocol blk
  ) =>
  ExtLedgerCfg blk ->
  ( StrictTVar m (LsqLeashingState blk)) ->
  ( STM m (AnchoredFragment (HeaderWithTime blk)) ) ->
  ( Target (Point blk) ->
    m (Either GetForkerError (ReadOnlyForker' m blk))
  ) ->
  LocalStateQueryServer blk (Point blk) (Query blk) m ()
localStateQueryServer cfg lsqLeashingStateVar getCurrentChain getView =
  LocalStateQueryServer $ return idle
 where
  idle :: ServerStIdle blk (Point blk) (Query blk) m ()
  idle =
    ServerStIdle
      { recvMsgAcquire = \tgt leashId -> do
          traceM $ "idle: handle acquire" 
          handleAcquire tgt leashId
      , recvMsgDone = \mLeashId -> 
          void $ traverse releaseLeash mLeashId
      }

  handleAcquire :: Target (Point blk)
                -> Maybe LeashId
                -> m (ServerStAcquiring blk (Point blk) (Query blk) m ())
  handleAcquire mpt mLeashId = do
    traceM $ "handleAcquire: start " <> show mLeashId 

    -- by @nfrisby:
    -- TODO: There's a race condition here; the selection might change between thegetViewcall and this getCurrentChain call.
    -- Might just have to add the "chain that was the current chain at the time of the call" to the range of ChainDB.getReadOnlyForkerAtPoint.
    -- TODO: or maybe a Forker's API already lets you determine what chain fragment it matches? I'm still not super-familiar with the UTxO HD api, which is where Forker comes from

    getView mpt >>= \case 
      -- case if we want to leash and there is a lsq leashing state var
      Right forker
        | Just leashId <- mLeashId -> do
          traceM $ "My leash id " <> show leashId 
          atomically $ do
            lsqLeashingState <- readTVar lsqLeashingStateVar
            currentChain <- getCurrentChain 
            let
              leashingFragment = case mpt of
                  ImmutableTip -> AF.Empty $ AF.anchor currentChain
                  SpecificPoint p -> AF.takeWhileOldest (\(HeaderWithTime h _) -> headerPoint h <= p) currentChain
                  VolatileTip -> currentChain
            let newState = Map.insert leashId leashingFragment lsqLeashingState 
            writeTVar lsqLeashingStateVar newState
            pure $ SendMsgAcquired $ acquired mLeashId forker
      Right forker -> pure $ SendMsgAcquired $ acquired Nothing forker
      Left PointTooOld{} -> pure $ SendMsgFailure AcquireFailurePointTooOld idle
      Left PointNotOnChain -> pure $ SendMsgFailure AcquireFailurePointNotOnChain idle

  acquired :: Maybe LeashId
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
      , recvMsgRelease   = do
          traceM $ "acquired: release, leash " <> show clientLeashId 
          close
          void $ traverse releaseLeash clientLeashId 
          return idle
      }
    where
      close = roforkerClose forker

  handleQuery ::
    Maybe LeashId ->
    ReadOnlyForker' m blk ->
    Query blk result ->
    m (ServerStQuerying blk (Point blk) (Query blk) m () result)
  handleQuery leashId forker query = do
    result <- Query.answerQuery cfg forker query
    return $ SendMsgResult result (acquired leashId forker)

  releaseLeash :: LeashId -> m ()
  releaseLeash leashId = atomically $ do
    lsqLeashingState <- readTVar lsqLeashingStateVar
    let newState = Map.delete leashId lsqLeashingState 
    writeTVar lsqLeashingStateVar newState
