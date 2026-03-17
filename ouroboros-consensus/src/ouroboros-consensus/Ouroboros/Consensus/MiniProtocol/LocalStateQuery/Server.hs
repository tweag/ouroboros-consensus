{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Ouroboros.Consensus.MiniProtocol.LocalStateQuery.Server (localStateQueryServer) where

import Debug.Trace (traceM)
import Control.Monad (when)
import Data.Foldable (for_, toList)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Ouroboros.Consensus.HeaderValidation (HeaderWithTime (..))
import Ouroboros.Network.AnchoredFragment (AnchoredFragment)
import qualified Ouroboros.Network.AnchoredFragment as AF
import Ouroboros.Consensus.Storage.ChainDB.API (LsqLeashingState(..))
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
  LocalStateQueryServer $ return (idle Nothing)
 where
  idle :: Maybe LeashId -> ServerStIdle blk (Point blk) (Query blk) m ()
  idle clientLeashId  =
    ServerStIdle
      { recvMsgAcquire = \tgt newLeashId -> do
          traceM $ "idle: handle acquire"
          -- TODO: The client can send a new LeashId here, which might be different
          -- to the already known clientLeashId - this should probably return an error
          -- if it doesn't match. Alternatively it could remove the existing ID from
          -- the LsqLeashingState using releaseLeash.
          handleAcquire tgt newLeashId
      -- TODO: MsgDone shouldn't need LeashId now, if there was a leashId it was
      -- passed as an argument.
      , recvMsgDone = \_notNeeded -> do
          traceM "MsgDone"
          deactivateLeashClient clientLeashId
          -- TODO: Decide if this is right
          -- void $ traverse releaseLeash clientLeashId
          return ()
      }

  handleAcquire :: Target (Point blk)
                -> Maybe LeashId
                -> m (ServerStAcquiring blk (Point blk) (Query blk) m ())
  handleAcquire mpt newLeashId = do
    traceM $ "handleAcquire: start " <> show newLeashId

    -- by @nfrisby:
    -- TODO: There's a race condition here; the selection might change between thegetViewcall and this getCurrentChain call.
    -- Might just have to add the "chain that was the current chain at the time of the call" to the range of ChainDB.getReadOnlyForkerAtPoint.
    -- TODO: or maybe a Forker's API already lets you determine what chain fragment it matches? I'm still not super-familiar with the UTxO HD api, which is where Forker comes from

    getView mpt >>= \case
      -- case if we want to leash and there is a lsq leashing state var
      Right forker
        | Just leashId <- newLeashId -> do
          traceM $ "My leash id " <> show leashId
          atomically $ do
            activeClients <- lsqActiveClients <$> readTVar lsqLeashingStateVar
            if Set.member leashId activeClients
            then do
              traceM $ "LeashId already in use: " <> show (leashId, activeClients)
              -- TODO: Need a busy error again - AcquireFailureLeashIdInUse
              pure $ SendMsgFailure AcquireFailurePointNotOnChain (idle Nothing)
            else do
              handleAcquireLeash mpt leashId
              pure $ SendMsgAcquired $ acquired (Just leashId) forker
      Right forker -> pure $ SendMsgAcquired $ acquired Nothing forker
      Left PointTooOld{} -> pure $ SendMsgFailure AcquireFailurePointTooOld (idle newLeashId)
      Left PointNotOnChain -> pure $ SendMsgFailure AcquireFailurePointNotOnChain (idle newLeashId)

  handleAcquireLeash :: Target (Point blk) -> LeashId -> STM m ()
  handleAcquireLeash mpt leashId = do
    lsqLeashingState <- readTVar lsqLeashingStateVar
    currentChain <- getCurrentChain
    let
      leashingFragment = case mpt of
          ImmutableTip -> AF.Empty $ AF.anchor currentChain
          SpecificPoint p -> AF.takeWhileOldest (\(HeaderWithTime h _) -> headerPoint h <= p) currentChain
          VolatileTip -> currentChain

    writeTVar lsqLeashingStateVar LsqLeashingState {
      lsqLeashes = Map.insert leashId leashingFragment (lsqLeashes lsqLeashingState),
      lsqActiveClients = Set.insert leashId (lsqActiveClients lsqLeashingState)
    }


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
      , recvMsgRelease   = \unleash -> do
          traceM $ "acquired: release, leash " <> show clientLeashId
          close
          when unleash $ for_ clientLeashId releaseLeash
          return (idle clientLeashId)
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
  releaseLeash leashId = do
    active <- atomically $
      stateTVar lsqLeashingStateVar $ \l ->
        let l' = l{
          lsqLeashes = Map.delete leashId (lsqLeashes l) ,
          lsqActiveClients = Set.delete leashId (lsqActiveClients l)
          }
        in (toList $ lsqActiveClients l', l')
    traceM $ "currently live clients: " <> show active

  deactivateLeashClient :: Maybe LeashId -> m ()
  deactivateLeashClient Nothing = pure ()
  deactivateLeashClient (Just leashId) = atomically $ do
    traceM $ "Releasing active leash: " <> show leashId
    modifyTVar lsqLeashingStateVar $ \l ->
      l { lsqActiveClients = Set.delete leashId (lsqActiveClients l) }