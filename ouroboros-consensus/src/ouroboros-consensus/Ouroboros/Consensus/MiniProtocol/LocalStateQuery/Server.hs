{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Ouroboros.Consensus.MiniProtocol.LocalStateQuery.Server (localStateQueryServer) where

import Debug.Trace (traceM)
import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.Ledger.Extended
import           Ouroboros.Consensus.Ledger.Query (BlockSupportsLedgerQuery,
                     Query)
import qualified Ouroboros.Consensus.Ledger.Query as Query
import           Ouroboros.Consensus.Ledger.SupportsProtocol
                     (LedgerSupportsProtocol)
import           Ouroboros.Consensus.Storage.LedgerDB
import           Ouroboros.Consensus.Util.IOLike
import           Ouroboros.Network.Protocol.LocalStateQuery.Server
import           Ouroboros.Network.Protocol.LocalStateQuery.Type
                     (AcquireFailure (..), Target (..), LeashID)

localStateQueryServer ::
     forall m blk.
     ( IOLike m
     , BlockSupportsLedgerQuery blk
     , Query.ConfigSupportsNode blk
     , LedgerSupportsProtocol blk
     )
  => ExtLedgerCfg blk
  -> ( StrictTVar m (Maybe (LeashID, Point blk)))
  -> ( STM m (Point blk) )
  -> (   Target (Point blk)
      -> m (Either GetForkerError (ReadOnlyForker' m blk))
     )
  -> LocalStateQueryServer blk (Point blk) (Query blk) m ()
localStateQueryServer cfg leashingPointVar getCurrentChainAnchorPoint getView =
    LocalStateQueryServer $ return idle
  where
    idle :: ServerStIdle blk (Point blk) (Query blk) m ()
    idle = ServerStIdle {
          recvMsgAcquire = \tgt leashId -> do
            traceM $ "idle: handle acquire"
            handleAcquire tgt leashId
        , recvMsgDone    = do
            traceM "idle: done"
            return ()
        }

    handleAcquire :: Target (Point blk)
                  -> Maybe LeashID
                  -> m (ServerStAcquiring blk (Point blk) (Query blk) m ())
    handleAcquire mpt mLeashId = do
      traceM $ "handleAcquire: start " <> show mLeashId
      getView mpt >>= \case
        Right forker | Just leashId <- mLeashId -> do
            atomically (acquireLeash mpt leashId) >>= \case
              Just err -> pure $ SendMsgFailure err idle
              Nothing  -> pure $ SendMsgAcquired $ acquired (Just leashId) forker
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
            releaseLeash msgLeashId
            return idle
        }
      where
        close = roforkerClose forker

    handleQuery ::
         Maybe LeashID
      -> ReadOnlyForker' m blk
      -> Query blk result
      -> m (ServerStQuerying blk (Point blk) (Query blk) m () result)
    handleQuery leashId forker query = do
      result <- Query.answerQuery cfg forker query
      return $ SendMsgResult result (acquired leashId forker)

    -- TODO: This should always compare with getCurrentChainAnchorPoint to
    -- check if the point is too old.
    requestedPoint mpt = case mpt of
        SpecificPoint p -> pure p
        _ -> getCurrentChainAnchorPoint

    acquireLeash mpt messageLeashId = do
        traceM $ "SETTING LEASHING POINT VARIABLE"
        readTVar leashingPointVar >>= \case
          Nothing -> do
            -- TODO: What happens if the point is too old?
            leashPoint <- requestedPoint mpt
            let newLeashState = (messageLeashId, leashPoint)
            traceM $ "START LEASHING, LEASHING POINT VARIABLE IS " <> show newLeashState
            writeTVar leashingPointVar $ Just newLeashState
            pure Nothing

          Just currentState@(currentLeashId, currentLeashPoint)
            | messageLeashId /= currentLeashId -> do
              traceM $ unwords ["ATTEMPTED TO UPDATE LEASH WITH WRONG ID, current:",
                                show currentLeashId, "provided:", show messageLeashId]
              pure $ Just AcquireFailurePointStateIsBusy

            | otherwise -> do
                requestedLeashPoint <- requestedPoint mpt
                let newState = (messageLeashId, requestedLeashPoint)
                if requestedLeashPoint < currentLeashPoint
                then do
                  traceM $ unwords ["ATTEMPTED TO LEASH BACKWARDS:", show currentState, "=>", show newState]
                  pure $ Just AcquireFailurePointStateIsBusy
                else do
                  traceM $ "UPDATED LEASHING, LEASHING POINT VARIABLE IS " <> show newState
                  writeTVar leashingPointVar $ Just newState
                  pure Nothing

    releaseLeash :: Maybe LeashID -> m ()
    releaseLeash Nothing = pure ()
    releaseLeash (Just leashId) = atomically $
      readTVar leashingPointVar >>= \case
        Just x@(currentId,_)
          | leashId == currentId -> do
            traceM $ "LEASH RELEASED: " <> show x
            writeTVar leashingPointVar Nothing
        _ -> pure ()
