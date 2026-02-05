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
          recvMsgAcquire = \tgt leashed -> do
            traceM $ "idle: handle acquire"
            handleAcquire tgt leashed
        , recvMsgDone    = return ()
        }

    handleAcquire :: Target (Point blk)
                  -> Maybe LeashID
                  -> m (ServerStAcquiring blk (Point blk) (Query blk) m ())
    handleAcquire mpt leashed = do
      traceM $ "handleAcquire: start " <> show leashed
      getView mpt >>= \case
        -- case if we want to leash and there is a leashing point var
        Right forker | Just leashId <- leashed ->
            atomically $ putLeashPoint forker mpt leashId
        Right forker -> pure $ SendMsgAcquired $ acquired Nothing forker
        Left PointTooOld{} -> pure $ SendMsgFailure AcquireFailurePointTooOld idle
        Left PointNotOnChain -> pure $ SendMsgFailure AcquireFailurePointNotOnChain idle

    requestedPoint mpt = case mpt of
        SpecificPoint p -> pure p
        _ -> getCurrentChainAnchorPoint

    putLeashPoint forker mpt messageLeashId = do
        traceM $ "SETTING LEASHING POINT VARIABLE"
        readTVar leashingPointVar >>= \case
          Nothing -> do
            -- TODO: What happens if the point is too old?
            leashState <- (\p -> (messageLeashId,p)) <$> requestedPoint mpt
            writeTVar leashingPointVar $ Just leashState
            traceM $ "START LEASHING, LEASHING POINT VARIABLE IS " <> show leashState
            pure $ SendMsgAcquired $ acquired (Just messageLeashId) forker

          Just current@(currentLeashId, currentLeashPoint)
            | messageLeashId /= currentLeashId -> do
              traceM $ "ATTEMPTED TO LEASH WITH WRONG ID: " <> show (currentLeashId, messageLeashId)
              pure $ SendMsgFailure AcquireFailurePointStateIsBusy idle
            | otherwise -> do
                requestedLeashPoint <- requestedPoint mpt
                let newLeashState = (messageLeashId, requestedLeashPoint)
                if requestedLeashPoint < currentLeashPoint
                then do
                  traceM $ "ATTEMPTED TO LEASH BACKWARDS: "
                        <> show current
                        <> " => "
                        <> show newLeashState
                  pure $ SendMsgFailure AcquireFailurePointStateIsBusy idle
                else do
                  writeTVar leashingPointVar $ Just newLeashState
                  traceM $ "UPDATED LEASHING, LEASHING POINT VARIABLE IS " <> show newLeashState
                  pure $ SendMsgAcquired $ acquired (Just messageLeashId) forker

    acquired :: Maybe LeashID
             -> ReadOnlyForker' m blk
             -> ServerStAcquired blk (Point blk) (Query blk) m ()
    acquired leashed forker = ServerStAcquired {
          recvMsgQuery     = handleQuery leashed forker
        , recvMsgReAcquire = \mp -> do
            traceM $ "acquired: re acquire, leash " <> show leashed
            close
            handleAcquire mp leashed
        , recvMsgRelease   = do
            traceM $ "acquired: release, leash " <> show leashed
            close
            -- TODO: Is this right? I think we need to make unleashing explicit
            atomically $ writeTVar leashingPointVar Nothing
            return idle
        }
      where
        close = roforkerClose forker

    handleQuery ::
         Maybe LeashID
      -> ReadOnlyForker' m blk
      -> Query blk result
      -> m (ServerStQuerying blk (Point blk) (Query blk) m () result)
    handleQuery leashed forker query = do
      result <- Query.answerQuery cfg forker query
      return $ SendMsgResult result (acquired leashed forker)
