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
                     (AcquireFailure (..), Target (..))

localStateQueryServer ::
     forall m blk.
     ( IOLike m
     , BlockSupportsLedgerQuery blk
     , Query.ConfigSupportsNode blk
     , LedgerSupportsProtocol blk
     )
  => ExtLedgerCfg blk
  -> ( StrictTVar m (Maybe (Point blk)))
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
                  -> Bool
                  -> m (ServerStAcquiring blk (Point blk) (Query blk) m ())
    handleAcquire mpt leashed = do
      traceM $ "handleAcquire: start " <> show leashed 
      getView mpt >>= \case 
        -- case if we want to leash and there is a leashing point var
        Right forker
          | leashed ->
            atomically $ do
              traceM $ "SETTING LEASHING POINT VARIABLE" 
              readTVar leashingPointVar >>= \case
                Nothing -> do
                  leashingPoint <- case mpt of
                    SpecificPoint p -> pure p
                    _ -> getCurrentChainAnchorPoint
                  writeTVar leashingPointVar $ Just leashingPoint
                  traceM $ "LEASHING POINT VARIABLE IS " <> show leashingPoint 
                  pure $ SendMsgAcquired $ acquired True forker
                -- return error if already leashed
                Just _ -> pure $ SendMsgFailure AcquireFailurePointStateIsBusy idle
        Right forker -> pure $ SendMsgAcquired $ acquired False forker
        Left PointTooOld{} -> pure $ SendMsgFailure AcquireFailurePointTooOld idle
        Left PointNotOnChain -> pure $ SendMsgFailure AcquireFailurePointNotOnChain idle

    acquired :: Bool
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
          atomically $ writeTVar leashingPointVar Nothing 
          return idle
        }
      where
        close = roforkerClose forker

    handleQuery ::
         Bool
      -> ReadOnlyForker' m blk
      -> Query blk result
      -> m (ServerStQuerying blk (Point blk) (Query blk) m () result)
    handleQuery leashed forker query = do
      result <- Query.answerQuery cfg forker query
      return $ SendMsgResult result (acquired leashed forker)
