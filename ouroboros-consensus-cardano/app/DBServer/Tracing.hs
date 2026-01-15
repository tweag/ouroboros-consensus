{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE PackageImports #-}

module DBServer.Tracing (getTrivialSendRecvTracer) where

import Ouroboros.Network.Driver.Simple (TraceSendRecv (..))
import "contra-tracer" Control.Tracer (Tracer, contramap)

showMessage :: forall ps. TraceSendRecv ps -> String
showMessage (TraceSendMsg _) = "send"
showMessage (TraceRecvMsg _) = "recv"

getTrivialSendRecvTracer ::
  forall m ps.
  Tracer m String ->
  Tracer m (TraceSendRecv ps)
getTrivialSendRecvTracer =
  contramap showMessage
