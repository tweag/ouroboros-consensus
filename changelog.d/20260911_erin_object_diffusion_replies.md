### Breaking

- Add an idle timeout argument to `objectDiffusionOutbound` and handle the
  Object Diffusion `MsgAwaitReply` and `MsgServerIdle` replies. Return agency
  after a bounded idle wait so clients can terminate gracefully.
