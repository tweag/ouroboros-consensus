### Breaking

- Require negotiated Peras certificate diffusion to be caught up before the
  GSM enters `CaughtUp`. Track certificate client registration and idling
  separately from ChainSync; exclude vote diffusion from this condition.
- Pass negotiated `PerasSupport` through `aChainSyncClient` and
  `bracketChainSyncClient`, and expose it as `csPerasSupport`.
- Replace `GsmView.getChainSyncStates` with `getPeerStates` and add an inbound
  Object Diffusion state view for reporting idling to the GSM.
