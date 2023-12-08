# Liquidity Ads

## Tasks

- model for Phoenix:
  - we always preserve the inbound liquidity they bought (ie we never splice-out below that amount)
  - we can do that by storing active lease details, with a duration of `0`
  - when splicing out, we look at active leases on that channel, and honor the maximum one
  - when storing leases with commitments, a lease should be stored with a specific commitment, since even an RBF could undo that lease
    - the lease should apply to that commitment and all commitments with a `fundingTxIndex` strictly greater
    - what do we do when this original commitment is pruned???
    - that means it must be stored in `Commitments`, but should reference the `fundingTxId`?
    - we should prune expired leases when pruning commitments
- add tests:
  - seller responds with a bigger funding amount than requested -> integration spec
- modify commit txs (`to_local` and `to_remote`):
  - add one output per lease and track two counters for it (`owed` and `current`)
  - when multiple leases are active, take from the closest expiry first
- when leasing liquidity:
  - ensure we don't raise our relay fees above what was negotiated while the lease is active
  - disallow mutual close and force-close commands during the lease
  - use a dedicated bitcoin wallet, on which we never lock utxos (abstract it away at the `BitcoinCoreClient` level)
    - but for Phoenix we will keep using the existing wallet
- when buying liquidity:
  - verify our peer doesn't raise their relay fees above what was negotiated: if they do, send a `warning` and log it
  - ignore remote `shutdown` messages? Send a `warning` and log?
- when we receive `tx_init_rbf`:
  - if they previously asked for liquidity and aren't asking for it anymore, remove our contribution
  - otherwise they would get liquidity for free by making an RBF attempt right after the funding attempt
- when we splice:
  - if the lease is still active, make sure it carries over to future commitments
  - splice-out can only take from the "main" output, not the leased ones
- lease renewal mechanism:
  - unnecessary, it's just a splice that uses the `request_funds` tlv

## Spec feedback

TODO
