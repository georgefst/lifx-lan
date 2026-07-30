# ⚠️ Looking for maintainers! ⚠️

Now that LIFX have given up on Europe, I'm less inclined to work on this project. Let me know if you're interested.

## Haskell bindings to the [LIFX LAN API](https://lan.developer.lifx.com/docs).

This library provides a reasonably high-level interface, but doesn't try to be *too* clever.
Messages and response types map directly to the low-level API (with links provided in the documentation).

Since the protocol runs over UDP, a dropped packet is a normal occurrence rather than a fault.
Messages which expect a response are retried if it doesn't arrive (configurable via `LifxConfig`),
and give up with a `RecvTimeout` after that. Messages which expect no response - the `Set*` ones -
are fire-and-forget, and their delivery is not checked; see the docs on `sendMessage'` for why.

It does not yet cover the full API, but PRs are very welcome and some functionality may be added on request.
