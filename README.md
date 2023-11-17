# ⚠️ Looking for maintainers! ⚠️

Now that LIFX have given up on Europe, I'm less inclined to work on this project. Let me know if you're interested.

## Haskell bindings to the [LIFX LAN API](https://lan.developer.lifx.com/docs).

This library provides a reasonably high-level interface, but doesn't try to be *too* clever.
For example, it doesn't check message delivery, and throws an error if a light takes too long to respond.
Messages and response types map directly to the low-level API (with links provided in the documentation).

It does not yet cover the full API, but PRs are very welcome and some functionality may be added on request.
