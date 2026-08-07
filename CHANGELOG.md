# Revision history for lifx-lan

## 0.9 -- unreleased

### Error handling

- `LifxError` and `ProductLookupError` are now `Exception`s, with useful `displayException`s, and
  are thrown rather than being returned in a dedicated error channel.
- Add `isTransient`, distinguishing dropped packets (normal, and worth retrying) from errors which
  indicate a misbehaving device or a bug.
- Messages which expect a response are now retried when it doesn't arrive, and device discovery is
  retried when it doesn't find the requested number of devices. Configure via `LifxConfig`'s
  `retries` field; set it to `0` for the old behaviour.
- Fix `getSendResult` applying its timeout per `recv` rather than to the wait as a whole, which
  meant a steady trickle of unrelated packets could stop it ever timing out.
- Remove `WrongSender`. A response whose sequence number matched but whose sender didn't used to be
  an error; it is now ignored, like a sequence number mismatch, since sharing one socket across
  devices makes it a normal thing to see.

### Monad stack

- `LifxT` is now a plain `ReaderT`, rather than `StateT` over `ReaderT` over `ExceptT`. It
  therefore commutes with other transformers, and derives `MonadState`, `MonadError`, `MonadThrow`,
  `MonadCatch` and `MonadMask` from the underlying monad.
- Remove the `MonadError (Either e LifxError) (LifxT m)` instance, which no longer has any reason
  to exist.
- `MonadLifx` loses `MonadLifxError`, `lifxThrow` and `liftProductLookupError`, gaining a
  `MonadThrow` superclass instead. Its remaining methods have defaults for monad transformers, so
  instances for these can now be written with an empty body.
- `MonadLifxIO` gains a `MonadCatch` superclass, loses `lifxThrowIO`, and replaces `getTimeout`
  with `getConfig`.
- `Mock` sheds its `ExceptT` similarly, and `MockError` loses `MockProductLookupError`, since
  `getProductInfo` now always throws `LifxError`.

### Other

- `runLifxT` takes a `LifxConfig` record instead of positional arguments, and returns `m a` rather
  than `m (Either LifxError a)`. Timeouts are `NominalDiffTime`s rather than bare `Int`s of
  microseconds.
- Split the single timeout in two, since it was being used for two quite different jobs.
  `messageTimeout` is a failure deadline for a message to one device, which in normal operation
  never elapses, and so wants to be around the round trip time to a device. `broadcastTimeout` is
  how long to spend collecting responses to a broadcast; since there's no way to know how many
  devices are out there, it *always* elapses in full, so setting it too low silently finds fewer
  devices rather than reporting an error. Conflating the two meant you had to pick a single value
  which was necessarily far too long for one job or far too short for the other.
- Add a port option to `LifxConfig`, useful with a firewall which blocks most ports.
- Close the socket when a run fails, rather than leaking it.
- Support GHC 9.14.

## 0.8.3 -- 2024-09-03
- Update to latest products list.

## 0.8 -- 2023-02-25
- Various minor improvements to MTL interfaces.
- Update to latest products list.

## 0.7.1 -- 2022-10-21
- Handle invalid UTF-8 in light label.
- Ensure discovery exits successfully when zero devices are wanted.

## 0.7 -- 2022-02-20
- Drop support for GHC < 9.2.
    - If anyone is stuck on an older version of GHC and needs recent features of `lifx-lan` then please let me know. It would be reasonably easy to create a branch for it.
- Don't provide field selector functions for any types. Using `OverloadedRecordDot` in client code is recommended. We still export `unLifxT` as a normal function, for backward compatibility.
- Move much of the implementation detail of `LifxT` to `Lifx.Lan.Internal`.
- Add `Lifx.Lan.Mock.Terminal` module for testing programs without a physical LIFX device.
- Add `sendMessageAndWait` function.
- Use `Text` rather than `ByteString` for `label` field of `LightState`.
- Rename `productId` field of `Product` to `id`.
- Update to latest products list.

## 0.6.2 -- 2022-02-02
- Update to latest products list.

## 0.6.1 -- 2022-02-01
- Update to latest products list.

## 0.6 -- 2021-12-23
- Refactor to expose lower-level product lookup functionality via `Lifx.Internal.ProductInfoMap`.
- To facilitate the above, modify `LifxError` slightly, with a new `ProductLookupError` type.

## 0.5.1 -- 2021-12-12
- Implement `getProductInfo`.
- Expose `LifxT` constructor.

## 0.5.0 -- 2021-08-01
- Fix bug which was causing broadcasting not to work with messages that expect no response.

## 0.4.0 -- 2021-07-30
- Use abstract Device type.
- Use more types from standard libraries where appropriate:
    - `PortNumber`
    - `NominalDiffTime`
- General cleanup and documentation improvements.

## 0.3.0 -- 2021-06-19
- Implement message broadcasting and device discovery.

## 0.2.0 -- 2021-06-18
- Enable querying state (colour or power level).
    - Various breaking changes to enable this.

## 0.1.0.2
- Basic. Only supports setting power and colour.
