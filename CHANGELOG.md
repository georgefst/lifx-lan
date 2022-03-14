# Revision history for lifx-lan

## 0.7 -- 2022-02-20
- Drop support for GHC < 9.2.
    - If anyone is stuck on an older version of GHC and needs recent features of `lifx-lan` then please let me know. It would be reasonably easy to create a branch for it.
- Don't provide field selector functions for any types. Using `OverloadedRecordDot` in client code is recommended. We still export `unLifxT` as a normal function, for backward compatibility.
- Move much of the implementation detail of `LifxT` has been moved to `Lifx.Lan.Internal`.
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
    - PortNumber
    - NominalDiffTime
- General cleanup and documentation improvements.

## 0.3.0 -- 2021-06-19
- Implement message broadcasting and device discovery.

## 0.2.0 -- 2021-06-18
- Enable querying state (colour or power level).
    - Various breaking changes to enable this.

## 0.1.0.2
- Basic. Only supports setting power and colour.
