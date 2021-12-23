# Revision history for lifx-lan

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
