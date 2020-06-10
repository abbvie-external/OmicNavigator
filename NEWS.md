# 0.5.1

* Fix bug in `getPlots()` so that it returns model-specific custom plots from
the database of installed study package

# 0.5.0

* Changed `<=` and `>=` to `<` and `>`, respectively, to match app UI. Affects
the functions `getResultsIntersection()`, `getEnrichmentsIntersection()`,
`getResultsUpset()`, and `getEnrichmentsUpset()`.

* Support customization of barcode plots:
    * `addBarcodes()`
    * `getBarcodes()`
    * `getBarcodeData()`

* `getAnnotations()`

* Changed the input argument of `plotStudy()` from `feature` to `featureID`

* Changed the argument name of custom plotting functions from `feature` to
`featureID`

# 0.4.0

* `getResultsIntersection()`
* `getEnrichmentsIntersection()`
* `getResultsUpset()`
* `getEnrichmentsUpset()`
