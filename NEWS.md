# 0.6.0.9000

* New function `getPlottingData()` to return the data required by the first
argument of the custom plotting functions.

* `getResultsIntersection()` positions the column `Set_Membership` between the
feature metadata variable columns and the results columns

* Study packages that includes plots using base graphics now import the graphics
package

* Fix bug when querying model-specific barcode metadata. Affected both
`getBarcodes()` and `getBarcodeData()` (reported by Paul Nordlund)

# 0.6.0

* New function `startApp()` to start the app running on local machine

* Fix bug in `getResultsIntersection()` so that it returns the feature metadata
columns that are included in `getResultsTable()`

* Fix bug in `getResults()` that returned columns of `NA` for columns specific
to other tests (only affected study packages, not oaStudy objects)

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
