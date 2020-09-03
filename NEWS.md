# 0.14.1

* The release tarball includes [version 0.1.8 of the web app][app-v0.1.8]

[app-v0.1.8]: https://***REMOVED***/IProt/OmicAnalyzer/releases/tag/v0.1.8

# 0.14.0

* Support for linking to external analysis report files (`addReports()`,
`getReports()`, `getReportLink()`). Accepts a URL or a path to file.

* New function `validateStudy()` to validate a study. It is automatically run
prior to exporting with `exportStudy()` (controlled by the new argument
`requireValid`). It can also be run directly.

* `createStudy()` now checks that the arguments `name`, `description`, and
`version` are valid.

* `exportStudy()` no longer exports empty directories for unused elements of the
OmicAnalyzer study object.

* Bug fix: `getMetaFeaturesTable()` now properly returns a data frame even if
it only contains one column.

# 0.13.3

* Bug fix: Preserve input order for results table. Due to the merge with the
features table, `getResultsTable()` was changing the original row order. Bug
identified by Brett Engelmann.

# 0.13.2

* The release tarball includes [version 0.1.7 of the web app][app-v0.1.7]

[app-v0.1.7]: https://***REMOVED***/IProt/OmicAnalyzer/releases/tag/v0.1.7

# 0.13.1

* `addFeatures()` now automatically converts all columns to character. It throws
a warning if it detects any non-character columns.

* `getFeatures()` and `getResultsTable()` always return the columns of the
features table as character strings, even if they appear numeric.

# 0.13.0

This release includes substantial internal changes. The study data is now
exported as plaintext files instead of as an SQLite file. There are no changes
to the API, so no changes need to be made to R scripts or the JavaScript
frontend. However, since the storage mechanism has changed, all existing
OmicAnalyzer studies need to be reinstalled in order to be compatible with this
version. Furthermore, the dependencies have changed, so you may have to re-run
`remotes::install_deps()`.

# 0.12.1

* The release tarball includes [version 0.1.6 of the web app][app-v0.1.6]

[app-v0.1.6]: https://***REMOVED***/IProt/OmicAnalyzer/releases/tag/v0.1.6

# 0.12.0

* The release tarball includes [version 0.1.5 of the web app][app-v0.1.5]

[app-v0.1.5]: https://***REMOVED***/IProt/OmicAnalyzer/releases/tag/v0.1.5

* `getEnrichmentsUpset()` has a new argument `tests`, which restricts the UpSet
plot to only include the desired tests (#1, @MOOREJX3)

# 0.11.0

* The release tarball includes [version 0.1.4 of the web app][app-v0.1.4]

[app-v0.1.4]: https://***REMOVED***/IProt/OmicAnalyzer/releases/tag/v0.1.4

* If `startApp()` is unable to run because the package was installed without the
bundled web app, provide the URL for the releases page to download the tarball.
Also offer to open the page in the browser.

# 0.10.0

* More documentation of function arguments
* New function `getMetaFeatures()`
* New function `getMetaFeaturesTable()`

# 0.9.3

* The release tarball includes [version 0.1.3 of the web app][app-v0.1.3]

[app-v0.1.3]: https://***REMOVED***/IProt/OmicAnalyzer/releases/tag/v0.1.3

# 0.9.2

* Fix bug in `getResultsIntersection()` introduced in commit [ff1ac89][] and
released in version [0.7.0][pkg-v0.7.0]

[ff1ac89]: https://***REMOVED***/***REMOVED***/OmicAnalyzer/commit/ff1ac89226b245072effbc5e8a7074c005360a4f
[pkg-v0.7.0]: https://***REMOVED***/***REMOVED***/OmicAnalyzer/releases/tag/v0.7.0

* The release tarball includes [version 0.1.2 of the web app][app-v0.1.2]

[app-v0.1.2]: https://***REMOVED***/IProt/OmicAnalyzer/releases/tag/v0.1.2

# 0.9.1

* The release tarball includes [version 0.1.1 of the web app][app-v0.1.1]

[app-v0.1.1]: https://***REMOVED***/IProt/OmicAnalyzer/releases/tag/v0.1.1

# 0.9.0

* The release tarball includes [version 0.1.0 of the web app][app-v0.1.0]. Run
`startApp()` to run it locally.

[app-v0.1.0]: https://***REMOVED***/IProt/OmicAnalyzer/releases/tag/v0.1.0

* New function `getUpsetCols()` to get the columns common across all tests of a
model, and thus are available for filtering with UpSet.

* `getBarcodeData()` now returns `data` in decreasing order of the `statistic`
column (requested by Paul Nordlund)

* `addBarcodes()` now has an additional optional field `featureDisplay`. This
can be set to any column of the features table, and that feature metadata
variable will be used to label the barcode plot (requested by Paul Nordlund). If
`featureDisplay` is not set, it will be automatically set to the feature
metadata variable used in the enrichment analysis. `getBarcodeData()` now
returns three feature-related columns:
    * `featureID` - The unique feature variable used in the inference results
    table
    * `featureEnrichment` - The feature variable used to perform the enrichment
    analysis with the given annotation database
    * `featureDisplay` - The feature variable to use to label the barcode plot
    on hover

   Note that these will all be identical in the simple case where the study
   `featureID` is used for the enrichment analysis.

# 0.8.0

* `getBarcodeData()` now returns both the study featureID (in the column
`featureID`) and the featureID used by the annotation database (in the column
`featureDisplay`). The former is needed to pass to `plotStudy()` to generate any
custom plots. The latter is needed to display when hovering over the barcode
plot (reported by Paul Nordlund).

* Fix bug in `plotStudy()` related to detaching package namespaces. When a
custom plot specified more than one package dependency, the last package listed
would always be detached from the search path (even if it shouldn't have been
since it was already attached). Furthermore, the other packages would never be
detached (even if they should have been since they were not attached
beforehand). The bug was caused by calling `detach()` with `on.exit()`, which
used the latest value of the variable.

# 0.7.0

* New function `getPlottingData()` to return the data required by the first
argument of the custom plotting functions.

* `getResultsIntersection()` positions the column `Set_Membership` between the
feature metadata variable columns and the results columns

* Study packages that includes plots using base graphics now import the graphics
package

* Fix bug when querying model-specific barcode metadata. Affected both
`getBarcodes()` and `getBarcodeData()` (reported by Paul Nordlund)

* Remove warning from `graphics::par()` by only having `plotStudy()` reset the
graphing parameters if they are changed by the custom plotting function

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
