# 1.9.1

* The release tarball includes version 1.4.1 of the web app

* **Breaking change:** The function `plotStudy()` now invisibly returns the
result from the custom plotting function. Previously it invisibly returned the
study object. It's unlikely you relied on this behavior. For a ggplot2 plot, the
return value will be the plotting object with class `"ggplot"`.

# 1.8.0

* Support for new plotType multiModel. This will enable visualizations between
models of a given study. See the User's Guide for instructions. Note that
currently this is only supported when calling `plotStudy()` in the R console.
Support for multiModel plots in the app will be implemented in a future release.
For now, the app will send a harmless error if it encounters a multiModel plot
(implemented by [Marco Curado](https://github.com/curadomr) in [PR
#8](https://github.com/abbvie-external/OmicNavigator/pull/8))

# 1.7.1

* The release tarball includes version 1.4.0 of the web app

# 1.7.0

* New study element `mapping` for mapping featureIDs across models of a study
(see `?addMapping` and `getMapping`). This will make it possible to create plots
that combine data across models, which will be enabled in future versions
(implemented by [Marco Curado](https://github.com/curadomr) in [PR
#7](https://github.com/abbvie-external/OmicNavigator/pull/7))

* Bug fix: The validation of the enrichments table linkouts was too strict. It
required that the annotationID be present in the annotations created by
`addAnnotations()`. This extra information about the annotationID is only
required for the network view. The validation now only throws an error if the
annotationID was not included for at least one modelID in the enrichments data
added with `addEnrichments()` (reported by [Anastasia
Galperina](https://github.com/agalperina))

* Bug fix: Now that custom plots can include data from the results table, it is
no longer an error to include custom plots in a study without assays or samples
data (reported by Joe LoGrasso). Also relaxed the validation requirement for
custom plots so that the study can contain assays data but not corresponding
samples data

* Bug fix: Custom plotting functions cannot have the same name as any of the
functions defined in the base package. This error is now caught early with
`addPlots()` (reported by [Anastasia Galperina](https://github.com/agalperina))

# 1.6.3

* The release tarball includes version 1.3.4 of the web app

# 1.6.2

* The release tarball includes version 1.3.3 of the web app

# 1.6.1

* The release tarball includes version 1.3.2 of the web app

* New `plotType` of `"multiTest"`. Include this when adding a custom plot with
`addPlots()` to have the app pass the results tables for all testIDs for a given
modelID (implemented by [Marco Curado](https://github.com/curadomr) in [PR
#6](https://github.com/abbvie-external/OmicNavigator/pull/6))

* Bug fix: The documentation states that the assays table must only contain
numeric columns. However, if a user added a data frame with non-numeric columns,
no warning or error occurred. Now `addAssays()` will throw an error if it
detects any non-numeric columns (reported by [Anastasia
Galperina](https://github.com/agalperina) in [WebApp Issue
#57](https://github.com/abbvie-external/OmicNavigatorWebApp/issues/57))

# 1.5.2

* The release tarball includes version 1.3.1 of the web app

# 1.5.1

* The release tarball includes version 1.2.1 of the web app

* Both `plotStudy()` and `getPlottingData()` gained a new argument `testID`. If
supplied, the R object passed to your custom plotting function will include the
element `results` that contains the rows of the results table for that testID
for the provided featureID(s). The app always passes the testID when calling
`plotStudy()`. If your plotting function doesn't need the information in the
results table, it will continue to work as before (implemented by [Marco
Curado](https://github.com/curadomr) in [PR
#4](https://github.com/abbvie-external/OmicNavigator/pull/4))

# 1.4.3

* Bug fix: Prevent a single missing value in an ID column that is required to be
unique (reported by Marco Rocha Curado)

* Report the error message from `R CMD build` as a warning if `exportStudy()`
fails to create a tarball for a study package

* Add the argument `reset` to `addOverlaps()`. You would only potentially need
this if you are pre-calculating the annotation term overlaps prior to exporting
the study

* Skip tests that fail only on CRAN Linux and macOS machines. The tests will
continue to run on GitHub Actions

# 1.4.2

* Prepare for CRAN submission

# 1.4.1

* The release tarball includes version 1.1.9 of the web app

* Optional support for model metadata via `addModels()` and `getModels()`.
Previously you could only add a single string to describe each modelID, which
would be displayed in the app as a tooltip. Now you also have the option to
provide a list with arbitrary metadata. The tooltip is derived from the list
element `description` (if it exists)

# 1.3.1

* New function `importStudy()` to import an installed study package into R as an
`onStudy` object. This is intended to be used to edit a study when you don't
have access to the original data and code that created it

* Optional support for test metadata via `addTests()` and `getTests()`.
Previously you could only add a single string to describe each testID, which
would be displayed in the app as a tooltip. Now you also have the option to
provide a list with arbitrary metadata. The tooltip is derived from the list
element `description` (if it exists)

# 1.2.1

* The release tarball includes version 1.1.8 of the web app

* New function `removeStudy()` to remove an installed study package

* New argument `studyMeta` for `createStudy()`. You can add metadata to
describe your study. See `?createStudy` and the User's Guide for more details.

* New arguments `maintainer` and `maintainerEmail` for `createStudy()` to make
it easier for others to know who to contact for questions about your study

* `listStudies()` now returns all the fields in `DESCRIPTION` for each study
package

* Fixed minor bug in `listStudies()`. It couldn't find study packages in
directories not included in `.libPaths()`. This likely only affected the unit
tests.

# 1.1.7

* The release tarball includes version 1.1.7 of the web app

* Added support for linkouts in metaFeatures table. See
`?addMetaFeaturesLinkouts` for details. These linkouts will be displayed as
favicons in the metaFeatures table in a future release of the app.

* The "add" functions have a new argument, `reset`. If `reset = TRUE`, then the
existing data is removed prior to adding the new data. The default continues to
be to add to or modify any existing data.

# 1.1.6

* The release tarball includes version 1.1.6 of the web app

[dt-aware]: https://rdatatable.gitlab.io/data.table/articles/datatable-importing.html#data-table-in-imports-but-nothing-imported-1

* Package functions are now "[data.table aware][dt-aware]". This is an internal
change only. The output is unaffected, and functions are potentially faster.

# 1.1.5

* The release tarball includes version 1.1.5 of the web app

* Fix bug in `getResultsIntersection()` where adding additional filters resulted
in more results when `notTests` were specified, instead of fewer (reported by
Terry Ernst, Brett Engelmann, and Wendy Waegell)

# 1.1.4

* The release tarball includes version 1.1.3 of the web app

# 1.1.3

* The release tarball includes version 1.1.2 of the web app

* Bug fix: `exportStudy()` can now export a study as a package tarball to a path
that contains spaces or quotes (reported by Naim Mahi and Brett Engelmann)

* Bug fix: Study packages will now use custom description if supplied to
`createStudy()`. Hover over the study name in the app to see the custom package
description (reported by Paul Nordlund and Brett Engelmann)

# 1.1.2

* The release tarball includes version 1.1.1 of the web app

* Bug fix: `installApp()` now properly searches for the installed OmicNavigator
package. It usually worked before, but now it should always work.

# 1.1.1

* The release tarball includes version 1.1.0 of the web app

# 1.1.0

* New function `installApp()` to download the web app after installing the R
package

* Support for tibble and data.table input. For consistency, they will be be
automatically converted to pure data frame objects internally.

* Bug fix: `exportStudy()` now properly overwrites an existing tarball when the
argument `path` is used (reported by Brett Engelmann)

* Bug fix: `getResultsIntersection()` now properly returns the filtered table
in the original order from `getResultsTable()` (reported by Paul Nordlund)

# 1.0.0

* The release tarball includes version 1.0.0 of the web app
* UpSet plots now display a maximum of 30 intersections (previously was 40)
* UpSet plots now no longer display empty intersections

# 0.24.1

* The release tarball includes version 0.3.8 of the web app
* Report versions of R package and app when first attached
* Send warning if the version of the app is not the version pinned to the
installed version of the R package

# 0.24.0

* **Breaking change:** `plotStudy()` and `getPlottingData()` now accept multiple
featureIDs as input in order to visualize multiple feature in a single plot. The
object returned by `getPlottingData()` is now a list of three data frames:
`assays`, `samples`, and `features`. Thus you can annotate your plots with any
feature metadata. You will need to update your custom plotting functions to
accept one argument (you can name it whatever you like). Also, when adding plots
with `addPlots()`, specify the `plotType` as `"singleFeature"` or
`"multipleFeature"` to accept a single or multiple featureIDs, respectively. The
app currently only supports singleFeature custom plots, but will soon support
multiFeature plots as well. **Study packages must be rebuilt with OmicNavigator
0.24.0.**

* Fix bug in `getResultsUpset()` that failed when using the absolute value
filters (`|<|`, `|>|`)

* `getResultsUpset()` now throws an error when there are no features remaining
for plotting after the filters are applied

# 0.23.1

* The release tarball includes version 0.3.7 of the web app

# 0.23.0

* The release tarball includes version 0.3.6 of the web app

* Use [faviconPlease][] to find URLs to favicons for table linkouts

[faviconPlease]: https://cran.r-project.org/package=faviconPlease

* Optimize `getResultsUpset()` for speed. Input remains unchanged. The plotting
data is now returned invisibly. The default font size for the UpSet plot was
increased.

* Apply MIT license

* Fix bug in `getResultsIntersection()` when a modelID has no features table.
The "Set_Membership" column was returned in the first column instead of after
the featureID column (reported by Joe Dalen and Paul Nordlund)

* Fix bug in `getResultsUpset()` when the results tables have differing
number of features

# 0.22.5

* The release tarball includes version 0.3.5.3 of the web app

# 0.22.4

* The release tarball includes version 0.3.5.2 of the web app

# 0.22.3

* The release tarball includes version 0.3.5.1 of the web app

# 0.22.2

* The release tarball includes version 0.3.5 of the web app

# 0.22.1

* The release tarball includes version 0.3.4 of the web app

# 0.22.0

* New function `getFavicons()` to obtain URLs to favicons to display table
linkouts. Currently just a wrapper around Google's favicon service, but in the
future will be more sophisticated.

# 0.21.8

* The release tarball includes version 0.3.3 of the web app

* Improved support for numeric-looking featureIDs, e.g. `1234` or `0001`

# 0.21.7

* The release tarball includes version 0.3.2.1 of the web app

# 0.21.6

* The release tarball includes version 0.3.2 of the web app

# 0.21.5

* The release tarball includes version 0.3.1 of the web app

* User's Guide:
    * Document minimal valid study
    * New section: Mapping between data elements and app features
    * Add linkouts to results and enrichments tables

* Moved UpSetR to a suggested dependency. This allows for a light-weight
installation when you only need to create a study and not run the app. As an
example, a continuous integration job that builds a study package tarball.

* Relax some of the validation requirements. Not all the featureIDs in the
results tables must be in the features or metaFeatures tables.

# 0.21.4

* If a study has no features table for a modelID, a study can still have
linkouts to external resources in the results table, as long as the linkouts
only refer to the featureID column.

* `validateStudy()` now throws an error if the results and features table for a
given modelID share any column names in addition to the featureID column. In
other words, it validates that the only column name in common between the two
tables is the first column.

# 0.21.3

* The release tarball includes version 0.2.8 of the web app

* `validateStudy()` now throws an error if there are any featureIDs in the
metaFeatures table that are not included in the features table (reported by Paul
Nordlund)

# 0.21.2

* The release tarball includes version 0.2.7 of the web app

# 0.21.1

* The release tarball includes version 0.2.6.1 of the web app

# 0.21.0

* Support for linkouts to external resources in results and enrichments tables

* New function `getPackageVersion()` for the app to conveniently query the
current installed version of the R package

# 0.20.1

* The release tarball includes version 0.2.5 of the web app

* `validateStudy()` now throws a warning if the results tables for the tests of
a model have no common columns. The lack of common columns will disable the
UpSet filtering in the app for that model.

# 0.20.0

## Breaking change

* `exportStudy()` - The argument `type` has changed. Studies can no longer be
exported as RDS (`type = "rds")` or text files (`type = "text"`). This is
because these were no longer able to capture all of the information that can
potentially be included in a study package (e.g. custom plotting functions,
report files). Instead, the default is now the new option `"tarball"`. This will
export the study package as a source package tarball, which is ready to be
installed directly with `install.packages()`. The option `"package"` remains; it
exports the study as a package directory.

## New feature

* The "get" functions have a new argument `quiet`. Setting `quiet = TRUE` will
suppress any messages, such as reporting unavailable data or the use of the
"default" data.

## Miscellaneous

* Improved the messages from `installStudy()` and `exportStudy()`

* `validateStudy()` now performs more in-depth check to validate that the
featureIDs are consistent between the results, features, and assays tables

# 0.19.0

* Most package functions now consistently return empty data structures (e.g.
`list()`) when a query requests data that is unavailable. Previously the
functions would throw an error if the requested data was unavailable. There is
a new section in the API vignette documenting this behavior for the functions
called by the app.

* It is now possible to export a study that only contains a single results
table.

# 0.18.2

* The release tarball includes version 0.2.4.1 of the web app

# 0.18.1

* The release tarball includes version 0.2.4 of the web app

# 0.18.0

## Breaking change

* Changed name of app and R package from OmicAnalyzer to OmicNavigator. This
breaks all previously installed study packages. Re-run your scripts to
re-install the study packages. Also note that the default study package prefix
has changed from "OAstudy" to "ONstudy".

## Other changes

* The release tarball includes version 0.2.3 of the web app

# 0.17.0

## Breaking change

* The input to `addTests()` is now a named list, similar to `addModels()`. You
will need to update your code and re-install any existing OmicNavigator study
packages.

## Other updates

* New package option `OmicNavigator.prefix` to control the prefix used to label
OmicNavigator study packages. The default is still "OAstudy". Run `?OmicNavigator`
for more information on OmicNavigator package options.

# 0.16.2

* The release tarball includes version 0.2.2 of the web app

# 0.16.1

* The release tarball includes version 0.2.1 of the web app

# 0.16.0

* New User's Guide and API vignettes created with Sweave

* Add `summary()` method for OmicNavigator study objects (class `onStudy`)

* Bugfix: `listStudies()` now works if its argument `libraries` is pointed to a
specific directory

# 0.15.0

* The release tarball includes version 0.1.9.1 of the web app

* Remove the argument `overwrite` for the add functions, e.g. `addSamples()`.
Instead subsequent calls to the same add function will update the existing data
using the rules of `utils::modifyList()`.

* `addMetaFeatures()` now automatically converts all columns to character. It
throws a warning if it detects any non-character columns.

* `getMetaFeatures()` and `getMetaFeaturesTable()` always return the columns of
the features table as character strings, even if they appear numeric.

* Support the option to manually calculate pairwise overlaps with
`addOverlaps()` prior to installing or exporting the study package.

# 0.14.1

* The release tarball includes version 0.1.8 of the web app

# 0.14.0

* Support for linking to external analysis report files (`addReports()`,
`getReports()`, `getReportLink()`). Accepts a URL or a path to file.

* New function `validateStudy()` to validate a study. It is automatically run
prior to exporting with `exportStudy()` (controlled by the new argument
`requireValid`). It can also be run directly.

* `createStudy()` now checks that the arguments `name`, `description`, and
`version` are valid.

* `exportStudy()` no longer exports empty directories for unused elements of the
OmicNavigator study object.

* Bug fix: `getMetaFeaturesTable()` now properly returns a data frame even if
it only contains one column.

# 0.13.3

* Bug fix: Preserve input order for results table. Due to the merge with the
features table, `getResultsTable()` was changing the original row order. Bug
identified by Brett Engelmann.

# 0.13.2

* The release tarball includes version 0.1.7 of the web app

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
OmicNavigator studies need to be reinstalled in order to be compatible with this
version. Furthermore, the dependencies have changed, so you may have to re-run
`remotes::install_deps()`.

# 0.12.1

* The release tarball includes version 0.1.6 of the web app

# 0.12.0

* The release tarball includes version 0.1.5 of the web app

* `getEnrichmentsUpset()` has a new argument `tests`, which restricts the UpSet
plot to only include the desired tests (#1, Justin Moore)

# 0.11.0

* The release tarball includes version 0.1.4 of the web app

* If `startApp()` is unable to run because the package was installed without the
bundled web app, provide the URL for the releases page to download the tarball.
Also offer to open the page in the browser.

# 0.10.0

* More documentation of function arguments
* New function `getMetaFeatures()`
* New function `getMetaFeaturesTable()`

# 0.9.3

* The release tarball includes version 0.1.3 of the web app

# 0.9.2

* Fix bug in `getResultsIntersection()` introduced in commit ff1ac89 and
released in version 0.7.0

* The release tarball includes version 0.1.2 of the web app

# 0.9.1

* The release tarball includes version 0.1.1 of the web app

# 0.9.0

* The release tarball includes version 0.1.0 of the web app. Run
`startApp()` to run it locally.

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
to other tests (only affected study packages, not onStudy objects)

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
