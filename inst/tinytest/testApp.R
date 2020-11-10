# Test app endpoints

# Setup ------------------------------------------------------------------------

source("tinytestSettings.R")
using(ttdo)

library(OmicNavigator)

testStudyName <- "ABC"
testStudyObj <- OmicNavigator:::testStudy(name = testStudyName, version = "0.3")
testStudyObj <- addPlots(testStudyObj, OmicNavigator:::testPlots())
testModelName <- names(testStudyObj[["models"]])[1]
testTestName <- names(testStudyObj[["tests"]][[1]])[1]
testAnnotationName <- names(testStudyObj[["annotations"]])[1]
testTermName <- names(testStudyObj[["annotations"]][[testAnnotationName]][["terms"]])[1]

# Add a report file
tmpReport <- tempfile(fileext = ".html")
writeLines("<p>example</p>", tmpReport)
testStudyObj <- addReports(testStudyObj, list(model_02 = tmpReport))

tmplib <- tempfile()
dir.create(tmplib)
libOrig <- .libPaths()
.libPaths(c(tmplib, libOrig))
suppressMessages(OmicNavigator::installStudy(testStudyObj))

# listStudies ------------------------------------------------------------------

studies <- listStudies(libraries = tmplib)

expect_identical(
  length(studies),
  1L
)

expect_identical(
  studies[[1]][["name"]],
  testStudyName
)

expect_identical(
  names(studies[[1]]),
  c("name", "package", "results", "enrichments", "plots")
)

expect_identical(
  studies[[1]][["package"]][["OmicNavigatorVersion"]],
  as.character(utils::packageVersion("OmicNavigator"))
)

expect_identical(
  vapply(studies[[1]][["results"]], function(x) x[["modelID"]], character(1)),
  names(getModels(testStudyObj))
)

expect_identical(
  vapply(studies[[1]][["results"]][[1]][["tests"]],
         function(x) x[["testID"]], character(1)),
  names(getTests(testStudyObj, modelID = testModelName))
)

expect_identical(
  vapply(studies[[1]][["enrichments"]], function(x) x[["modelID"]], character(1)),
  names(getModels(testStudyObj))
)

expect_identical(
  vapply(studies[[1]][["enrichments"]][[1]][["annotations"]],
         function(x) x[["annotationID"]], character(1)),
  names(getEnrichments(testStudyObj, modelID = testModelName))
)

expect_identical(
  vapply(studies[[1]][["plots"]], function(x) x[["modelID"]], character(1)),
  names(getModels(testStudyObj))
)

expect_identical(
  vapply(studies[[1]][["plots"]][[1]][["plots"]],
         function(x) x[["plotID"]], character(1)),
  names(getPlots(testStudyObj, modelID = testModelName))
)

# If there are no OmicNavigator study packages installed, return an empty list.
expect_identical(
  listStudies(libraries = tempfile()),
  list()
)

# getResultsTable --------------------------------------------------------------

resultsTable <- getResultsTable(testStudyName, testModelName, testTestName)

expect_identical(
  class(resultsTable),
  "data.frame"
)

features <- getFeatures(testStudyObj, testModelName)
expect_true(all(colnames(features) %in% colnames(resultsTable)))

results <- getResults(testStudyObj, testModelName, testTestName)
expect_true(all(colnames(results) %in% colnames(resultsTable)))

expect_identical_xl(
  resultsTable[, 1],
  results[, 1]
)

expect_error(
  getResultsTable(1),
  "missing"
)

expect_equal_with_diff(
  resultsTable,
  getResultsTable(testStudyObj, testModelName, testTestName)
)

# getEnrichmentsTable ----------------------------------------------------------

enrichmentsTable <- getEnrichmentsTable(testStudyName, testModelName, testAnnotationName)

expect_identical_xl(
  class(enrichmentsTable),
  "data.frame"
)

expect_true(all(names(getTests(testStudyName, testModelName)) %in% colnames(enrichmentsTable)))

expect_error(
  getEnrichmentsTable(1),
  "missing"
)

expect_equal_xl(
  enrichmentsTable,
  getEnrichmentsTable(testStudyObj, testModelName, testAnnotationName)
)

# getEnrichmentsNetwork --------------------------------------------------------

enrichmentsNetwork <- getEnrichmentsNetwork(
  testStudyName,
  testModelName,
  testAnnotationName
)

expect_identical_xl(
  class(enrichmentsNetwork),
  "list"
)

expect_identical_xl(
  names(enrichmentsNetwork),
  c("tests", "nodes", "links")
)

expect_identical_xl(
  enrichmentsNetwork[["tests"]],
  names(getTests(testStudyName, testModelName))
)

expect_message(
  getEnrichmentsNetwork(testStudyObj, testModelName, testAnnotationName),
  "No overlaps available"
)

expect_error(
  getEnrichmentsNetwork(1),
  "missing"
)

# getMetaFeaturesTable ---------------------------------------------------------

metaFeaturesTable <- getMetaFeaturesTable(
  testStudyName,
  testModelName,
  "feature_0042"
)

expect_identical(
  class(metaFeaturesTable),
  "data.frame"
)

expect_identical(
  dim(metaFeaturesTable),
  c(3L, 5L)
)

# Confirm that even numeric-looking columns are returned as character
expect_identical(
  unique(vapply(metaFeaturesTable, class, FUN.VALUE = character(1), USE.NAMES = FALSE)),
  "character"
)

expect_message(
  getMetaFeaturesTable(
    testStudyName,
    testModelName,
    "non-existent"
  ),
  "No metaFeatures found for featureID \"non-existent\""
)

# getBarcodeData ---------------------------------------------------------------

barcodeData <- getBarcodeData(
  testStudyName,
  testModelName,
  testTestName,
  testAnnotationName,
  testTermName
)

expect_identical(
  names(barcodeData),
  c("data", "highest", "labelStat", "labelLow", "labelHigh")
)

expect_identical(
  colnames(barcodeData[["data"]]),
  c("featureID", "featureEnrichment", "featureDisplay", "statistic", "logFoldChange")
)

expect_identical(
  barcodeData[["data"]][["statistic"]],
  sort(barcodeData[["data"]][["statistic"]], decreasing = TRUE),
  info = "Barcode results should be ordered by statistic column"
)

expect_equal_with_diff(
  barcodeData[["highest"]],
  ceiling(max(abs(barcodeData[["data"]][, "statistic"])))
)

barcodeData <- getBarcodeData(
  testStudyName,
  "model_03",
  testTestName,
  testAnnotationName,
  testTermName
)

expect_identical(
  barcodeData[["labelStat"]],
  "Effect size",
  info = "Confirm model-specific barcode data returned"
)

# getReportLink ----------------------------------------------------------------

expect_identical(
  getReportLink(testStudyName, testModelName),
  getReports(testStudyObj, modelID = testModelName)
)

expect_identical_xl(
  getReportLink(testStudyName, "model_02"),
  sprintf("%s%s/OmicNavigatorReports/model_02/report.html",
          OmicNavigator:::getPrefix(), testStudyName)
)

# getNodeFeatures --------------------------------------------------------------

annotation <- getAnnotations(testStudyName, testAnnotationName)

expect_identical(
  getNodeFeatures(testStudyName, testAnnotationName, testTermName),
  sort(annotation[["terms"]][[testTermName]])
)

expect_identical(
  getNodeFeatures(testStudyObj, testAnnotationName, testTermName),
  sort(annotation[["terms"]][[testTermName]])
)

expect_message(
  getNodeFeatures(testStudyName, testAnnotationName, "non-existent-term"),
  "non-existent-term"
)

expect_message(
  getNodeFeatures(testStudyName, "non-existent-annotation", testTermName),
  "non-existent-annotation"
)

# getLinkFeatures --------------------------------------------------------------

expect_identical(
  getLinkFeatures(testStudyName, testAnnotationName, testTermName, "term_03"),
  sort(intersect(annotation[["terms"]][[testTermName]],
                 annotation[["terms"]][["term_03"]]))
)

expect_identical(
  getLinkFeatures(testStudyObj, testAnnotationName, testTermName, "term_03"),
  sort(intersect(annotation[["terms"]][[testTermName]],
                 annotation[["terms"]][["term_03"]]))
)

# getPackageVersion ------------------------------------------------------------

expect_identical_xl(
  class(getPackageVersion()),
  "character"
)

# Teardown ---------------------------------------------------------------------

unlink(tmplib, recursive = TRUE, force = TRUE)
.libPaths(libOrig)
