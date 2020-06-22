# Test app endpoints

# Setup ------------------------------------------------------------------------

library(OmicAnalyzer)
library(tinytest)

testStudyName <- "ABC"
testStudyObj <- OmicAnalyzer:::testStudy(name = testStudyName, version = "0.3")
testStudyObj <- addPlots(testStudyObj, OmicAnalyzer:::testPlots())
testModelName <- names(testStudyObj[["models"]])[1]
testTestName <- testStudyObj[["tests"]][[1]][1, "testID"]
testAnnotationName <- names(testStudyObj[["annotations"]])[1]
testTermName <- names(testStudyObj[["annotations"]][[testAnnotationName]][["terms"]])[1]

tmplib <- tempfile()
dir.create(tmplib)
libOrig <- .libPaths()
.libPaths(c(tmplib, libOrig))
suppressMessages(OmicAnalyzer::installStudy(testStudyObj))

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
  studies[[1]][["package"]][["OmicAnalyzerVersion"]],
  as.character(utils::packageVersion("OmicAnalyzer"))
)

expect_identical(
  vapply(studies[[1]][["results"]], function(x) x[["modelID"]], character(1)),
  names(getModels(testStudyObj))
)

expect_identical(
  vapply(studies[[1]][["results"]][[1]][["tests"]],
         function(x) x[["testID"]], character(1)),
  getTests(testStudyObj, modelID = testModelName)[, "testID"]
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

expect_error(
  getResultsTable(1),
  "No method for object of class \"numeric\""
)

expect_identical(
  resultsTable,
  getResultsTable(testStudyObj, testModelName, testTestName)
)

# getEnrichmentsTable ----------------------------------------------------------

enrichmentsTable <- getEnrichmentsTable(testStudyName, testModelName, testAnnotationName)

expect_identical(
  class(enrichmentsTable),
  "data.frame"
)

expect_true(all(getTests(testStudyName, testModelName)[, "testID"] %in% colnames(enrichmentsTable)))

expect_error(
  getEnrichmentsTable(1),
  "No method for object of class \"numeric\""
)

expect_identical(
  enrichmentsTable,
  getEnrichmentsTable(testStudyObj, testModelName, testAnnotationName)
)

# getEnrichmentsNetwork --------------------------------------------------------

enrichmentsNetwork <- getEnrichmentsNetwork(testStudyName, testModelName, testAnnotationName)

expect_identical(
  class(enrichmentsNetwork),
  "list"
)

expect_identical(
  names(enrichmentsNetwork),
  c("tests", "nodes", "links")
)

expect_identical(
  enrichmentsNetwork[["tests"]],
  getTests(testStudyName, testModelName)[, "testID"]
)

expect_error(
  getEnrichmentsNetwork(testStudyObj),
  "The Enrichment Network is only available for study packages or databases"
)

expect_error(
  getEnrichmentsNetwork(1),
  "No method for object of class \"numeric\""
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
  c("featureID", "featureDisplay", "statistic", "logFoldChange")
)

expect_equal(
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

# Teardown ---------------------------------------------------------------------

unlink(tmplib, recursive = TRUE, force = TRUE)
.libPaths(libOrig)
