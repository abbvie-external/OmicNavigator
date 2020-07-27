# Test app endpoints

# Setup ------------------------------------------------------------------------

source("tinytestSettings.R")
using(ttdo)

library(OmicAnalyzer)

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
  "missing"
)

expect_equal_with_diff(
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
  "missing"
)

expect_identical(
  enrichmentsTable,
  getEnrichmentsTable(testStudyObj, testModelName, testAnnotationName)
)

# getEnrichmentsNetwork --------------------------------------------------------

enrichmentsNetwork <- getEnrichmentsNetwork(
  testStudyName,
  testModelName,
  testAnnotationName
)

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
  "The Enrichment Network is only available for study packages"
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
  c(3L, 4L)
)

expect_warning(
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

# getNodeFeatures --------------------------------------------------------------

annotation <- getAnnotations(testStudyName, testAnnotationName)

expect_identical(
  getNodeFeatures(testStudyName, testAnnotationName, testTermName),
  sort(annotation[["terms"]][[testTermName]])
)

expect_error(
  getNodeFeatures(testStudyObj),
  "\"study\" must be the name of an installed study package"
)

expect_error(
  getNodeFeatures(testStudyName, testAnnotationName, "non-existent-term"),
  "non-existent-term"
)

expect_error(
  getNodeFeatures(testStudyName, "non-existent-annotation", testTermName),
  "non-existent-annotation"
)

# getLinkFeatures --------------------------------------------------------------

expect_identical(
  getLinkFeatures(testStudyName, testAnnotationName, testTermName, "term_03"),
  sort(intersect(annotation[["terms"]][[testTermName]],
                 annotation[["terms"]][["term_03"]]))
)

expect_error(
  getLinkFeatures(testStudyObj),
  "\"study\" must be the name of an installed study package"
)

# Teardown ---------------------------------------------------------------------

unlink(tmplib, recursive = TRUE, force = TRUE)
.libPaths(libOrig)
