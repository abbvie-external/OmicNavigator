# Test getX() methods with a study that has numeric-looking featureIDs

# Setup ------------------------------------------------------------------------

source("tinytestSettings.R")
using(ttdo)

library(OmicNavigator)

testStudyObj <- OmicNavigator:::testStudyNumeric()
testStudyName <- testStudyObj[["name"]]
testModelName <- names(testStudyObj[["models"]])[1]
testTestName <- names(testStudyObj[["tests"]][[1]])[1]
testAnnotationName <- names(testStudyObj[["annotations"]])[1]
testTermName <- names(testStudyObj[["annotations"]][[testAnnotationName]][["terms"]])[1]

tmplib <- tempfile()
dir.create(tmplib)
libOrig <- .libPaths()
.libPaths(c(tmplib, libOrig))
suppressMessages(OmicNavigator::installStudy(testStudyObj))

# getFeatures ------------------------------------------------------------------

expect_identical_xl(
  getFeatures(testStudyObj),
  testStudyObj[["features"]]
)

expect_identical_xl(
  getFeatures(testStudyName),
  testStudyObj[["features"]]
)

# getAssays --------------------------------------------------------------------

expect_identical_xl(
  getAssays(testStudyObj),
  testStudyObj[["assays"]]
)

expect_equal_xl(
  getAssays(testStudyName),
  testStudyObj[["assays"]]
)

# getAnnotations ---------------------------------------------------------------

expect_identical_xl(
  getAnnotations(testStudyObj),
  testStudyObj[["annotations"]]
)

expect_identical_xl(
  getAnnotations(testStudyName),
  testStudyObj[["annotations"]]
)

# getResults -------------------------------------------------------------------

expect_identical_xl(
  getResults(testStudyObj),
  testStudyObj[["results"]]
)

expect_identical_xl(
  getResults(testStudyObj, modelID = testModelName, testID = testTestName),
  testStudyObj[["results"]][[testModelName]][[testTestName]]
)


expect_identical_xl(
  getResults(testStudyName),
  testStudyObj[["results"]]
)

expect_identical_xl(
  getResults(testStudyName, modelID = testModelName, testID = testTestName),
  testStudyObj[["results"]][[testModelName]][[testTestName]]
)

# getMetaFeatures --------------------------------------------------------------

expect_identical_xl(
  getMetaFeatures(testStudyObj),
  testStudyObj[["metaFeatures"]]
)

expect_identical_xl(
  getMetaFeatures(testStudyObj, modelID = testModelName),
  testStudyObj[["metaFeatures"]][["default"]]
)

expect_identical_xl(
  getMetaFeatures(testStudyName),
  testStudyObj[["metaFeatures"]]
)

expect_identical_xl(
  getMetaFeatures(testStudyName, modelID = testModelName),
  testStudyObj[["metaFeatures"]][["default"]]
)

# getResultsTable --------------------------------------------------------------

resultsTableFromR <- getResultsTable(testStudyObj, testModelName, testTestName)
resultsTableFromFile <- getResultsTable(testStudyName, testModelName, testTestName)

expect_identical_xl(
  resultsTableFromFile,
  resultsTableFromR
)

# getBarcodeData ---------------------------------------------------------------

barcodeDataFromR <- getBarcodeData(
  testStudyObj,
  testModelName,
  testTestName,
  testAnnotationName,
  testTermName
)

barcodeDataFromFile <- getBarcodeData(
  testStudyName,
  testModelName,
  testTestName,
  testAnnotationName,
  testTermName
)

expect_identical_xl(
  barcodeDataFromFile,
  barcodeDataFromR
)

# Teardown ---------------------------------------------------------------------

unlink(tmplib, recursive = TRUE, force = TRUE)
.libPaths(libOrig)
