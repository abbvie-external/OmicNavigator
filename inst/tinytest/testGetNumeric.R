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

expect_true_xl(
  is.character(
    getFeatures(testStudyName, modelID = testModelName)[, 1]
  )
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

expect_true_xl(
  is.character(
    row.names(getAssays(testStudyName, modelID = testModelName))
  )
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

expect_equal_xl(
  getResults(testStudyName),
  testStudyObj[["results"]]
)

expect_equal_xl(
  getResults(testStudyName, modelID = testModelName, testID = testTestName),
  testStudyObj[["results"]][[testModelName]][[testTestName]]
)

expect_true_xl(
  is.character(
    getResults(testStudyName, modelID = testModelName, testID = testTestName)[, 1]
  )
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

expect_true_xl(
  is.character(
    getMetaFeatures(testStudyName, modelID = testModelName)[, 1]
  )
)

expect_true_xl(
  is.character(
    getMetaFeatures(testStudyName, modelID = testModelName)[, 2]
  )
)

# getResultsTable --------------------------------------------------------------

resultsTableFromR <- getResultsTable(testStudyObj, testModelName, testTestName)
resultsTableFromFile <- getResultsTable(testStudyName, testModelName, testTestName)

expect_equal_xl(
  resultsTableFromFile,
  resultsTableFromR
)

expect_true_xl(
  is.character(
    getResultsTable(testStudyName, modelID = testModelName, testID = testTestName)[, 1]
  )
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

# The test below is more trouble than it's worth. Due to rounding of floating-
# point numbers when the "statistic" column is written to file and read back
# into R, the sorting can be slightly different. I think this is because the
# numbers are slightly different, but after the I/O, they are considered a tie.
# The most frustrating thing is that the behavior differs between versions of R.
# The code as is works fine for R 4.0.3 (Windows or WSL Ubuntu 18.04) and also R
# 3.6.3 (Docker Ubuntu 20.04). However, it fails for R 3.4.4 (Jenkins Ubuntu
# 18.04 or Docker 18.04). When I tried adding the "featureID" column to be a tie
# breaker, it fixed it for R 3.4.4 but then broke it for everything else. I
# looked through the R NEWS file for any relevant changes in behavior, but I
# didn't see anything. I spent too much time on this. The only difference is
# that the ordering of ties can vary after writing and reading to file due to
# floating point arithmetic. For all intents and purposes, these are essentially
# ties. I also tried using signif() to limit the decimal places, which fixed it,
# but then broke the test that checks that the table is properly sorted. So I'm
# just going to test on more recent versions of R and move on.
if (getRversion() >= "3.6.3") {
  expect_equal_xl(
    barcodeDataFromFile,
    barcodeDataFromR
  )
}

expect_true_xl(
  is.character(
    barcodeDataFromFile[["data"]][["featureID"]]
  )
)

expect_true_xl(
  is.character(
    barcodeDataFromFile[["data"]][["featureEnrichment"]]
  )
)

expect_true_xl(
  is.character(
    barcodeDataFromFile[["data"]][["featureDisplay"]]
  )
)

# Teardown ---------------------------------------------------------------------

unlink(tmplib, recursive = TRUE, force = TRUE)
.libPaths(libOrig)
