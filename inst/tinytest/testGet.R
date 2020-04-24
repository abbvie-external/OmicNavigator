# Test getX() methods

# Setup ------------------------------------------------------------------------

library(OmicAnalyzer)
library(tinytest)

testStudyName <- "ABC"
testStudyObj <- OmicAnalyzer:::testStudy(name = testStudyName)
testModelName <- names(testStudyObj[["models"]])[1]
testTestName <- names(testStudyObj[["tests"]])[1]

tmplib <- tempfile()
dir.create(tmplib)
libOrig <- .libPaths()
.libPaths(c(tmplib, libOrig))
OmicAnalyzer::installStudy(testStudyObj)

# installedStudies -------------------------------------------------------------

installedStudies <- getInstalledStudies(libraries = tmplib)
expect_identical(
  installedStudies,
  testStudyName
)

# getModels --------------------------------------------------------------------

expect_identical(
  getModels(testStudyObj),
  testStudyObj[["models"]]
)

expect_identical(
  getModels(testStudyObj, model = testModelName),
  testStudyObj[["models"]][testModelName]
)

expect_error(
  getModels(testStudyObj, model = "non-existent-model"),
  "non-existent-model"
)

expect_identical(
  getModels(testStudyName),
  testStudyObj[["models"]]
)

expect_identical(
  getModels(testStudyName, model = testModelName),
  testStudyObj[["models"]][testModelName]
)

expect_error(
  getModels(testStudyName, model = "non-existent-model"),
  "non-existent-model"
)


# getTests ---------------------------------------------------------------------

expect_identical(
  getTests(testStudyObj),
  testStudyObj[["tests"]]
)

expect_identical(
  getTests(testStudyObj, testID = testTestName),
  testStudyObj[["tests"]][testTestName]
)

expect_error(
  getTests(testStudyObj, testID = "non-existent-test"),
  "non-existent-test"
)

expect_identical(
  getTests(testStudyObj, modelID = testModelName),
  testStudyObj[["tests"]][names(testStudyObj[["results"]][[testModelName]])]
)

expect_error(
  getTests(testStudyObj, modelID = "non-existent-model"),
  "non-existent-model"
)

expect_error(
  getTests(testStudyObj, modelID = "not null", testID = "not null"),
  "Can only filter by modelID or testID, not both"
)

# name **

expect_identical(
  getTests(testStudyName),
  testStudyObj[["tests"]]
)

expect_identical(
  getTests(testStudyName, testID = testTestName),
  testStudyObj[["tests"]][testTestName]
)

expect_error(
  getTests(testStudyName, testID = "non-existent-test"),
  "non-existent-test"
)

expect_identical(
  getTests(testStudyName, modelID = testModelName),
  testStudyObj[["tests"]][names(testStudyObj[["results"]][[testModelName]])]
)

expect_error(
  getTests(testStudyName, modelID = "non-existent-model"),
  "non-existent-model"
)

expect_error(
  getTests(testStudyName, modelID = "not null", testID = "not null"),
  "Can only filter by modelID or testID, not both"
)

# getResults -------------------------------------------------------------------

expect_identical(
  getResults(testStudyObj),
  testStudyObj[["results"]]
)

expect_identical(
  getResults(testStudyObj, modelID = testModelName),
  testStudyObj[["results"]][[testModelName]]
)

expect_error(
  getResults(testStudyObj, modelID = "non-existent-model"),
  "non-existent-model"
)

expect_identical(
  getResults(testStudyObj, modelID = testModelName, testID = testTestName),
  testStudyObj[["results"]][[testModelName]][[testTestName]]
)

expect_error(
  getResults(testStudyObj, modelID = testModelName, testID = "non-existent-test"),
  "non-existent-test"
)

expect_error(
  getResults(testStudyObj, testID = testTestName),
  "Must specify a model in order to specify a test"
)

expect_identical(
  getResults(testStudyName),
  testStudyObj[["results"]]
)

expect_identical(
  getResults(testStudyName, modelID = testModelName),
  testStudyObj[["results"]][[testModelName]]
)

expect_error(
  getResults(testStudyName, modelID = "non-existent-model"),
  "non-existent-model"
)

expect_identical(
  getResults(testStudyName, modelID = testModelName, testID = testTestName),
  testStudyObj[["results"]][[testModelName]][[testTestName]]
)

expect_error(
  getResults(testStudyName, modelID = testModelName, testID = "non-existent-test"),
  "non-existent-test"
)

expect_error(
  getResults(testStudyName, testID = testTestName),
  "Must specify a model in order to specify a test"
)

# Teardown ---------------------------------------------------------------------

unlink(tmplib, recursive = TRUE, force = TRUE)
.libPaths(libOrig)
