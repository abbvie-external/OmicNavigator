# Test getX() methods

# Setup ------------------------------------------------------------------------

library(OmicAnalyzer)
library(tinytest)

testStudyName <- "ABC"
testStudyObj <- OmicAnalyzer:::testStudy(name = testStudyName)
testModelName <- names(testStudyObj[["models"]])[1]
testTestName <- names(testStudyObj[["tests"]])[1]
testAnnotationName <- names(testStudyObj[["annotations"]])[1]

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
  testStudyObj[["tests"]][names(testStudyObj[["enrichments"]][[testModelName]])]
)

expect_error(
  getTests(testStudyObj, modelID = "non-existent-model"),
  "non-existent-model"
)

expect_error(
  getTests(testStudyObj, modelID = "not null", testID = "not null"),
  "Can only filter by modelID or testID, not both"
)

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
  testStudyObj[["tests"]][names(testStudyObj[["enrichments"]][[testModelName]])]
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

# getEnrichments -------------------------------------------------------------------

expect_identical(
  getEnrichments(testStudyObj),
  testStudyObj[["enrichments"]]
)

expect_identical(
  getEnrichments(testStudyObj, modelID = testModelName),
  testStudyObj[["enrichments"]][[testModelName]]
)

expect_error(
  getEnrichments(testStudyObj, modelID = "non-existent-model"),
  "non-existent-model"
)

expect_identical(
  getEnrichments(testStudyObj, modelID = testModelName, testID = testTestName),
  testStudyObj[["enrichments"]][[testModelName]][[testTestName]]
)

expect_error(
  getEnrichments(testStudyObj, modelID = testModelName, testID = "non-existent-test"),
  "non-existent-test"
)

expect_error(
  getEnrichments(testStudyObj, testID = testTestName),
  "Must specify a model in order to specify a test"
)

expect_identical(
  getEnrichments(testStudyObj, modelID = testModelName, testID = testTestName, annotationID = testAnnotationName),
  testStudyObj[["enrichments"]][[testModelName]][[testTestName]][[testAnnotationName]]
)

expect_error(
  getEnrichments(testStudyObj, modelID = testModelName, testID = testTestName, annotationID = "non-existent-annotation"),
  "non-existent-annotation"
)

expect_error(
  getEnrichments(testStudyObj, annotationID = testAnnotationName),
  "Must specify a test in order to specify an annotation"
)

expect_error(
  getEnrichments(testStudyObj, modelID = testModelName, annotationID = testAnnotationName),
  "Must specify a test in order to specify an annotation"
)

expect_identical(
  getEnrichments(testStudyName),
  testStudyObj[["enrichments"]]
)

expect_identical(
  getEnrichments(testStudyName, modelID = testModelName),
  testStudyObj[["enrichments"]][[testModelName]]
)

expect_error(
  getEnrichments(testStudyName, modelID = "non-existent-model"),
  "non-existent-model"
)

expect_identical(
  getEnrichments(testStudyName, modelID = testModelName, testID = testTestName),
  testStudyObj[["enrichments"]][[testModelName]][[testTestName]]
)

expect_error(
  getEnrichments(testStudyName, modelID = testModelName, testID = "non-existent-test"),
  "non-existent-test"
)

expect_error(
  getEnrichments(testStudyName, testID = testTestName),
  "Must specify a model in order to specify a test"
)

expect_identical(
  getEnrichments(testStudyName, modelID = testModelName, testID = testTestName, annotationID = testAnnotationName),
  testStudyObj[["enrichments"]][[testModelName]][[testTestName]][[testAnnotationName]]
)

expect_error(
  getEnrichments(testStudyName, modelID = testModelName, testID = testTestName, annotationID = "non-existent-annotation"),
  "non-existent-annotation"
)

expect_error(
  getEnrichments(testStudyName, annotationID = testAnnotationName),
  "Must specify a test in order to specify an annotation"
)

expect_error(
  getEnrichments(testStudyName, modelID = testModelName, annotationID = testAnnotationName),
  "Must specify a test in order to specify an annotation"
)

# Teardown ---------------------------------------------------------------------

unlink(tmplib, recursive = TRUE, force = TRUE)
.libPaths(libOrig)
