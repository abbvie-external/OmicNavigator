# Test getX() methods

# Setup ------------------------------------------------------------------------

library(OmicAnalyzer)
library(tinytest)

testStudyName <- "ABC"
testStudyObj <- OmicAnalyzer:::testStudy(name = testStudyName)
testStudyObj <- addPlots(testStudyObj, OmicAnalyzer:::testPlots())
testModelName <- names(testStudyObj[["models"]])[1]
testTestName <- testStudyObj[["tests"]][[1]][1, "testID"]
testAnnotationName <- names(testStudyObj[["annotations"]])[1]

tmplib <- tempfile()
dir.create(tmplib)
libOrig <- .libPaths()
.libPaths(c(tmplib, libOrig))
suppressMessages(OmicAnalyzer::installStudy(testStudyObj))

emptyStudy <- createStudy(name = "empty")

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

expect_error(
  getModels(emptyStudy),
  "No models available"
)

# getTests ---------------------------------------------------------------------

expect_identical(
  getTests(testStudyObj),
  testStudyObj[["tests"]]
)

expect_identical(
  getTests(testStudyObj, modelID = testModelName),
  testStudyObj[["tests"]][["default"]]
)

expect_identical(
  getTests(testStudyName),
  testStudyObj[["tests"]]
)

expect_identical(
  getTests(testStudyName, modelID = testModelName),
  testStudyObj[["tests"]][["default"]]
)

expect_error(
  getTests(emptyStudy),
  "No tests available"
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

expect_error(
  getResults(emptyStudy),
  "No results available"
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

# getEnrichments ---------------------------------------------------------------

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
  getEnrichments(testStudyObj, modelID = testModelName, annotationID = testAnnotationName),
  testStudyObj[["enrichments"]][[testModelName]][[testAnnotationName]]
)

expect_error(
  getEnrichments(testStudyObj, modelID = testModelName, annotationID = "non-existent-annotation"),
  "non-existent-annotation"
)

expect_error(
  getEnrichments(testStudyObj, annotationID = testAnnotationName),
  "Must specify a model in order to specify an annotation"
)

expect_identical(
  getEnrichments(testStudyObj, modelID = testModelName, annotationID = testAnnotationName, testID = testTestName),
  testStudyObj[["enrichments"]][[testModelName]][[testAnnotationName]][[testTestName]]
)

expect_error(
  getEnrichments(testStudyObj, modelID = testModelName, annotationID = testAnnotationName, testID = "non-existent-test"),
  "non-existent-test"
)

expect_error(
  getEnrichments(testStudyObj, testID = testTestName),
  "Must specify a model in order to specify a test"
)

expect_error(
  getEnrichments(testStudyObj, modelID = testModelName, testID = testTestName),
  "Must specify an annotation in order to specify a test"
)

expect_error(
  getEnrichments(testStudyObj, annotationID = testAnnotationName, testID = testTestName),
  "Must specify a model in order to specify an annotation"
)

expect_error(
  getEnrichments(emptyStudy),
  "No enrichments available"
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
  getEnrichments(testStudyName, modelID = testModelName, annotationID = testAnnotationName),
  testStudyObj[["enrichments"]][[testModelName]][[testAnnotationName]]
)

expect_error(
  getEnrichments(testStudyName, modelID = testModelName, annotationID = "non-existent-annotation"),
  "non-existent-annotation"
)

expect_error(
  getEnrichments(testStudyName, annotationID = testAnnotationName),
  "Must specify a model in order to specify an annotation"
)

expect_identical(
  getEnrichments(testStudyName, modelID = testModelName, annotationID = testAnnotationName, testID = testTestName),
  testStudyObj[["enrichments"]][[testModelName]][[testAnnotationName]][[testTestName]]
)

expect_error(
  getEnrichments(testStudyName, modelID = testModelName, annotationID = testAnnotationName, testID = "non-existent-test"),
  "non-existent-test"
)

expect_error(
  getEnrichments(testStudyName, testID = testTestName),
  "Must specify a model in order to specify a test"
)

expect_error(
  getEnrichments(testStudyName, modelID = testModelName, testID = testTestName),
  "Must specify an annotation in order to specify a test"
)

expect_error(
  getEnrichments(testStudyName, annotationID = testAnnotationName, testID = testTestName),
  "Must specify a model in order to specify an annotation"
)
# getPlots ---------------------------------------------------------------------

expect_identical(
  getPlots(testStudyObj),
  testStudyObj[["plots"]]
)

expect_identical(
  getPlots(testStudyObj, modelID = testModelName),
  testStudyObj[["plots"]][["default"]]
)

expect_error(
  getPlots(emptyStudy),
  "No plots available"
)

expect_identical(
  getPlots(testStudyName),
  testStudyObj[["plots"]]
)

expect_identical(
  getPlots(testStudyName, modelID = testModelName),
  testStudyObj[["plots"]][["default"]]
)

# Teardown ---------------------------------------------------------------------

unlink(tmplib, recursive = TRUE, force = TRUE)
.libPaths(libOrig)
