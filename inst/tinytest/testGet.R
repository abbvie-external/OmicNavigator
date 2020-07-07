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

# getSamples -------------------------------------------------------------------

expect_identical(
  getSamples(testStudyObj),
  testStudyObj[["samples"]]
)

expect_message(
  getSamples(testStudyObj, model = "non-existent-model"),
  "Returning \"default\" samples for model \"non-existent-model\""
)

expect_identical(
  getSamples(testStudyObj, model = testModelName),
  testStudyObj[["samples"]][["default"]]
)


expect_identical(
  getSamples(testStudyObj, model = "non-existent-model"),
  testStudyObj[["samples"]][["default"]]
)

expect_identical(
  getSamples(testStudyName),
  testStudyObj[["samples"]]
)

expect_identical(
  getSamples(testStudyName, model = testModelName),
  testStudyObj[["samples"]][["default"]]
)

expect_message(
  getSamples(testStudyObj, model = "non-existent-model"),
  "Returning \"default\" samples for model \"non-existent-model\""
)

expect_identical(
  getSamples(testStudyName, model = "non-existent-model"),
  testStudyObj[["samples"]][["default"]]
)

expect_error(
  getSamples(emptyStudy),
  "No samples available"
)

expect_error(
  getSamples(1),
  "No method for object of class \"numeric\""
)

# getFeatures -------------------------------------------------------------------

expect_identical(
  getFeatures(testStudyObj),
  testStudyObj[["features"]]
)

expect_identical(
  getFeatures(testStudyObj, model = testModelName),
  testStudyObj[["features"]][["default"]]
)

expect_message(
  getFeatures(testStudyObj, model = "non-existent-model"),
  "Returning \"default\" features for model \"non-existent-model\""
)

expect_identical(
  getFeatures(testStudyObj, model = "non-existent-model"),
  testStudyObj[["features"]][["default"]]
)

expect_identical(
  getFeatures(testStudyName),
  testStudyObj[["features"]]
)

expect_identical(
  getFeatures(testStudyName, model = testModelName),
  testStudyObj[["features"]][["default"]]
)

expect_message(
  getFeatures(testStudyName, model = "non-existent-model"),
  "Returning \"default\" features for model \"non-existent-model\""
)

expect_identical(
  getFeatures(testStudyName, model = "non-existent-model"),
  testStudyObj[["features"]][["default"]]
)

expect_error(
  getFeatures(emptyStudy),
  "No features available"
)

expect_error(
  getFeatures(1),
  "No method for object of class \"numeric\""
)

# getAssays -------------------------------------------------------------------

expect_identical(
  getAssays(testStudyObj),
  testStudyObj[["assays"]]
)

expect_identical(
  getAssays(testStudyObj, model = testModelName),
  testStudyObj[["assays"]][[testModelName]]
)

expect_error(
  getAssays(testStudyObj, model = "non-existent-model"),
  "non-existent-model"
)

expect_identical(
  getAssays(testStudyName),
  testStudyObj[["assays"]]
)

expect_identical(
  getAssays(testStudyName, model = testModelName),
  testStudyObj[["assays"]][[testModelName]]
)

expect_error(
  getAssays(testStudyName, model = "non-existent-model"),
  "non-existent-model"
)

expect_error(
  getAssays(emptyStudy),
  "No assays available"
)

expect_error(
  getAssays(1),
  "No method for object of class \"numeric\""
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

expect_error(
  getModels(1),
  "No method for object of class \"numeric\""
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

expect_error(
  getTests(1),
  "No method for object of class \"numeric\""
)

# getAnnotations ---------------------------------------------------------------

expect_identical(
  getAnnotations(testStudyObj),
  testStudyObj[["annotations"]]
)

expect_identical(
  getAnnotations(testStudyObj, annotationID = testAnnotationName),
  testStudyObj[["annotations"]][[testAnnotationName]]
)

expect_identical(
  getAnnotations(testStudyName),
  testStudyObj[["annotations"]]
)

expect_identical(
  getAnnotations(testStudyName, annotationID = testAnnotationName),
  testStudyObj[["annotations"]][[testAnnotationName]]
)

expect_error(
  getAnnotations(emptyStudy),
  "No annotations available"
)

expect_error(
  getAnnotations(1),
  "No method for object of class \"numeric\""
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

expect_error(
  getResults(1),
  "No method for object of class \"numeric\""
)

# The sorting can get funky when retrieving from the database. Thus for now
# confirm the dimensions are correct. This should resolve itself if we move away
# from the database setup.
expect_identical(
  lapply(getResults(testStudyName), function(x) lapply(x, dim)),
  lapply(testStudyObj[["results"]], function(x) lapply(x, dim))
)

expect_identical(
  lapply(getResults(testStudyName, modelID = testModelName), dim),
  lapply(testStudyObj[["results"]][[testModelName]], dim)
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

expect_error(
  getEnrichments(1),
  "No method for object of class \"numeric\""
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

# getMetaFeatures -------------------------------------------------------------------

expect_identical(
  getMetaFeatures(testStudyObj),
  testStudyObj[["metaFeatures"]]
)

expect_identical(
  getMetaFeatures(testStudyObj, model = testModelName),
  testStudyObj[["metaFeatures"]][["default"]]
)

expect_message(
  getMetaFeatures(testStudyObj, model = "non-existent-model"),
  "Returning \"default\" metaFeatures for model \"non-existent-model\""
)

expect_identical(
  getMetaFeatures(testStudyObj, model = "non-existent-model"),
  testStudyObj[["metaFeatures"]][["default"]]
)

expect_identical(
  getMetaFeatures(testStudyName),
  testStudyObj[["metaFeatures"]]
)

expect_identical(
  getMetaFeatures(testStudyName, model = testModelName),
  testStudyObj[["metaFeatures"]][["default"]]
)

expect_message(
  getMetaFeatures(testStudyName, model = "non-existent-model"),
  "Returning \"default\" metaFeatures for model \"non-existent-model\""
)

expect_identical(
  getMetaFeatures(testStudyName, model = "non-existent-model"),
  testStudyObj[["metaFeatures"]][["default"]]
)

expect_error(
  getMetaFeatures(emptyStudy),
  "No metaFeatures available"
)

expect_error(
  getMetaFeatures(1),
  "No method for object of class \"numeric\""
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

expect_identical(
  getPlots(testStudyObj, modelID = "model_03"),
  testStudyObj[["plots"]][["model_03"]]
)

expect_error(
  getPlots(emptyStudy),
  "No plots available"
)

expect_error(
  getPlots(1),
  "No method for object of class \"numeric\""
)

expect_identical(
  getPlots(testStudyName),
  testStudyObj[["plots"]]
)

expect_identical(
  getPlots(testStudyName, modelID = testModelName),
  testStudyObj[["plots"]][["default"]]
)

expect_identical(
  getPlots(testStudyName, modelID = "model_03"),
  testStudyObj[["plots"]][["model_03"]]
)

# getBarcodes ---------------------------------------------------------------------

expect_identical(
  getBarcodes(testStudyObj),
  testStudyObj[["barcodes"]]
)

expect_identical(
  getBarcodes(testStudyObj, modelID = testModelName),
  testStudyObj[["barcodes"]][["default"]]
)

expect_error(
  getBarcodes(emptyStudy),
  "No barcodes available"
)

expect_error(
  getBarcodes(1),
  "No method for object of class \"numeric\""
)

expect_identical(
  getBarcodes(testStudyName, modelID = testModelName),
  list(statistic = "beta", logFoldChange = NA_character_, absolute = 1L,
       labelStat = "Beta coefficient", labelLow = "Small effect size",
       labelHigh = "Large effect size", featureDisplay = NA_character_)
)

expect_identical(
  getBarcodes(testStudyObj, modelID = "model_03"),
  testStudyObj[["barcodes"]][["model_03"]],
  info = "Confirm model-specific barcode data returned"
)

# Teardown ---------------------------------------------------------------------

unlink(tmplib, recursive = TRUE, force = TRUE)
.libPaths(libOrig)
