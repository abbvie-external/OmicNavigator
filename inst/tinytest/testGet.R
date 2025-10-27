# Test getX() methods

# Setup ------------------------------------------------------------------------

# source(paste0(getwd(), "/inst/tinytest/tinytestSettings.R"))
source("tinytestSettings.R")
using(ttdo)

library(OmicNavigator)

testStudyName <- "ABC"
testStudyObj <- OmicNavigator:::testStudy(name = testStudyName)
testStudyObj <- addPlots(testStudyObj, OmicNavigator:::testPlots())
testModelName <- names(testStudyObj[["models"]])[1]
testTestName <- names(testStudyObj[["tests"]][[1]])[1]
testAnnotationName <- names(testStudyObj[["annotations"]])[1]
minimalStudyObj <- OmicNavigator:::testStudyMinimal()
minimalStudyName <- minimalStudyObj[["name"]]

tmplib <- tempfile()
dir.create(tmplib)
libOrig <- .libPaths()
.libPaths(c(tmplib, libOrig))
suppressMessages(installStudy(testStudyObj))
suppressMessages(installStudy(minimalStudyObj))

emptyStudyObj <- createStudy(name = "empty")

# getInstalledStudies ----------------------------------------------------------

installedStudies <- getInstalledStudies(libraries = tmplib)
expect_identical_xl(
  installedStudies,
  c(testStudyName, minimalStudyName)
)

# If there are no OmicNavigator study packages installed, return an empty vector
expect_identical_xl(
  getInstalledStudies(libraries = tempfile()),
  character(0)
)

# Test that only valid elements are accepted.
expect_error_xl(
  getInstalledStudies(hasElements = c("metaFeatures", "invalidElement")),
  "invalidElement"
)

expect_error_xl(
  getInstalledStudies(hasElements = c("invalidElement1", "invalidElement2")),
  "invalidElement1, invalidElement2"
)

expect_identical_xl(
  getInstalledStudies(hasElements = c("results", "enrichments"), libraries = tmplib),
  c(testStudyName, minimalStudyName)
)

# The example minimal study has no plots
expect_identical_xl(
  getInstalledStudies(hasElements = c("results", "enrichments", "plots"), libraries = tmplib),
  testStudyName
)

# getSamples -------------------------------------------------------------------

expect_identical_xl(
  getSamples(testStudyObj),
  testStudyObj[["samples"]]
)

expect_message_xl(
  getSamples(testStudyObj, modelID = "non-existent-model"),
  "Returning \"default\" samples for modelID \"non-existent-model\""
)

expect_identical_xl(
  getSamples(testStudyObj, modelID = testModelName),
  testStudyObj[["samples"]][["default"]]
)


expect_identical_xl(
  getSamples(testStudyObj, modelID = "non-existent-model"),
  testStudyObj[["samples"]][["default"]]
)

expect_identical_xl(
  getSamples(testStudyName),
  testStudyObj[["samples"]]
)

expect_identical_xl(
  getSamples(testStudyName, modelID = testModelName),
  testStudyObj[["samples"]][["default"]]
)

expect_message_xl(
  getSamples(testStudyObj, modelID = "non-existent-model"),
  "Returning \"default\" samples for modelID \"non-existent-model\""
)

expect_identical_xl(
  getSamples(testStudyName, modelID = "non-existent-model"),
  testStudyObj[["samples"]][["default"]]
)

expect_message_xl(
  getSamples(emptyStudyObj),
  "No samples available"
)

expect_error_xl(
  getSamples(1),
  "No method for object of class \"numeric\""
)

# getFeatures ------------------------------------------------------------------

expect_identical_xl(
  getFeatures(testStudyObj),
  testStudyObj[["features"]]
)

expect_identical_xl(
  getFeatures(testStudyObj, modelID = testModelName),
  testStudyObj[["features"]][["default"]]
)

expect_message_xl(
  getFeatures(testStudyObj, modelID = "non-existent-model"),
  "Returning \"default\" features for modelID \"non-existent-model\""
)

expect_identical_xl(
  getFeatures(testStudyObj, modelID = "non-existent-model"),
  testStudyObj[["features"]][["default"]]
)

expect_identical_xl(
  getFeatures(testStudyName),
  testStudyObj[["features"]]
)

expect_identical_xl(
  getFeatures(testStudyName, modelID = testModelName),
  testStudyObj[["features"]][["default"]]
)

expect_message_xl(
  getFeatures(testStudyName, modelID = "non-existent-model"),
  "Returning \"default\" features for modelID \"non-existent-model\""
)

expect_identical_xl(
  getFeatures(testStudyName, modelID = "non-existent-model"),
  testStudyObj[["features"]][["default"]]
)

expect_message_xl(
  getFeatures(emptyStudyObj),
  "No features available"
)

expect_error_xl(
  getFeatures(1),
  "No method for object of class \"numeric\""
)

# getAssays --------------------------------------------------------------------

expect_identical_xl(
  getAssays(testStudyObj),
  testStudyObj[["assays"]]
)

expect_identical_xl(
  getAssays(testStudyObj, modelID = testModelName),
  testStudyObj[["assays"]][[testModelName]]
)

expect_message_xl(
  getAssays(testStudyObj, modelID = "non-existent-model"),
  "non-existent-model"
)

expect_equal_xl(
  getAssays(testStudyName),
  testStudyObj[["assays"]]
)

expect_equal_xl(
  getAssays(testStudyName, modelID = testModelName),
  testStudyObj[["assays"]][[testModelName]]
)

expect_message_xl(
  getAssays(testStudyName, modelID = "non-existent-model"),
  "non-existent-model"
)

expect_message_xl(
  getAssays(emptyStudyObj),
  "No assays available"
)

expect_error_xl(
  getAssays(1),
  "No method for object of class \"numeric\""
)

# getModels --------------------------------------------------------------------

expect_identical_xl(
  getModels(testStudyObj),
  testStudyObj[["models"]]
)

expect_identical_xl(
  getModels(testStudyObj, modelID = testModelName),
  testStudyObj[["models"]][[testModelName]]
)

expect_message_xl(
  getModels(testStudyObj, modelID = "non-existent-model"),
  "non-existent-model"
)

expect_identical_xl(
  getModels(testStudyName),
  testStudyObj[["models"]]
)

expect_identical_xl(
  getModels(testStudyName, modelID = testModelName),
  testStudyObj[["models"]][[testModelName]]
)

expect_message_xl(
  getModels(testStudyName, modelID = "non-existent-model"),
  "non-existent-model"
)

expect_message_xl(
  getModels(emptyStudyObj),
  "No models available"
)

expect_error_xl(
  getModels(1),
  "No method for object of class \"numeric\""
)

# getTests ---------------------------------------------------------------------

expect_identical_xl(
  getTests(testStudyObj),
  testStudyObj[["tests"]]
)

expect_identical_xl(
  getTests(testStudyObj, modelID = testModelName),
  testStudyObj[["tests"]][["default"]]
)

expect_identical_xl(
  getTests(testStudyObj, modelID = testModelName, testID = testTestName),
  testStudyObj[["tests"]][["default"]][[testTestName]]
)

expect_error_xl(
  getTests(testStudyObj, testID = testTestName),
  "Must specify a model in order to specify a test"
)

expect_identical_xl(
  getTests(testStudyName),
  testStudyObj[["tests"]]
)

expect_identical_xl(
  getTests(testStudyName, modelID = testModelName),
  testStudyObj[["tests"]][["default"]]
)

expect_identical_xl(
  getTests(testStudyName, modelID = testModelName, testID = testTestName),
  testStudyObj[["tests"]][["default"]][[testTestName]]
)

expect_error_xl(
  getTests(testStudyName, testID = testTestName),
  "Must specify a model in order to specify a test"
)

expect_message_xl(
  getTests(emptyStudyObj),
  "No tests available"
)

expect_error_xl(
  getTests(1),
  "No method for object of class \"numeric\""
)

# getAnnotations ---------------------------------------------------------------

expect_identical_xl(
  getAnnotations(testStudyObj),
  testStudyObj[["annotations"]]
)

expect_identical_xl(
  getAnnotations(testStudyObj, annotationID = testAnnotationName),
  testStudyObj[["annotations"]][[testAnnotationName]]
)

expect_identical_xl(
  getAnnotations(testStudyName),
  testStudyObj[["annotations"]]
)

expect_identical_xl(
  getAnnotations(testStudyName, annotationID = testAnnotationName),
  testStudyObj[["annotations"]][[testAnnotationName]]
)

expect_message_xl(
  getAnnotations(emptyStudyObj),
  "No annotations available"
)

expect_error_xl(
  getAnnotations(1),
  "No method for object of class \"numeric\""
)

# getResults -------------------------------------------------------------------

expect_identical_xl(
  getResults(testStudyObj),
  testStudyObj[["results"]]
)

expect_identical_xl(
  getResults(testStudyObj, modelID = testModelName),
  testStudyObj[["results"]][[testModelName]]
)

expect_message_xl(
  getResults(testStudyObj, modelID = "non-existent-model"),
  "non-existent-model"
)

expect_identical_xl(
  getResults(testStudyObj, modelID = testModelName, testID = testTestName),
  testStudyObj[["results"]][[testModelName]][[testTestName]]
)

expect_message_xl(
  getResults(testStudyObj, modelID = testModelName, testID = "non-existent-test"),
  "non-existent-test"
)

expect_error_xl(
  getResults(testStudyObj, testID = testTestName),
  "Must specify a model in order to specify a test"
)

expect_message_xl(
  getResults(emptyStudyObj),
  "No results available"
)

expect_error_xl(
  getResults(1),
  "No method for object of class \"numeric\""
)

expect_equal_xl(
  getResults(testStudyName),
  testStudyObj[["results"]]
)

expect_equal_xl(
  getResults(testStudyName, modelID = testModelName),
  testStudyObj[["results"]][[testModelName]]
)

expect_message_xl(
  getResults(testStudyName, modelID = "non-existent-model"),
  "non-existent-model"
)

expect_equal_xl(
  getResults(testStudyName, modelID = testModelName, testID = testTestName),
  testStudyObj[["results"]][[testModelName]][[testTestName]]
)

expect_message_xl(
  getResults(testStudyName, modelID = testModelName, testID = "non-existent-test"),
  "non-existent-test"
)

expect_error_xl(
  getResults(testStudyName, testID = testTestName),
  "Must specify a model in order to specify a test"
)

# getEnrichments ---------------------------------------------------------------

expect_equal_xl(
  getEnrichments(testStudyObj),
  testStudyObj[["enrichments"]]
)

expect_equal_xl(
  getEnrichments(testStudyObj, modelID = testModelName),
  testStudyObj[["enrichments"]][[testModelName]]
)

expect_message_xl(
  getEnrichments(testStudyObj, modelID = "non-existent-model"),
  "non-existent-model"
)

expect_equal_xl(
  getEnrichments(testStudyObj, modelID = testModelName, annotationID = testAnnotationName),
  testStudyObj[["enrichments"]][[testModelName]][[testAnnotationName]]
)

expect_message_xl(
  getEnrichments(testStudyObj, modelID = testModelName, annotationID = "non-existent-annotation"),
  "non-existent-annotation"
)

expect_error_xl(
  getEnrichments(testStudyObj, annotationID = testAnnotationName),
  "Must specify a model in order to specify an annotation"
)

expect_equal_xl(
  getEnrichments(testStudyObj, modelID = testModelName, annotationID = testAnnotationName, testID = testTestName),
  testStudyObj[["enrichments"]][[testModelName]][[testAnnotationName]][[testTestName]]
)

expect_message_xl(
  getEnrichments(testStudyObj, modelID = testModelName, annotationID = testAnnotationName, testID = "non-existent-test"),
  "non-existent-test"
)

expect_error_xl(
  getEnrichments(testStudyObj, testID = testTestName),
  "Must specify a model in order to specify a test"
)

expect_error_xl(
  getEnrichments(testStudyObj, modelID = testModelName, testID = testTestName),
  "Must specify an annotation in order to specify a test"
)

expect_error_xl(
  getEnrichments(testStudyObj, annotationID = testAnnotationName, testID = testTestName),
  "Must specify a model in order to specify an annotation"
)

expect_message_xl(
  getEnrichments(emptyStudyObj),
  "No enrichments available"
)

expect_error_xl(
  getEnrichments(1),
  "No method for object of class \"numeric\""
)

expect_equal_xl(
  getEnrichments(testStudyName),
  testStudyObj[["enrichments"]]
)

expect_equal_xl(
  getEnrichments(testStudyName, modelID = testModelName),
  testStudyObj[["enrichments"]][[testModelName]]
)

expect_message_xl(
  getEnrichments(testStudyName, modelID = "non-existent-model"),
  "non-existent-model"
)

expect_equal_xl(
  getEnrichments(testStudyName, modelID = testModelName, annotationID = testAnnotationName),
  testStudyObj[["enrichments"]][[testModelName]][[testAnnotationName]]
)

expect_message_xl(
  getEnrichments(testStudyName, modelID = testModelName, annotationID = "non-existent-annotation"),
  "non-existent-annotation"
)

expect_error_xl(
  getEnrichments(testStudyName, annotationID = testAnnotationName),
  "Must specify a model in order to specify an annotation"
)

expect_equal_xl(
  getEnrichments(testStudyName, modelID = testModelName, annotationID = testAnnotationName, testID = testTestName),
  testStudyObj[["enrichments"]][[testModelName]][[testAnnotationName]][[testTestName]]
)

expect_message_xl(
  getEnrichments(testStudyName, modelID = testModelName, annotationID = testAnnotationName, testID = "non-existent-test"),
  "non-existent-test"
)

expect_error_xl(
  getEnrichments(testStudyName, testID = testTestName),
  "Must specify a model in order to specify a test"
)

expect_error_xl(
  getEnrichments(testStudyName, modelID = testModelName, testID = testTestName),
  "Must specify an annotation in order to specify a test"
)

expect_error_xl(
  getEnrichments(testStudyName, annotationID = testAnnotationName, testID = testTestName),
  "Must specify a model in order to specify an annotation"
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

expect_message_xl(
  getMetaFeatures(testStudyObj, modelID = "non-existent-model"),
  "Returning \"default\" metaFeatures for modelID \"non-existent-model\""
)

expect_identical_xl(
  getMetaFeatures(testStudyObj, modelID = "non-existent-model"),
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

expect_message_xl(
  getMetaFeatures(testStudyName, modelID = "non-existent-model"),
  "Returning \"default\" metaFeatures for modelID \"non-existent-model\""
)

expect_identical_xl(
  getMetaFeatures(testStudyName, modelID = "non-existent-model"),
  testStudyObj[["metaFeatures"]][["default"]]
)

expect_message_xl(
  getMetaFeatures(emptyStudyObj),
  "No metaFeatures available"
)

expect_error_xl(
  getMetaFeatures(1),
  "No method for object of class \"numeric\""
)

# getPlots ---------------------------------------------------------------------

expect_identical_xl(
  getPlots(testStudyObj),
  testStudyObj[["plots"]]
)

expect_identical_xl(
  getPlots(testStudyObj, modelID = testModelName),
  testStudyObj[["plots"]][["default"]]
)

expect_identical_xl(
  getPlots(testStudyObj, modelID = "model_03"),
  testStudyObj[["plots"]][["model_03"]]
)

expect_message_xl(
  getPlots(emptyStudyObj),
  "No plots available"
)

expect_error_xl(
  getPlots(1),
  "No method for object of class \"numeric\""
)

expect_identical_xl(
  getPlots(testStudyName),
  testStudyObj[["plots"]]
)

expect_identical_xl(
  getPlots(testStudyName, modelID = testModelName),
  testStudyObj[["plots"]][["default"]]
)

expect_identical_xl(
  getPlots(testStudyName, modelID = "model_03"),
  testStudyObj[["plots"]][["model_03"]]
)

# getMapping -------------------------------------------------------------------

expect_identical_xl(
  getMapping(testStudyObj),
  testStudyObj[["mapping"]]
)

expect_identical_xl(
  getMapping(testStudyName),
  testStudyObj[["mapping"]]
)

expect_message_xl(
  getMapping(emptyStudyObj),
  "No mapping available"
)

expect_error_xl(
  getMapping(1),
  "No method for object of class \"numeric\""
)

expect_error_xl(
  getMapping(data.frame()),
  "No method for object of class \"data.frame\""
)

# getBarcodes ------------------------------------------------------------------

expect_identical_xl(
  getBarcodes(testStudyObj),
  testStudyObj[["barcodes"]]
)

expect_identical_xl(
  getBarcodes(testStudyObj, modelID = testModelName),
  testStudyObj[["barcodes"]][["default"]]
)

expect_identical_xl(
  getBarcodes(testStudyObj, modelID = "model_03"),
  testStudyObj[["barcodes"]][["model_03"]],
  info = "Confirm model-specific barcode data returned"
)

expect_message_xl(
  getBarcodes(emptyStudyObj),
  "No barcodes available"
)

expect_error_xl(
  getBarcodes(1),
  "No method for object of class \"numeric\""
)

expect_identical_xl(
  getBarcodes(testStudyName, modelID = testModelName),
  list(
    statistic = "beta",
    labelStat = "Beta coefficient",
    labelLow = "Small effect size",
    labelHigh = "Large effect size"
  )
)

expect_identical_xl(
  getBarcodes(testStudyName, modelID = "model_03"),
  testStudyObj[["barcodes"]][["model_03"]],
  info = "Confirm model-specific barcode data returned"
)

# getReports -------------------------------------------------------------------

expect_identical_xl(
  getReports(testStudyObj),
  testStudyObj[["reports"]]
)

expect_identical_xl(
  getReports(testStudyObj, modelID = testModelName),
  testStudyObj[["reports"]][["default"]]
)

expect_identical_xl(
  getReports(testStudyObj, modelID = "model_03"),
  testStudyObj[["reports"]][["model_03"]],
  info = "Confirm model-specific report data returned"
)

expect_message_xl(
  getReports(emptyStudyObj),
  "No reports available"
)

expect_error_xl(
  getReports(1),
  "No method for object of class \"numeric\""
)

expect_identical_xl(
  getReports(testStudyName, modelID = testModelName),
  testStudyObj[["reports"]][["default"]]
)

expect_identical_xl(
  getReports(testStudyName, modelID = "model_03"),
  testStudyObj[["reports"]][["model_03"]],
  info = "Confirm model-specific report data returned"
)

# getResultsLinkouts -----------------------------------------------------------

expect_identical_xl(
  getResultsLinkouts(testStudyObj),
  testStudyObj[["resultsLinkouts"]]
)

expect_identical_xl(
  getResultsLinkouts(testStudyObj, modelID = testModelName),
  testStudyObj[["resultsLinkouts"]][["default"]]
)

expect_identical_xl(
  getResultsLinkouts(testStudyObj, modelID = "model_03"),
  testStudyObj[["resultsLinkouts"]][["model_03"]],
  info = "Confirm model-specific results table linkouts returned"
)

expect_message_xl(
  getResultsLinkouts(emptyStudyObj),
  "No resultsLinkouts available"
)

expect_error_xl(
  getResultsLinkouts(1),
  "No method for object of class \"numeric\""
)

expect_identical_xl(
  getResultsLinkouts(testStudyName, modelID = testModelName),
  testStudyObj[["resultsLinkouts"]][["default"]]
)

expect_identical_xl(
  getResultsLinkouts(testStudyName, modelID = "model_03"),
  testStudyObj[["resultsLinkouts"]][["model_03"]],
  info = "Confirm model-specific results table linkouts returned"
)

# getEnrichmentsLinkouts -------------------------------------------------------

expect_identical_xl(
  getEnrichmentsLinkouts(testStudyObj),
  testStudyObj[["enrichmentsLinkouts"]]
)

expect_identical_xl(
  getEnrichmentsLinkouts(testStudyObj, annotationID = testAnnotationName),
  testStudyObj[["enrichmentsLinkouts"]][[testAnnotationName]]
)

expect_identical_xl(
  getEnrichmentsLinkouts(testStudyObj, annotationID = "annotation_03"),
  testStudyObj[["enrichmentsLinkouts"]][["annotation_03"]],
  info = "Confirm annotation-specific enrichments table linkouts returned"
)

expect_message_xl(
  getEnrichmentsLinkouts(emptyStudyObj),
  "No enrichmentsLinkouts available"
)

expect_error_xl(
  getEnrichmentsLinkouts(1),
  "No method for object of class \"numeric\""
)

expect_identical_xl(
  getEnrichmentsLinkouts(testStudyName, annotationID = testAnnotationName),
  testStudyObj[["enrichmentsLinkouts"]][[testAnnotationName]]
)

expect_identical_xl(
  getEnrichmentsLinkouts(testStudyName, annotationID = "annotation_03"),
  testStudyObj[["enrichmentsLinkouts"]][["annotation_03"]],
  info = "Confirm annotation-specific enrichments table linkouts returned"
)

# getMetaFeaturesLinkouts ------------------------------------------------------

expect_identical_xl(
  getMetaFeaturesLinkouts(testStudyObj),
  testStudyObj[["metaFeaturesLinkouts"]]
)

expect_identical_xl(
  getMetaFeaturesLinkouts(testStudyObj, modelID = testModelName),
  testStudyObj[["metaFeaturesLinkouts"]][["default"]]
)

expect_identical_xl(
  getMetaFeaturesLinkouts(testStudyObj, modelID = "model_03"),
  testStudyObj[["metaFeaturesLinkouts"]][["model_03"]],
  info = "Confirm model-specific metaFeatures table linkouts returned"
)

expect_message_xl(
  getMetaFeaturesLinkouts(emptyStudyObj),
  "No metaFeaturesLinkouts available"
)

expect_error_xl(
  getMetaFeaturesLinkouts(1),
  "No method for object of class \"numeric\""
)

expect_identical_xl(
  getMetaFeaturesLinkouts(testStudyName, modelID = testModelName),
  testStudyObj[["metaFeaturesLinkouts"]][["default"]]
)

expect_identical_xl(
  getMetaFeaturesLinkouts(testStudyName, modelID = "model_03"),
  testStudyObj[["metaFeaturesLinkouts"]][["model_03"]],
  info = "Confirm model-specific metaFeatures table linkouts returned"
)

# Teardown ---------------------------------------------------------------------

unlink(tmplib, recursive = TRUE, force = TRUE)
.libPaths(libOrig)
