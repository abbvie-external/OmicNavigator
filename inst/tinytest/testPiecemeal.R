# Test piecemeal addition, export, and retrieval

# Setup ------------------------------------------------------------------------

source("tinytestSettings.R")
using(ttdo)

library(OmicNavigator)

tmplib <- tempfile()
dir.create(tmplib)
libOrig <- .libPaths()
.libPaths(c(tmplib, libOrig))

study <- createStudy(name = "test")

# Test results -----------------------------------------------------------------

results <- OmicNavigator:::testResults()
study <- addResults(study, results = results)

suppressWarnings(suppressMessages(installStudy(study)))

testModelName <- names(study[["results"]])[1]
testTestName <- names(study[["results"]][[1]])[1]

expect_equal_xl(
  getResultsTable(study$name, testModelName, testTestName),
  getResults(study$name, testModelName, testTestName)
)

expect_identical_xl(
  getEnrichmentsTable(study$name, testModelName, "non-existent-annotation"),
  data.frame()
)

expect_identical_xl(
  getEnrichmentsNetwork(study$name, testModelName, "non-existent-annotation"),
  list()
)

expect_identical_xl(
  getNodeFeatures(study$name, "non-existent-annotation", "non-existent-term"),
  character()
)

expect_identical_xl(
  getLinkFeatures(study$name, "non-existent-annotation", "non-existent-term-1", "non-existent-term-2"),
  character()
)

expect_identical_xl(
  getMetaFeaturesTable(study$name, testModelName, "non-existent-feature"),
  data.frame()
)

expect_identical_xl(
  getBarcodeData(study$name, testModelName, testTestName, "non-existent-annotation", "non-existent-term"),
  list()
)

expect_identical_xl(
  getReportLink(study, testModelName),
  character()
)

# Test models and tests --------------------------------------------------------

models <- OmicNavigator:::testModels()
# Confirm a message is emitted and empty list returned when only some models
# have a description available.
study <- addModels(study, models = models[1])
expect_message(
  modelReturnObject <- getModels(study, modelID = names(models)[2]),
  names(models)[2]
)
expect_identical_xl(
  modelReturnObject,
  list()
)

study <- addModels(study, models = models)
tests <- OmicNavigator:::testTests()
study <- addTests(study, tests = tests)

suppressWarnings(
  suppressMessages(
    exportStudy(study, type = "package", path = tmplib)
  )
)

# Test features, samples, and assays -------------------------------------------

features <- OmicNavigator:::testFeatures()
study <- addFeatures(study, features = features)

samples <- OmicNavigator:::testSamples()
study <- addSamples(study, samples = samples)

assays <- OmicNavigator:::testAssays()
study <- addAssays(study, assays = assays)

suppressWarnings(
  suppressMessages(
    exportStudy(study, type = "package", path = tmplib)
  )
)

# Test enrichments and annotations ---------------------------------------------

enrichments <- OmicNavigator:::testEnrichments()
study <- addEnrichments(study, enrichments = enrichments)

annotations <- OmicNavigator:::testAnnotations()
study <- addAnnotations(study, annotations = annotations)

suppressMessages(
  exportStudy(study, type = "package", path = tmplib)
)

# Teardown ---------------------------------------------------------------------

unlink(tmplib, recursive = TRUE, force = TRUE)
.libPaths(libOrig)
