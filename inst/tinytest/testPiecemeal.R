# Test piecemeal addition and export

# Setup ------------------------------------------------------------------------

source("tinytestSettings.R")
using(ttdo)

library(OmicNavigator)

tmplib <- tempfile()
dir.create(tmplib)

study <- createStudy(name = "test")

# Test results, models, and tests ----------------------------------------------

results <- OmicNavigator:::testResults()
study <- addResults(study, results = results)

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
    OmicNavigator::exportStudy(study, type = "package", path = tmplib)
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
    OmicNavigator::exportStudy(study, type = "package", path = tmplib)
  )
)

# Test enrichments and annotations ---------------------------------------------

enrichments <- OmicNavigator:::testEnrichments()
study <- addEnrichments(study, enrichments = enrichments)

annotations <- OmicNavigator:::testAnnotations()
study <- addAnnotations(study, annotations = annotations)

suppressMessages(
  OmicNavigator::exportStudy(study, type = "package", path = tmplib)
)

# Teardown ---------------------------------------------------------------------

unlink(tmplib, recursive = TRUE, force = TRUE)
