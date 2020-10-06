# Test addX() methods

# Setup ------------------------------------------------------------------------

source("tinytestSettings.R")
using(ttdo)

library(OmicNavigator)

tmplib <- tempfile()
dir.create(tmplib)

# Test addX() ------------------------------------------------------------------

study <- createStudy(name = "test")

features <- OmicNavigator:::testFeatures()
study <- addFeatures(study, features = features)

samples <- OmicNavigator:::testSamples()
study <- addSamples(study, samples = samples)

models <- OmicNavigator:::testModels()
study <- addModels(study, models = models)

assays <- OmicNavigator:::testAssays()
study <- addAssays(study, assays = assays)

tests <- OmicNavigator:::testTests()
study <- addTests(study, tests = tests)

annotations <- OmicNavigator:::testAnnotations()
study <- addAnnotations(study, annotations = annotations)

results <- OmicNavigator:::testResults()
study <- addResults(study, results = results)

enrichments <- OmicNavigator:::testEnrichments()
study <- addEnrichments(study, enrichments = enrichments)

metaFeatures <- OmicNavigator:::testMetaFeatures()
study <- addMetaFeatures(study, metaFeatures = metaFeatures)

barcodes <- OmicNavigator:::testBarcodes()
study <- addBarcodes(study, barcodes = barcodes)

reports <- OmicNavigator:::testReports()
study <- addReports(study, reports = reports)

expect_identical(
  study,
  OmicNavigator:::testStudy(name = "test")
)

# Plots include a unique enclosing environment, which is ultimately discarded
# when written to a package
plots <- OmicNavigator:::testPlots()
study <- addPlots(study, plots = plots)

suppressMessages(
  OmicNavigator::exportStudy(study, type = "package", path = tmplib)
)

# Export again with overlaps pre-calculated
suppressMessages(
  study <- addOverlaps(study)
)
suppressMessages(
  OmicNavigator::exportStudy(study, type = "package", path = tmplib)
)

# Teardown ---------------------------------------------------------------------

unlink(tmplib, recursive = TRUE, force = TRUE)
