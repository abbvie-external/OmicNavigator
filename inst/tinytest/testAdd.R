# Test addX() methods

# Setup ------------------------------------------------------------------------

source("tinytestSettings.R")
using(ttdo)

library(OmicAnalyzer)

tmplib <- tempfile()
dir.create(tmplib)

# Test addX() ------------------------------------------------------------------

study <- createStudy(name = "test")

features <- OmicAnalyzer:::testFeatures()
study <- addFeatures(study, features = features)

samples <- OmicAnalyzer:::testSamples()
study <- addSamples(study, samples = samples)

models <- OmicAnalyzer:::testModels()
study <- addModels(study, models = models)

assays <- OmicAnalyzer:::testAssays()
study <- addAssays(study, assays = assays)

tests <- OmicAnalyzer:::testTests()
study <- addTests(study, tests = tests)

annotations <- OmicAnalyzer:::testAnnotations()
study <- addAnnotations(study, annotations = annotations)

results <- OmicAnalyzer:::testResults()
study <- addResults(study, results = results)

enrichments <- OmicAnalyzer:::testEnrichments()
study <- addEnrichments(study, enrichments = enrichments)

metaFeatures <- OmicAnalyzer:::testMetaFeatures()
study <- addMetaFeatures(study, metaFeatures = metaFeatures)

barcodes <- OmicAnalyzer:::testBarcodes()
study <- addBarcodes(study, barcodes = barcodes)

reports <- OmicAnalyzer:::testReports()
study <- addReports(study, reports = reports)

expect_identical(
  study,
  OmicAnalyzer:::testStudy(name = "test")
)

# Plots include a unique enclosing environment, which is ultimately discarded
# when written to a package
plots <- OmicAnalyzer:::testPlots()
study <- addPlots(study, plots = plots)

suppressMessages(
  OmicAnalyzer::exportStudy(study, type = "package", path = tmplib)
)

# Export again with overlaps pre-calculated
study <- addOverlaps(study)
suppressMessages(
  OmicAnalyzer::exportStudy(study, type = "package", path = tmplib)
)

# Teardown ---------------------------------------------------------------------

unlink(tmplib, recursive = TRUE, force = TRUE)
