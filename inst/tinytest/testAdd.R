library(OmicAnalyzer)
library(tinytest)

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

expect_identical(
  study,
  OmicAnalyzer:::testStudy(name = "test")
)

# Plots include a unique enclosing environment, which is ultimately discarded
# when written to a package
plots <- OmicAnalyzer:::testPlots()
study <- addPlots(study, plots = plots)
