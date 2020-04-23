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

contrasts <- OmicAnalyzer:::testContrasts()
study <- addContrasts(study, contrasts = contrasts)

annotations <- OmicAnalyzer:::testAnnotations()
study <- addAnnotations(study, annotations = annotations)

inferences <- OmicAnalyzer:::testInferences()
study <- addInferences(study, inferences = inferences)

enrichments <- OmicAnalyzer:::testEnrichments()
study <- addEnrichments(study, enrichments = enrichments)

expect_identical(
  study,
  OmicAnalyzer:::testStudy(name = "test")
)
