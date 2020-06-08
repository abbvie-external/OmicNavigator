library(OmicAnalyzer)
library(tinytest)

invalidStudy <- list(nameIncorrect = "incorrect")
class(invalidStudy) <- "oaStudy"

expect_error(
  addFeatures(invalidStudy, features = data.frame()),
  "name"
)

study <- createStudy(name = "test")

expect_error(
  addFeatures(study, features = NULL)
)

expect_error(
  addSamples(study, samples = NULL)
)

expect_error(
  addModels(study, models = NULL)
)

expect_error(
  addAssays(study, assays = NULL)
)

expect_error(
  addTests(study, tests = NULL)
)

expect_error(
  addAnnotations(study, annotations = NULL)
)

expect_error(
  addResults(study, results = NULL)
)

expect_error(
  addEnrichments(study, enrichments = NULL)
)

expect_error(
  addMetaFeatures(study, metaFeatures = NULL)
)

expect_error(
  addPlots(study, plots = NULL)
)

expect_error(
  addBarcodes(study, barcodes = NULL)
)
