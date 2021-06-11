# Test addX() methods

# Setup ------------------------------------------------------------------------

source("tinytestSettings.R")
using(ttdo)

library(OmicNavigator)

tmplib <- tempfile()
dir.create(tmplib)

# Test addX() ------------------------------------------------------------------

study <- createStudy(name = "test", studyMeta = OmicNavigator:::testStudyMeta())

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

resultsLinkouts <- OmicNavigator:::testResultsLinkouts()
study <- addResultsLinkouts(study, resultsLinkouts = resultsLinkouts)

enrichmentsLinkouts <- OmicNavigator:::testEnrichmentsLinkouts()
study <- addEnrichmentsLinkouts(study, enrichmentsLinkouts = enrichmentsLinkouts)

metaFeaturesLinkouts <- OmicNavigator:::testMetaFeaturesLinkouts()
study <- addMetaFeaturesLinkouts(study, metaFeaturesLinkouts = metaFeaturesLinkouts)

expect_identical_xl(
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

# Test tibble and data.table input ---------------------------------------------

studyWithTibbleInput <- createStudy("tibble")
resultsTibble <- OmicNavigator:::testResults()
class(resultsTibble[[1]][[1]]) <- c("tbl_df", "tbl", "data.frame")
studyWithTibbleInput <- addResults(studyWithTibbleInput, resultsTibble)
expect_identical_xl(
  class(studyWithTibbleInput[["results"]][[1]][[1]]),
  "data.frame"
)
enrichmentsTibble <- OmicNavigator:::testEnrichments()
class(enrichmentsTibble[[1]][[1]][[1]]) <- c("tbl_df", "tbl", "data.frame")
studyWithTibbleInput <- addEnrichments(studyWithTibbleInput, enrichmentsTibble)
expect_identical_xl(
  class(studyWithTibbleInput[["enrichments"]][[1]][[1]][[1]]),
  "data.frame"
)

studyWithDataTableInput <- createStudy("data.table")
resultsDataTable <- OmicNavigator:::testResults()
class(resultsDataTable[[1]][[1]]) <- c("data.table", "data.frame")
studyWithDataTableInput <- addResults(studyWithDataTableInput, resultsDataTable)
expect_identical_xl(
  class(studyWithDataTableInput[["results"]][[1]][[1]]),
  "data.frame"
)
enrichmentsDataTable <- OmicNavigator:::testEnrichments()
class(enrichmentsDataTable[[1]][[1]][[1]]) <- c("data.table", "data.frame")
studyWithTibbleInput <- addEnrichments(studyWithTibbleInput, enrichmentsDataTable)
expect_identical_xl(
  class(studyWithTibbleInput[["enrichments"]][[1]][[1]][[1]]),
  "data.frame"
)

# Test incremental add and reset -----------------------------------------------

studyInc <- createStudy(name = "testInc")
resultsAll <- OmicNavigator:::testResults()
resultsOne <- resultsAll[1]
resultsTwo <- resultsAll[2]
resultsThreeA <- resultsAll[3]
resultsThreeA[[1]][[2]] <- NULL
resultsThreeB <- resultsAll[3]
resultsThreeB[[1]][[1]] <- NULL

# Add the data incrementally
studyInc <- addResults(studyInc, results = resultsOne)
studyInc <- addResults(studyInc, results = resultsTwo)
studyInc <- addResults(studyInc, results = resultsThreeA)
studyInc <- addResults(studyInc, results = resultsThreeB)

expect_equal_xl(
  studyInc[["results"]],
  resultsAll,
  info = "Data can be added incrementally"
)

# Reset the data and only include a subset
studyReset <- addResults(studyInc, results = resultsOne, reset = TRUE)

expect_equal_xl(
  studyReset[["results"]],
  resultsOne,
  info = "Existing data can be reset"
)

# Teardown ---------------------------------------------------------------------

unlink(tmplib, recursive = TRUE, force = TRUE)
