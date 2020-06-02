# Test app endpoints

# Setup ------------------------------------------------------------------------

library(OmicAnalyzer)
library(tinytest)

testStudyName <- "ABC"
testStudyObj <- OmicAnalyzer:::testStudy(name = testStudyName, version = "0.3")
testStudyObj <- addPlots(testStudyObj, OmicAnalyzer:::testPlots())
testModelName <- names(testStudyObj[["models"]])[1]
testTestName <- testStudyObj[["tests"]][[1]][1, "testID"]
testAnnotationName <- names(testStudyObj[["annotations"]])[1]

tmplib <- tempfile()
dir.create(tmplib)
libOrig <- .libPaths()
.libPaths(c(tmplib, libOrig))
suppressMessages(OmicAnalyzer::installStudy(testStudyObj))

# listStudies ------------------------------------------------------------------

studies <- listStudies(libraries = tmplib)

expect_identical(
  length(studies),
  1L
)

expect_identical(
  studies[[1]][["name"]],
  testStudyName
)

expect_identical(
  names(studies[[1]]),
  c("name", "package", "results", "enrichments", "plots")
)

expect_identical(
  studies[[1]][["package"]][["OmicAnalyzerVersion"]],
  as.character(utils::packageVersion("OmicAnalyzer"))
)

expect_identical(
  vapply(studies[[1]][["results"]], function(x) x[["modelID"]], character(1)),
  names(getModels(testStudyObj))
)

expect_identical(
  vapply(studies[[1]][["results"]][[1]][["tests"]],
         function(x) x[["testID"]], character(1)),
  getTests(testStudyObj, modelID = testModelName)[, "testID"]
)

expect_identical(
  vapply(studies[[1]][["enrichments"]], function(x) x[["modelID"]], character(1)),
  names(getModels(testStudyObj))
)

expect_identical(
  vapply(studies[[1]][["enrichments"]][[1]][["annotations"]],
         function(x) x[["annotationID"]], character(1)),
  names(getEnrichments(testStudyObj, modelID = testModelName))
)

expect_identical(
  vapply(studies[[1]][["plots"]], function(x) x[["modelID"]], character(1)),
  names(getModels(testStudyObj))
)

expect_identical(
  vapply(studies[[1]][["plots"]][[1]][["plots"]],
         function(x) x[["plotID"]], character(1)),
  names(getPlots(testStudyObj, modelID = testModelName))
)

# getResultsTable --------------------------------------------------------------

resultsTable <- getResultsTable(testStudyName, testModelName, testTestName)

expect_identical(
  class(resultsTable),
  "data.frame"
)

features <- getFeatures(testStudyObj, testModelName)
expect_true(all(colnames(features) %in% colnames(resultsTable)))

results <- getResults(testStudyObj, testModelName, testTestName)
expect_true(all(colnames(results) %in% colnames(resultsTable)))

# getEnrichmentsTable ----------------------------------------------------------

enrichmentsTable <- getEnrichmentsTable(testStudyName, testModelName, testAnnotationName)

expect_identical(
  class(enrichmentsTable),
  "data.frame"
)

expect_true(all(getTests(testStudyName, testModelName)[, "testID"] %in% colnames(enrichmentsTable)))

# getEnrichmentsNetwork --------------------------------------------------------

enrichmentsNetwork <- getEnrichmentsNetwork(testStudyName, testModelName, testAnnotationName)

expect_identical(
  class(enrichmentsNetwork),
  "list"
)

expect_identical(
  names(enrichmentsNetwork),
  c("tests", "nodes", "links")
)

expect_identical(
  enrichmentsNetwork[["tests"]],
  getTests(testStudyName, testModelName)[, "testID"]
)

# Teardown ---------------------------------------------------------------------

unlink(tmplib, recursive = TRUE, force = TRUE)
.libPaths(libOrig)
