# Test app endpoints

# Setup ------------------------------------------------------------------------

library(OmicAnalyzer)
library(tinytest)

testStudyName <- "ABC"
testStudyObj <- OmicAnalyzer:::testStudy(name = testStudyName, version = "0.3")
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
  c("name", "package", "results", "enrichments")
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
