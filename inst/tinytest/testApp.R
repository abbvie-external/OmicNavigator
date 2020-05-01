# Test app endpoints

# Setup ------------------------------------------------------------------------

library(OmicAnalyzer)
library(tinytest)

testStudyName <- "ABC"
testStudyObj <- OmicAnalyzer:::testStudy(name = testStudyName, version = "0.3")
testModelName <- names(testStudyObj[["models"]])[1]
testTestName <- names(testStudyObj[["tests"]])[1]
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
  names(studies),
  testStudyName
)

expect_identical(
  names(studies[[testStudyName]]),
  c("package", "results", "enrichments")
)

# getResultsTable --------------------------------------------------------------

resultsTable <- getResultsTable(testStudyName, testModelName, testTestName)

expect_identical(
  class(resultsTable),
  "data.frame"
)

expect_true(all(colnames(testStudyObj[["features"]]) %in% colnames(resultsTable)))

expect_true(all(colnames(testStudyObj[["results"]]) %in% colnames(resultsTable)))

# getEnrichmentsTable ----------------------------------------------------------

enrichmentsTable <- getEnrichmentsTable(testStudyName, testModelName, testAnnotationName)

expect_identical(
  class(enrichmentsTable),
  "data.frame"
)

expect_true(all(names(getTests(testStudyName, testModelName)) %in% colnames(enrichmentsTable)))

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
  names(getTests(testStudyName, testModelName))
)

# Teardown ---------------------------------------------------------------------

unlink(tmplib, recursive = TRUE, force = TRUE)
.libPaths(libOrig)
