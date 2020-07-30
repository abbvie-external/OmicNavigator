# Test UpSet endpoints

# Setup ------------------------------------------------------------------------

library(OmicAnalyzer)
library(tinytest)

testStudyName <- "ABC"
testStudyObj <- OmicAnalyzer:::testStudy(name = testStudyName, version = "0.3")
testStudyObj <- addPlots(testStudyObj, OmicAnalyzer:::testPlots())
testModelName <- names(testStudyObj[["models"]])[1]
testTestsAll <- testStudyObj[["tests"]][[1]][, "testID"]
testTestName <- testTestsAll[1]
testAnnotationName <- names(testStudyObj[["annotations"]])[1]

tmplib <- tempfile()
dir.create(tmplib)
libOrig <- .libPaths()
.libPaths(c(tmplib, libOrig))
suppressMessages(OmicAnalyzer::installStudy(testStudyObj))

# getResultsIntersection -------------------------------------------------------

resultsIntersection <- getResultsIntersection(
  study = testStudyObj,
  modelID = testModelName,
  anchor = testTestName,
  mustTests = testTestsAll,
  notTests = c(),
  sigValue = .5,
  operator = "<",
  column = "p_val"
)

expect_true(
  nrow(resultsIntersection) > 0
)

expect_true(
  all(resultsIntersection[["p_val"]] < 0.5)
)

resultsIntersection <- getResultsIntersection(
  study = testStudyObj,
  modelID = testModelName,
  anchor = testTestName,
  mustTests = testTestsAll,
  notTests = c(),
  sigValue = 1.2,
  operator = ">",
  column = "beta"
)

expect_true(
  nrow(resultsIntersection) > 0
)

expect_true(
  all(resultsIntersection[["beta"]] > 1.2)
)

resultsIntersection <- getResultsIntersection(
  study = testStudyObj,
  modelID = testModelName,
  anchor = testTestName,
  mustTests = testTestsAll,
  notTests = c(),
  sigValue = c(.5, 1.2),
  operator = c("<", ">"),
  column = c("p_val", "beta")
)

expect_true(
  nrow(resultsIntersection) > 0
)

expect_true(
  all(resultsIntersection[["p_val"]] < 0.5)
)

expect_true(
  all(resultsIntersection[["beta"]] > 1.2)
)

# Confirm it works when there is only one test per model
testStudyObjSingle <- testStudyObj
testStudyObjSingle[["results"]][["model_03"]] <-
  testStudyObjSingle[["results"]][["model_03"]][1]

resultsIntersection <- getResultsIntersection(
  study = testStudyObjSingle,
  modelID = "model_03",
  anchor = testTestName,
  mustTests = testTestName,
  notTests = c(),
  sigValue = .5,
  operator = "<",
  column = "p_val"
)

expect_true(
  nrow(resultsIntersection) > 0
)

expect_identical(
  unique(resultsIntersection[, "Set_Membership"]),
  testTestName
)

resultsIntersection <- getResultsIntersection(
  study = testStudyName,
  modelID = testModelName,
  anchor = testTestName,
  mustTests = testTestsAll,
  notTests = c(),
  sigValue = c(.5, 1.2),
  operator = c("<", ">"),
  column = c("p_val", "beta")
)

expect_true(
  nrow(resultsIntersection) > 0
)

expect_true(
  inherits(resultsIntersection, "data.frame")
)

resultsTable <- getResultsTable(
  study = testStudyName,
  modelID = testModelName,
  testID = testTestName
)

# getResultsIntersection() should add the column Set_Membership between the
# feature metadata variable columns and the results columns
expect_identical(
  colnames(resultsIntersection),
  c(
    colnames(getFeatures(testStudyName, modelID = testModelName)),
    "Set_Membership",
    colnames(getResults(testStudyName, modelID = testModelName, testID = testTestName))[-1]
  )
)

# getEnrichmentsIntersection ---------------------------------------------------

enrichmentsIntersection <- getEnrichmentsIntersection(
  study = testStudyObj,
  modelID = testModelName,
  annotationID = testAnnotationName,
  mustTests = testTestsAll,
  notTests = c(),
  sigValue = .05,
  operator = "<",
  type = "nominal"
)

expect_true(
  nrow(enrichmentsIntersection) > 0
)

for (i in seq_along(testTestsAll)) {
  expect_true(
    all(enrichmentsIntersection[[testTestsAll[i]]] < 0.05)
  )
}

enrichmentsIntersection <- getEnrichmentsIntersection(
  study = testStudyObj,
  modelID = testModelName,
  annotationID = testAnnotationName,
  mustTests = testTestsAll,
  notTests = c(),
  sigValue = c(.05, .02),
  operator = c("<", ">"),
  type = "nominal"
)

expect_true(
  nrow(enrichmentsIntersection) > 0
)

for (i in seq_along(testTestsAll)) {
  expect_true(
    all(enrichmentsIntersection[[testTestsAll[i]]] < 0.05)
  )
  expect_true(
    all(enrichmentsIntersection[[testTestsAll[i]]] > 0.02)
  )
}

enrichmentsIntersection <- getEnrichmentsIntersection(
  study = testStudyName,
  modelID = testModelName,
  annotationID = testAnnotationName,
  mustTests = testTestsAll,
  notTests = c(),
  sigValue = c(.05, .02),
  operator = c("<", ">"),
  type = "adjusted"
)

expect_true(
  nrow(enrichmentsIntersection) > 0
)

for (i in seq_along(testTestsAll)) {
  expect_true(
    all(enrichmentsIntersection[[testTestsAll[i]]] < 0.05)
  )
  expect_true(
    all(enrichmentsIntersection[[testTestsAll[i]]] > 0.02)
  )
}

expect_true(
  inherits(enrichmentsIntersection, "data.frame")
)

expect_error(
  enrichmentsIntersection <- getEnrichmentsIntersection(
    study = testStudyName,
    modelID = testModelName,
    annotationID = testAnnotationName,
    mustTests = testTestsAll,
    notTests = c(),
    sigValue = c(.05, .02),
    operator = c("<", ">"),
    type = "wrong"
  ),
  "wrong"
)

# getResultsUpset --------------------------------------------------------------

resultsUpset <- getResultsUpset(
  study = testStudyObj,
  modelID = testModelName,
  sigValue = .5,
  operator = "<",
  column = "p_val"
)

resultsUpset <- getResultsUpset(
  study = testStudyName,
  modelID = testModelName,
  sigValue = .5,
  operator = "<",
  column = "p_val"
)

# getEnrichmentsUpset ----------------------------------------------------------

enrichmentsUpset <- getEnrichmentsUpset(
  study = testStudyObj,
  modelID = testModelName,
  annotationID = testAnnotationName,
  sigValue = .03,
  operator = "<",
  type = "nominal"
)

enrichmentsUpset <- getEnrichmentsUpset(
  study = testStudyObj,
  modelID = testModelName,
  annotationID = "annotation_02",
  sigValue = .04,
  operator = "<",
  type = "nominal"
)

enrichmentsUpset <- getEnrichmentsUpset(
  study = testStudyName,
  modelID = testModelName,
  annotationID = testAnnotationName,
  sigValue = .05,
  operator = "<",
  type = "adjusted"
)

enrichmentsUpset <- getEnrichmentsUpset(
  study = testStudyName,
  modelID = testModelName,
  annotationID = "annotation_02",
  sigValue = .05,
  operator = "<",
  type = "adjusted"
)

expect_error(
  enrichmentsUpset <- getEnrichmentsUpset(
    study = testStudyName,
    modelID = testModelName,
    annotationID = "annotation_02",
    sigValue = .05,
    operator = "<",
    type = "wrong"
  ),
  "wrong"
)

# Test new argument `tests`. The tests aren't ideal because my example created
# study created with testTests() only has 2 tests.

enrichmentsUpset <- getEnrichmentsUpset(
  study = testStudyObj,
  modelID = testModelName,
  annotationID = "annotation_02",
  sigValue = .04,
  operator = "<",
  type = "nominal",
  tests = testTestsAll
)

enrichmentsUpset <- getEnrichmentsUpset(
  study = testStudyName,
  modelID = testModelName,
  annotationID = "annotation_02",
  sigValue = .05,
  operator = "<",
  type = "adjusted",
  tests = testTestsAll
)

expect_error(
  enrichmentsUpset <- getEnrichmentsUpset(
    study = testStudyName,
    modelID = testModelName,
    annotationID = "annotation_02",
    sigValue = .05,
    operator = "<",
    type = "adjusted",
    tests = testTestsAll[1]
  ),
  "UpSet plot requires two or more tests to subset"
)

# getUpsetCols -----------------------------------------------------------------

upsetCols <- getUpsetCols(
  study = testStudyName,
  modelID = testModelName
)

expect_identical(
  upsetCols,
  c("beta", "p_val")
)

# Teardown ---------------------------------------------------------------------

unlink(tmplib, recursive = TRUE, force = TRUE)
.libPaths(libOrig)
