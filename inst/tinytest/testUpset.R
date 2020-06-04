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

# Note: operator = "<" is internally converted to `<=`
expect_true(
  all(resultsIntersection[["p_val"]] <= 0.5)
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

# Note: operator = ">" is internally converted to `>=`
expect_true(
  all(resultsIntersection[["beta"]] >= 1.2)
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

# Note: operator = "<" is internally converted to `<=`
expect_true(
  all(resultsIntersection[["p_val"]] <= 0.5)
)

# Note: operator = ">" is internally converted to `>=`
expect_true(
  all(resultsIntersection[["beta"]] >= 1.2)
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
  inherits(resultsIntersection, "data.frame")
)

# getEnrichmentsIntersection -------------------------------------------------------

enrichmentsIntersection <- getEnrichmentsIntersection(
  study = testStudyObj,
  modelID = testModelName,
  annotationID = testAnnotationName,
  mustTests = testTestsAll,
  notTests = c(),
  sigValue = .03,
  operator = "<",
  type = "nominal"
)

for (i in seq_along(testTestsAll)) {
  expect_true(
    all(enrichmentsIntersection[[testTestsAll[i]]] <= 0.03)
  )
}

enrichmentsIntersection <- getEnrichmentsIntersection(
  study = testStudyObj,
  modelID = testModelName,
  annotationID = testAnnotationName,
  mustTests = testTestsAll,
  notTests = c(),
  sigValue = c(.03, .02),
  operator = c("<", ">"),
  type = "nominal"
)

for (i in seq_along(testTestsAll)) {
  expect_true(
    all(enrichmentsIntersection[[testTestsAll[i]]] <= 0.03)
  )
  expect_true(
    all(enrichmentsIntersection[[testTestsAll[i]]] >= 0.02)
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

for (i in seq_along(testTestsAll)) {
  expect_true(
    all(enrichmentsIntersection[[testTestsAll[i]]] <= 0.05)
  )
  expect_true(
    all(enrichmentsIntersection[[testTestsAll[i]]] >= 0.02)
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

# getEnrichmentsUpset --------------------------------------------------------------

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
  sigValue = .02,
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

# Teardown ---------------------------------------------------------------------

unlink(tmplib, recursive = TRUE, force = TRUE)
.libPaths(libOrig)
