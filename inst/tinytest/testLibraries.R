# Test app endpoints using arg `libraries` instead of setting `.libPaths()`

# Setup ------------------------------------------------------------------------

source("tinytestSettings.R")
using(ttdo)

library(OmicNavigator)

testStudyName <- "testArgLibraries"
testStudyObj <- OmicNavigator:::testStudy(name = testStudyName)
testModelName <- names(testStudyObj[["models"]])[1]
testTestName <- names(testStudyObj[["tests"]][[1]])[1]
testAnnotationName <- names(testStudyObj[["annotations"]])[1]
testTermName <- names(testStudyObj[["annotations"]][[testAnnotationName]][["terms"]])[1]

# Add a report file
tmpReport <- tempfile(fileext = ".html")
writeLines("<p>example</p>", tmpReport)
testStudyObj <- addReports(testStudyObj, list(model_02 = tmpReport))

# Install into temporary directory but do not update `.libPaths()`
tmplib <- tempfile()
dir.create(tmplib)
suppressMessages(installStudy(testStudyObj, library = tmplib))

# Test API endpoints -----------------------------------------------------------

expect_silent_xl(
  getStudyMeta(
    testStudyName,
    libraries = tmplib
  )
)

expect_silent_xl(
  getResultsTable(
    testStudyName,
    testModelName,
    testTestName,
    libraries = tmplib
  )
)

expect_silent_xl(
  getResultsTable(
    testStudyName,
    testModelName,
    testTestName,
    testAnnotationName,
    testTermName,
    libraries = tmplib
  )
)

expect_silent_xl(
  getNodeFeatures(
    testStudyName,
    testAnnotationName,
    testTermName,
    libraries = tmplib
  )
)

expect_silent_xl(
  getEnrichmentsTable(
    testStudyName,
    testModelName,
    testAnnotationName,
    libraries = tmplib
  )
)

expect_silent_xl(
  getEnrichmentsNetwork(
    testStudyName,
    testModelName,
    testAnnotationName,
    libraries = tmplib
  )
)

expect_silent_xl(
  getMetaFeaturesTable(
    testStudyName,
    testModelName,
    "feature_0042",
    libraries = tmplib
  )
)

expect_silent_xl(
  getBarcodeData(
    testStudyName,
    testModelName,
    testTestName,
    testAnnotationName,
    testTermName,
    libraries = tmplib
  )
)

expect_silent_xl(
  getReportLink(
    testStudyName,
    testModelName,
    libraries = tmplib
  )
)

expect_silent_xl(
  getAnnotations(
    testStudyName,
    testAnnotationName,
    libraries = tmplib
  )
)

expect_silent_xl(
  getNodeFeatures(
    testStudyName,
    testAnnotationName,
    testTermName,
    libraries = tmplib
  )
)

expect_silent_xl(
  getLinkFeatures(
    testStudyName,
    testAnnotationName,
    testTermName,
    "term_03",
    libraries = tmplib
  )
)

# Test UpSet endpoints ---------------------------------------------------------

testTestsAll <- names(testStudyObj[["tests"]][[1]])

expect_silent_xl(
  resultsIntersection <- getResultsIntersection(
    study = testStudyName,
    modelID = testModelName,
    anchor = testTestName,
    mustTests = testTestsAll,
    notTests = c(),
    sigValue = .5,
    operator = "<",
    column = "p_val",
    libraries = tmplib
  )
)

expect_silent_xl(
  getEnrichmentsIntersection(
    study = testStudyName,
    modelID = testModelName,
    annotationID = testAnnotationName,
    mustTests = testTestsAll,
    notTests = c(),
    sigValue = c(.05, .02),
    operator = c("<", ">"),
    type = "adjusted",
    libraries = tmplib
  )
)

expect_silent_xl(
  # Suppress warnings from UpSetR about ggplot2 deprecations
  suppressWarnings(
    getResultsUpset(
      study = testStudyName,
      modelID = testModelName,
      sigValue = c(.5, 1),
      operator = c("<", ">"),
      column = c("p_val", "beta"),
      libraries = tmplib
    )
  )
)

expect_silent_xl(
  # Suppress warnings from UpSetR about ggplot2 deprecations
  suppressWarnings(
    getEnrichmentsUpset(
      study = testStudyName,
      modelID = testModelName,
      annotationID = testAnnotationName,
      sigValue = .05,
      operator = "<",
      type = "adjusted",
      libraries = tmplib
    )
  )
)

expect_silent_xl(
  getUpsetCols(
    testStudyName,
    testModelName,
    libraries = tmplib
  )
)

# Teardown ---------------------------------------------------------------------

unlink(tmplib, recursive = TRUE, force = TRUE)
