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

# Teardown ---------------------------------------------------------------------

unlink(tmplib, recursive = TRUE, force = TRUE)
