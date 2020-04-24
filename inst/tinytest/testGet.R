library(OmicAnalyzer)
library(tinytest)

testStudyName <- "ABC"
testStudyObj <- OmicAnalyzer:::testStudy(name = "ABC")

tmplib <- tempfile()
dir.create(tmplib)
libOrig <- .libPaths()
.libPaths(c(tmplib, libOrig))
OmicAnalyzer::installStudy(testStudyObj)

installedStudies <- getInstalledStudies(libraries = tmplib)
expect_identical(
  installedStudies,
  testStudyName
)

expect_identical(
  getModels(testStudyObj),
  testStudyObj[["models"]]
)

expect_identical(
  getModels(testStudyName),
  testStudyObj[["models"]]
)

expect_identical(
  getResults(testStudyObj),
  testStudyObj[["results"]]
)

expect_identical(
  getResults(testStudyName),
  testStudyObj[["results"]]
)

# Cleanup
unlink(tmplib, recursive = TRUE, force = TRUE)
.libPaths(libOrig)
