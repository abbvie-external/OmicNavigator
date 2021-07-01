# Test optional extra metadata capabilities

# Setup ------------------------------------------------------------------------

source("tinytestSettings.R")
using(ttdo)

library(OmicNavigator)

tmplib <- tempfile()
dir.create(tmplib)
libOrig <- .libPaths()
.libPaths(c(tmplib, libOrig))

# Extra test metadata ----------------------------------------------------------

testStudyName <- "extraTestMetadata"
testExtraMetadata <- OmicNavigator:::testStudy(name = testStudyName)

tests <- list(
 default = list(
   test_01 = list(
     description = "Name of first test",
     comparison_type = "treatment vs control",
     effect_size = "beta"
   ),
   test_02 = list(
     description = "Name of second test",
     comparison_type = "treatment vs control",
     effect_size = "logFC"
   )
 )
)

expect_silent_xl(
  testExtraMetadata <- addTests(testExtraMetadata, tests = tests, reset = TRUE)
)

expect_identical_xl(
  getTests(testExtraMetadata),
  tests
)

expect_identical_xl(
  getTests(testExtraMetadata, modelID = "model_01", quiet = TRUE),
  tests[["default"]]
)

expect_identical_xl(
  getTests(testExtraMetadata, modelID = "model_01", testID = "test_01", quiet = TRUE),
  tests[["default"]][["test_01"]]
)

suppressMessages(installStudy(testExtraMetadata))

expect_identical_xl(
  getTests(testStudyName),
  tests
)


expect_identical_xl(
  getTests(testStudyName, modelID = "model_01", quiet = TRUE),
  tests[["default"]]
)

expect_identical_xl(
  getTests(testStudyName, modelID = "model_01", testID = "test_01", quiet = TRUE),
  tests[["default"]][["test_01"]]
)

listed <- listStudies(libraries = tmplib)

expect_identical_xl(
  listed[[1]][["results"]][[1]][["tests"]][[1]][["testDisplay"]],
  tests[["default"]][["test_01"]][["description"]]
)

expect_identical_xl(
  listed[[1]][["results"]][[1]][["tests"]][[2]][["testDisplay"]],
  tests[["default"]][["test_02"]][["description"]]
)

imported <- importStudy(testStudyName)

expect_identical_xl(
  imported[["tests"]],
  testExtraMetadata[["tests"]]
)

# Teardown ---------------------------------------------------------------------

unlink(tmplib, recursive = TRUE, force = TRUE)
.libPaths(libOrig)
