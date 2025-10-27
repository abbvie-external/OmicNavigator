# Test optional extra metadata capabilities

# Setup ------------------------------------------------------------------------

source("tinytestSettings.R")
using(ttdo)

library(OmicNavigator)

tmplib <- tempfile()
dir.create(tmplib)
libOrig <- .libPaths()
.libPaths(c(tmplib, libOrig))

testStudyName <- "extraTestMetadata"
testExtraMetadata <- OmicNavigator:::testStudy(name = testStudyName)

# Add extra metadata -----------------------------------------------------------

models <- list(
 model_01 = list(
   description = "Name of first model",
   data_type = "transcriptomics"
 ),
 model_02 = list(
   description = "Name of second model",
   data_type = "proteomics"
 )
)

expect_silent_xl(
  testExtraMetadata <- addModels(testExtraMetadata, models = models, reset = TRUE)
)

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

# Get extra metadata from study object -----------------------------------------

expect_identical_xl(
  getModels(testExtraMetadata),
  models
)

expect_identical_xl(
  getModels(testExtraMetadata, modelID = "model_01", quiet = TRUE),
  models[["model_01"]]
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

# Get extra metadata from installed study --------------------------------------

suppressMessages(installStudy(testExtraMetadata))

expect_identical_xl(
  getModels(testStudyName),
  models
)

expect_identical_xl(
  getModels(testStudyName, modelID = "model_01", quiet = TRUE),
  models[["model_01"]]
)

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

# Extra metadata imported from installed study package -------------------------

imported <- importStudy(testStudyName)

expect_identical_xl(
  imported[["models"]],
  testExtraMetadata[["models"]]
)

expect_identical_xl(
  imported[["tests"]],
  testExtraMetadata[["tests"]]
)

# Teardown ---------------------------------------------------------------------

unlink(tmplib, recursive = TRUE, force = TRUE)
.libPaths(libOrig)
