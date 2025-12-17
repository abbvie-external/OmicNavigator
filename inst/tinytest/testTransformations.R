# Test support for multiple transformations for assays and metaAssays

# Setup ------------------------------------------------------------------------

source("tinytestSettings.R")
using(ttdo)

library(OmicNavigator)

tmplib <- tempfile()
dir.create(tmplib)

testStudyName <- "transformations"
testStudyObj <- OmicNavigator:::testStudy(testStudyName)
plots <- OmicNavigator:::testPlots()
testStudyObj <- addPlots(testStudyObj, plots)

assaysDataFrame <- OmicNavigator:::testAssays(n = 1)[[1]]
assaysWithTransformations <- list(
  model_01 = assaysDataFrame,
  model_02 = list(a1 = assaysDataFrame, a2 = assaysDataFrame + 1)
)

# checkAssays() ----------------------------------------------------------------

expect_silent_xl(
  OmicNavigator:::checkAssays(assays = assaysWithTransformations)
)

expect_error_xl(
  OmicNavigator:::checkAssays(assays = c(assaysWithTransformations, model_04 = "banana"))
)

expect_error_xl(
  OmicNavigator:::checkAssays(
    assays = c(
      assaysWithTransformations,
      model_04 = list(a1 = cbind(assaysDataFrame, "banana"), a2 = assaysDataFrame + 1)
    )
  ),
  "The columns of the assays data frame must all be numeric.*model_04"
)

# sanitizeAssays() -------------------------------------------------------------

assaysClasses <- assaysWithTransformations
# Brute-forcing this one-time operation instead of doing something more complex
class(assaysClasses[["model_01"]]) <- c("custom_class", "data.frame")
class(assaysClasses[["model_02"]][["a1"]]) <- c("custom_class", "data.frame")
class(assaysClasses[["model_02"]][["a2"]]) <- c("custom_class", "data.frame")

expect_identical_xl(
  OmicNavigator:::sanitizeAssays(assaysClasses),
  assaysWithTransformations
)

# addAssays() ------------------------------------------------------------------

expect_silent_xl(
  testStudyObj <- addAssays(
    testStudyObj,
    assays = list(
      model_04 = list(
        a1 = assaysDataFrame,
        a2 = assaysDataFrame + 1
      )
    )
  )
)

# getAssays() ------------------------------------------------------------------

expect_identical_xl(
  getAssays(testStudyObj),
  testStudyObj[["assays"]]
)

expect_identical_xl(
  getAssays(testStudyObj, modelID = "model_04"),
  list(
    a1 = assaysDataFrame,
    a2 = assaysDataFrame + 1
  )
)

# validatePlots() --------------------------------------------------------------

# Only checks assays for model_04 if results has a model_04
testStudyObj <- addResults(
  testStudyObj,
  results = OmicNavigator:::testResults(nModels = 4)
)

expect_true_xl(OmicNavigator:::validatePlots(testStudyObj))

expect_true_xl(validateStudy(testStudyObj))

# Teardown ---------------------------------------------------------------------

unlink(tmplib, recursive = TRUE, force = TRUE)
