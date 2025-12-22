# Test support for multiple transformations for assays

# Setup ------------------------------------------------------------------------

source("tinytestSettings.R")
using(ttdo)

library(OmicNavigator)

tmplib <- tempfile()
dir.create(tmplib)

testStudyName <- "transformations"
testStudyObj <- OmicNavigator:::testStudy(testStudyName)
testStudyPkg <- OmicNavigator:::studyToPkg(testStudyName)
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

expect_error_xl(
  OmicNavigator:::checkAssays(
    assays = list(
      model_04 = list(assaysDataFrame, assaysDataFrame + 1)
    )
  ),
  "must be named"
)

expect_error_xl(
  OmicNavigator:::checkAssays(
    assays = list(
      model_01 = assaysDataFrame,
      `model---02` = list(a1 = assaysDataFrame, a2 = assaysDataFrame + 1)
    )
  ),
  "---",
  info = "modelID cannot have --- because this is used in filenames to separate transformations"
)

expect_error_xl(
  OmicNavigator:::checkAssays(
    assays = list(
      model_01 = assaysDataFrame,
      model_02 = list(`a---1` = assaysDataFrame, `a---2` = assaysDataFrame + 1)
    )
  ),
  "---",
  info = "assayID cannot have --- because this is used in filenames to separate transformations"
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

# validateAssays() -------------------------------------------------------------

expect_true_xl(OmicNavigator:::validateAssays(testStudyObj))

testStudyObjInvalidAssays <- testStudyObj
row.names(testStudyObjInvalidAssays[["assays"]][["model_04"]][["a2"]])[1] <- "a"

expect_error_xl(
  OmicNavigator:::validateAssays(testStudyObjInvalidAssays),
  "modelID model_04: all assay transformations must have the same row names"
)

expect_error_xl(
  validateStudy(testStudyObjInvalidAssays),
  "modelID model_04: all assay transformations must have the same row names"
)

# exportStudy() ----------------------------------------------------------------

exportStudy(testStudyObj, type = "package", path = tmplib)

exportedAssaysDir <- file.path(
  tmplib,
  testStudyPkg,
  "inst",
  "OmicNavigator",
  "assays"
)

expect_true_xl(
  file.exists(file.path(exportedAssaysDir, "model_04---a1.txt"))
)

expect_true_xl(
  file.exists(file.path(exportedAssaysDir, "model_04---a2.txt"))
)

# installStudy() ---------------------------------------------------------------

installStudy(testStudyObj, library = tmplib)

expect_true_xl(
  testStudyPkg %in% installed.packages(lib.loc = tmplib)
)

# getAssays() from study package -----------------------------------------------

expect_equal_xl(
  getAssays(testStudyName, libraries = tmplib),
  testStudyObj[["assays"]]
)

expect_equal_xl(
  getAssays(testStudyName, modelID = "model_01", libraries = tmplib),
  testStudyObj[["assays"]][["model_01"]]
)

expect_equal_xl(
  getAssays(testStudyName, modelID = "model_04", libraries = tmplib),
  testStudyObj[["assays"]][["model_04"]]
)

# importStudy() ----------------------------------------------------------------

testStudyImported <- importStudy(testStudyName, libraries = tmplib)

expect_equal_xl(
  testStudyImported[["assays"]],
  testStudyObj[["assays"]]
)

# getPlottingData() from study object ------------------------------------------

plottingDataFromObj <- getPlottingData(
  study = testStudyObj,
  modelID = "model_04",
  featureID = "feature_0010"
)

expect_equal_xl(
  plottingDataFromObj[["assays"]][["a1"]],
  testStudyObj[["assays"]][["model_04"]][["a1"]]["feature_0010", , drop = FALSE]
)

expect_equal_xl(
  plottingDataFromObj[["assays"]][["a2"]],
  testStudyObj[["assays"]][["model_04"]][["a2"]]["feature_0010", , drop = FALSE]
)

expect_equal_xl(
  plottingDataFromObj[["features"]],
  subset(getFeatures(testStudyObj, modelID = "model_04"), customID == "feature_0010"),
  check.attributes = FALSE # subset() doesn't reset the row number
)

expect_equal_xl(
  plottingDataFromObj[["samples"]],
  getSamples(testStudyObj, modelID = "model_04")
)

# getPlottingData() multiFeature -----------------------------------------------

multiFeature <- c("feature_0010", "feature_0026")
plottingDataMultiFeature <- getPlottingData(
  study = testStudyObj,
  modelID = "model_04",
  featureID = multiFeature
)

expect_equal_xl(
  plottingDataMultiFeature[["assays"]][["a1"]],
  testStudyObj[["assays"]][["model_04"]][["a1"]][multiFeature, ]
)

expect_equal_xl(
  plottingDataMultiFeature[["assays"]][["a2"]],
  testStudyObj[["assays"]][["model_04"]][["a2"]][multiFeature, ]
)

expect_equal_xl(
  plottingDataMultiFeature[["features"]],
  subset(getFeatures(testStudyObj, modelID = "model_04"), customID %in% multiFeature),
  check.attributes = FALSE # subset() doesn't reset the row number
)

expect_equal_xl(
  plottingDataMultiFeature[["samples"]],
  getSamples(testStudyObj, modelID = "model_04")
)

# getPlottingData() from study package -----------------------------------------

plottingDataFromPkg <- getPlottingData(
  study = testStudyName,
  modelID = "model_04",
  featureID = "feature_0010",
  libraries = tmplib
)

expect_equal_xl(
  plottingDataFromPkg[["assays"]][["a1"]],
  testStudyObj[["assays"]][["model_04"]][["a1"]]["feature_0010", , drop = FALSE]
)

expect_equal_xl(
  plottingDataFromPkg[["assays"]][["a2"]],
  testStudyObj[["assays"]][["model_04"]][["a2"]]["feature_0010", , drop = FALSE]
)

# Teardown ---------------------------------------------------------------------

unlink(tmplib, recursive = TRUE, force = TRUE)
