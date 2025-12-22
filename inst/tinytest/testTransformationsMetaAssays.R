# Test support for multiple transformations for metaAssays

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

# Need normal assays for model_04 to test getPlottingData()
testStudyObj <- addAssays(
  study = testStudyObj,
  assays = OmicNavigator:::testAssays(n = 4)
)

metaAssaysDataFrame <- OmicNavigator:::testMetaAssays(n = 1)[[1]]
metaAssaysWithTransformations <- list(
  model_01 = metaAssaysDataFrame,
  model_02 = list(a1 = metaAssaysDataFrame, a2 = metaAssaysDataFrame + 1)
)

# checkMetaAssays() ------------------------------------------------------------

expect_silent_xl(
  OmicNavigator:::checkMetaAssays(metaAssays = metaAssaysWithTransformations)
)

expect_error_xl(
  OmicNavigator:::checkMetaAssays(metaAssays = c(metaAssaysWithTransformations, model_04 = "banana"))
)

expect_error_xl(
  OmicNavigator:::checkMetaAssays(
    metaAssays = c(
      metaAssaysWithTransformations,
      model_04 = list(a1 = cbind(metaAssaysDataFrame, "banana"), a2 = metaAssaysDataFrame + 1)
    )
  ),
  "The columns of the metaAssays data frame must all be numeric.*model_04"
)

expect_error_xl(
  OmicNavigator:::checkMetaAssays(
    metaAssays = list(
      model_04 = list(metaAssaysDataFrame, metaAssaysDataFrame + 1)
    )
  ),
  "must be named"
)

expect_error_xl(
  OmicNavigator:::checkMetaAssays(
    metaAssays = list(
      model_01 = metaAssaysDataFrame,
      `model---02` = list(a1 = metaAssaysDataFrame, a2 = metaAssaysDataFrame + 1)
    )
  ),
  "---",
  info = "modelID cannot have --- because this is used in filenames to separate transformations"
)

expect_error_xl(
  OmicNavigator:::checkMetaAssays(
    metaAssays = list(
      model_01 = metaAssaysDataFrame,
      model_02 = list(`a---1` = metaAssaysDataFrame, `a---2` = metaAssaysDataFrame + 1)
    )
  ),
  "---",
  info = "assayID cannot have --- because this is used in filenames to separate transformations"
)

# sanitizeMetaAssays() ---------------------------------------------------------

metaAssaysClasses <- metaAssaysWithTransformations
# Brute-forcing this one-time operation instead of doing something more complex
class(metaAssaysClasses[["model_01"]]) <- c("custom_class", "data.frame")
class(metaAssaysClasses[["model_02"]][["a1"]]) <- c("custom_class", "data.frame")
class(metaAssaysClasses[["model_02"]][["a2"]]) <- c("custom_class", "data.frame")

expect_identical_xl(
  OmicNavigator:::sanitizeMetaAssays(metaAssaysClasses),
  metaAssaysWithTransformations
)

# addMetaAssays() --------------------------------------------------------------

expect_warning_xl(
  testStudyObj <- addMetaAssays(
    testStudyObj,
    metaAssays = list(
      model_04 = list(
        a1 = metaAssaysDataFrame,
        a2 = metaAssaysDataFrame + 1
      )
    )
  ),
  "Support for metaAssays is highly experimental"
)

# getMetaAssays() --------------------------------------------------------------

expect_identical_xl(
  getMetaAssays(testStudyObj),
  testStudyObj[["metaAssays"]]
)

expect_identical_xl(
  getMetaAssays(testStudyObj, modelID = "model_04"),
  list(
    a1 = metaAssaysDataFrame,
    a2 = metaAssaysDataFrame + 1
  )
)

# validateMetaAssays() ---------------------------------------------------------

# Only checks metaAssays for model_04 if results has a model_04
testStudyObj <- addResults(
  testStudyObj,
  results = OmicNavigator:::testResults(nModels = 4)
)

expect_true_xl(OmicNavigator:::validateMetaAssays(testStudyObj))

expect_true_xl(validateStudy(testStudyObj))

testStudyObjInvalidMetaAssays <- testStudyObj
row.names(testStudyObjInvalidMetaAssays[["metaAssays"]][["model_04"]][["a2"]])[1] <- "a"

expect_error_xl(
  OmicNavigator:::validateMetaAssays(testStudyObjInvalidMetaAssays),
  "modelID model_04: all metaAssay transformations must have the same row names"
)

expect_error_xl(
  validateStudy(testStudyObjInvalidMetaAssays),
  "modelID model_04: all metaAssay transformations must have the same row names"
)

# exportStudy() ----------------------------------------------------------------

exportStudy(testStudyObj, type = "package", path = tmplib)

exportedMetaAssaysDir <- file.path(
  tmplib,
  testStudyPkg,
  "inst",
  "OmicNavigator",
  "metaAssays"
)

expect_true_xl(
  file.exists(file.path(exportedMetaAssaysDir, "model_04---a1.txt"))
)

expect_true_xl(
  file.exists(file.path(exportedMetaAssaysDir, "model_04---a2.txt"))
)

# installStudy() ---------------------------------------------------------------

installStudy(testStudyObj, library = tmplib)

expect_true_xl(
  testStudyPkg %in% installed.packages(lib.loc = tmplib)
)

# getMetaAssays() from study package -------------------------------------------

expect_equal_xl(
  getMetaAssays(testStudyName, libraries = tmplib),
  testStudyObj[["metaAssays"]]
)

expect_equal_xl(
  getMetaAssays(testStudyName, modelID = "model_01", libraries = tmplib),
  testStudyObj[["metaAssays"]][["model_01"]]
)

expect_equal_xl(
  getMetaAssays(testStudyName, modelID = "model_04", libraries = tmplib),
  testStudyObj[["metaAssays"]][["model_04"]]
)

# importStudy() ----------------------------------------------------------------

testStudyImported <- importStudy(testStudyName, libraries = tmplib)

expect_equal_xl(
  testStudyImported[["metaAssays"]],
  testStudyObj[["metaAssays"]]
)

# getPlottingData() from study object ------------------------------------------

plottingDataFromObj <- getPlottingData(
  study = testStudyObj,
  modelID = "model_04",
  featureID = "feature_0010"
)

expectedMetaFeatures <- subset(
  testStudyObj[["metaFeatures"]][["default"]],
  customID == "feature_0010",
  select = "metaFeatureID"
)[[1]]

expect_equal_xl(
  plottingDataFromObj[["metaAssays"]][["a1"]],
  testStudyObj[["metaAssays"]][["model_04"]][["a1"]][expectedMetaFeatures, , drop = FALSE]
)

expect_equal_xl(
  plottingDataFromObj[["assays"]][["a2"]],
  testStudyObj[["assays"]][["model_04"]][["a2"]][expectedMetaFeatures, , drop = FALSE]
)

# getPlottingData() from study package -----------------------------------------

plottingDataFromPkg <- getPlottingData(
  study = testStudyName,
  modelID = "model_04",
  featureID = "feature_0010",
  libraries = tmplib
)

expect_equal_xl(
  plottingDataFromPkg[["metaAssays"]][["a1"]],
  testStudyObj[["metaAssays"]][["model_04"]][["a1"]][expectedMetaFeatures, , drop = FALSE]
)

expect_equal_xl(
  plottingDataFromPkg[["assays"]][["a2"]],
  testStudyObj[["assays"]][["model_04"]][["a2"]][expectedMetaFeatures, , drop = FALSE]
)

# getPlottingData() multiFeature -----------------------------------------------

multiFeature <- c("feature_0010", "feature_0026")
plottingDataMultiFeature <- getPlottingData(
  study = testStudyObj,
  modelID = "model_04",
  featureID = multiFeature
)

expectedMetaFeatures <- subset(
  testStudyObj[["metaFeatures"]][["default"]],
  customID %in% multiFeature,
  select = "metaFeatureID"
)[[1]]

expect_identical_xl(
  sort(row.names(plottingDataMultiFeature[["metaAssays"]][["a1"]])),
  sort(row.names(testStudyObj[["metaAssays"]][["model_04"]][["a1"]][expectedMetaFeatures, , drop = FALSE]))
)

expect_identical_xl(
  sort(row.names(plottingDataMultiFeature[["metaAssays"]][["a2"]])),
  sort(row.names(testStudyObj[["metaAssays"]][["model_04"]][["a2"]][expectedMetaFeatures, , drop = FALSE]))
)

# Teardown ---------------------------------------------------------------------

unlink(tmplib, recursive = TRUE, force = TRUE)
