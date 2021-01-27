# Test custom plots

# Setup ------------------------------------------------------------------------

source("tinytestSettings.R")
using(ttdo)

library(OmicNavigator)

testStudyName <- "ABC"
testStudyObj <- OmicNavigator:::testStudy(name = testStudyName)
plots <- OmicNavigator:::testPlots()
testStudyObj <- addPlots(testStudyObj, plots)
testModelName <- names(testStudyObj[["models"]])[1]

tmplib <- tempfile()
dir.create(tmplib)
libOrig <- .libPaths()
.libPaths(c(tmplib, libOrig))
suppressMessages(installStudy(testStudyObj))
testPkgName <- paste0(OmicNavigator:::getPrefix(), testStudyName)

# Test plots in exported package -----------------------------------------------

pkgDependencies <- utils::packageDescription(
  testPkgName,
  lib.loc = tmplib,
  fields = "Imports"
)

expect_identical_xl(
  pkgDependencies,
  "ggplot2, graphics, rlang, stats"
)

pkgExports <- sort(getNamespaceExports(testPkgName))
plotsAll <- sort(unlist(lapply(plots, names), use.names = FALSE))

expect_identical_xl(
  pkgExports,
  plotsAll
)

# plotStudy --------------------------------------------------------------------

pkgsAttachedPre <- search()
parSettingsPre <- graphics::par(no.readonly = TRUE)

expect_silent_xl(
  plotStudy(testStudyObj, modelID = "model_01", featureID = "feature_0001", plotID = "plotBase")
)

expect_silent_xl(
  plotStudy(testStudyObj, modelID = "model_02", featureID = "feature_0001", plotID = "plotBase")
)

expect_error_xl(
  plotStudy(testStudyObj, modelID = "model_03", featureID = "feature_0001", plotID = "plotBase")
)

expect_error_xl(
  plotStudy(testStudyObj, modelID = "model_01", featureID = "feature_0001", plotID = "plotGg")
)

expect_error_xl(
  plotStudy(testStudyObj, modelID = "model_02", featureID = "feature_0001", plotID = "plotGg")
)

expect_silent_xl(
  plotStudy(testStudyObj, modelID = "model_03", featureID = "feature_0001", plotID = "plotGg")
)

expect_error_xl(
  plotStudy(testStudyObj, modelID = "model_01", featureID = "feature_0001", plotID = "non-existent"),
  "non-existent"
)

expect_error_xl(
  plotStudy(testStudyObj, modelID = "model_01", featureID = "non-existent", plotID = "plotBase"),
  "non-existent"
)

# Remove plotting functions from current environment. This ensures that the
# calls to plotStudy() below obtain the plotting functions from the package
# namespace.
rm(list = plotsAll)

expect_silent_xl(
  plotStudy(testStudyName, modelID = "model_01", featureID = "feature_0001", plotID = "plotBase")
)

expect_silent_xl(
  plotStudy(testStudyName, modelID = "model_02", featureID = "feature_0001", plotID = "plotBase")
)

expect_error_xl(
  plotStudy(testStudyName, modelID = "model_03", featureID = "feature_0001", plotID = "plotBase")
)

expect_error_xl(
  plotStudy(testStudyName, modelID = "model_01", featureID = "feature_0001", plotID = "plotGg")
)

expect_error_xl(
  plotStudy(testStudyName, modelID = "model_02", featureID = "feature_0001", plotID = "plotGg")
)

expect_silent_xl(
  plotStudy(testStudyName, modelID = "model_03", featureID = "feature_0001", plotID = "plotGg")
)

expect_error_xl(
  plotStudy(testStudyName, modelID = "model_01", featureID = "feature_0001", plotID = "non-existent"),
  "non-existent"
)

expect_error_xl(
  plotStudy(testStudyName, modelID = "model_01", featureID = "non-existent", plotID = "plotBase"),
  "non-existent"
)

pkgsAttachedPost <- search()
expect_identical_xl(
  pkgsAttachedPost,
  pkgsAttachedPre
)

parSettingsPost <- graphics::par(no.readonly = TRUE)
expect_identical_xl(
  parSettingsPost,
  parSettingsPre
)

# plotStudy (multiFeature) -----------------------------------------------------

expect_silent_xl(
  plotStudy(
    testStudyName,
    modelID = "model_01",
    featureID = c("feature_0026", "feature_0001"),
    plotID = "plotMultiFeature"
  )
)

expect_error_xl(
  plotStudy(
    testStudyName,
    modelID = "model_01",
    featureID = c("feature_0001"),
    plotID = "plotMultiFeature"
  ),
  "Received 1 featureID\\(s\\)",
  info = "Cannot pass a single featureID to a multiFeature plot"
)

expect_error_xl(
  plotStudy(
    testStudyName,
    modelID = "model_01",
    featureID = c("feature_0026", "feature_0001"),
    plotID = "plotBase"
  ),
  "Received 2 featureID\\(s\\)",
  info = "Cannot pass multiple featureIDs to a singleFeature plot"
)

# getPlottingData --------------------------------------------------------------

plottingData <- getPlottingData(
  testStudyObj,
  modelID = testModelName,
  featureID = "feature_0001"
)

samples <- getSamples(testStudyObj, modelID = testModelName)
assays <- getAssays(testStudyObj, modelID = testModelName)

expect_true_xl(
  inherits(plottingData, "list")
)

expect_true_xl(
  inherits(plottingData[["assays"]], "data.frame")
)

expect_true_xl(
  inherits(plottingData[["samples"]], "data.frame")
)

expect_true_xl(
  inherits(plottingData[["features"]], "data.frame")
)

plottingData <- getPlottingData(
  testStudyName,
  modelID = testModelName,
  featureID = "feature_0001"
)

samples <- getSamples(testStudyName, modelID = testModelName)
assays <- getAssays(testStudyName, modelID = testModelName)

expect_true_xl(
  inherits(plottingData, "list")
)

expect_true_xl(
  inherits(plottingData[["assays"]], "data.frame")
)

expect_true_xl(
  inherits(plottingData[["samples"]], "data.frame")
)

expect_true_xl(
  inherits(plottingData[["features"]], "data.frame")
)

# getPlottingData (multiFeature) -----------------------------------------------

plottingData <- getPlottingData(
  testStudyObj,
  modelID = testModelName,
  featureID = c("feature_0001", "feature_0002")
)

expect_true_xl(
  inherits(plottingData, "list")
)

expect_true_xl(
  inherits(plottingData[["assays"]], "data.frame")
)

expect_equal_xl(
  nrow(plottingData[["assays"]]),
  2
)

expect_true_xl(
  inherits(plottingData[["samples"]], "data.frame")
)

expect_true_xl(
  inherits(plottingData[["features"]], "data.frame")
)

expect_equal_xl(
  nrow(plottingData[["features"]]),
  2
)

plottingData <- getPlottingData(
  testStudyName,
  modelID = testModelName,
  featureID = c("feature_0001", "feature_0002")
)

expect_true_xl(
  inherits(plottingData, "list")
)

expect_true_xl(
  inherits(plottingData[["assays"]], "data.frame")
)

expect_equal_xl(
  nrow(plottingData[["assays"]]),
  2
)

expect_true_xl(
  inherits(plottingData[["samples"]], "data.frame")
)

expect_true_xl(
  inherits(plottingData[["features"]], "data.frame")
)

expect_equal_xl(
  nrow(plottingData[["features"]]),
  2
)

# Duplicate featureIDs should be deduplicated
plottingData <- getPlottingData(
  testStudyName,
  modelID = testModelName,
  featureID = c("feature_0001", "feature_0001")
)

expect_true_xl(
  inherits(plottingData, "list")
)

expect_true_xl(
  inherits(plottingData[["assays"]], "data.frame")
)

expect_equal_xl(
  nrow(plottingData[["assays"]]),
  1
)

expect_true_xl(
  inherits(plottingData[["samples"]], "data.frame")
)

expect_true_xl(
  inherits(plottingData[["features"]], "data.frame")
)

expect_equal_xl(
  nrow(plottingData[["features"]]),
  1
)

# Teardown ---------------------------------------------------------------------

unloadNamespace(testPkgName)
unlink(tmplib, recursive = TRUE, force = TRUE)
.libPaths(libOrig)
