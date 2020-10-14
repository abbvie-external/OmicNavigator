# Test custom plots

# Setup ------------------------------------------------------------------------

library(OmicNavigator)
library(tinytest)

testStudyName <- "ABC"
testStudyObj <- OmicNavigator:::testStudy(name = testStudyName)
plots <- OmicNavigator:::testPlots()
testStudyObj <- addPlots(testStudyObj, plots)
testModelName <- names(testStudyObj[["models"]])[1]

tmplib <- tempfile()
dir.create(tmplib)
libOrig <- .libPaths()
.libPaths(c(tmplib, libOrig))
suppressMessages(OmicNavigator::installStudy(testStudyObj))
testPkgName <- paste0(OmicNavigator:::getPrefix(), testStudyName)

# Test plots in exported package -----------------------------------------------

pkgDependencies <- utils::packageDescription(
  testPkgName,
  lib.loc = tmplib,
  fields = "Imports"
)

expect_identical(
  pkgDependencies,
  "ggplot2, graphics, rlang, stats"
)

pkgExports <- sort(getNamespaceExports(testPkgName))
plotsAll <- sort(unlist(lapply(plots, names), use.names = FALSE))

expect_identical(
  pkgExports,
  plotsAll
)

# plotStudy --------------------------------------------------------------------

pkgsAttachedPre <- search()
parSettingsPre <- graphics::par(no.readonly = TRUE)

expect_silent(
  plotStudy(testStudyObj, modelID = "model_01", feature = "feature_0001", plotID = "plotBase")
)

expect_silent(
  plotStudy(testStudyObj, modelID = "model_02", feature = "feature_0001", plotID = "plotBase")
)

expect_error(
  plotStudy(testStudyObj, modelID = "model_03", feature = "feature_0001", plotID = "plotBase")
)

expect_error(
  plotStudy(testStudyObj, modelID = "model_01", feature = "feature_0001", plotID = "plotGg")
)

expect_error(
  plotStudy(testStudyObj, modelID = "model_02", feature = "feature_0001", plotID = "plotGg")
)

expect_silent(
  plotStudy(testStudyObj, modelID = "model_03", feature = "feature_0001", plotID = "plotGg")
)

expect_error(
  plotStudy(testStudyObj, modelID = "model_01", feature = "feature_0001", plotID = "non-existent"),
  "non-existent"
)

expect_error(
  plotStudy(testStudyObj, modelID = "model_01", feature = "non-existent", plotID = "plotBase"),
  "non-existent"
)

# Remove plotting functions from current environment. This ensures that the
# calls to plotStudy() below obtain the plotting functions from the package
# namespace.
rm(list = plotsAll)

expect_silent(
  plotStudy(testStudyName, modelID = "model_01", feature = "feature_0001", plotID = "plotBase")
)

expect_silent(
  plotStudy(testStudyName, modelID = "model_02", feature = "feature_0001", plotID = "plotBase")
)

expect_error(
  plotStudy(testStudyName, modelID = "model_03", feature = "feature_0001", plotID = "plotBase")
)

expect_error(
  plotStudy(testStudyName, modelID = "model_01", feature = "feature_0001", plotID = "plotGg")
)

expect_error(
  plotStudy(testStudyName, modelID = "model_02", feature = "feature_0001", plotID = "plotGg")
)

expect_silent(
  plotStudy(testStudyName, modelID = "model_03", feature = "feature_0001", plotID = "plotGg")
)

expect_error(
  plotStudy(testStudyName, modelID = "model_01", feature = "feature_0001", plotID = "non-existent"),
  "non-existent"
)

expect_error(
  plotStudy(testStudyName, modelID = "model_01", feature = "non-existent", plotID = "plotBase"),
  "non-existent"
)

pkgsAttachedPost <- search()
expect_identical(
  pkgsAttachedPost,
  pkgsAttachedPre
)

parSettingsPost <- graphics::par(no.readonly = TRUE)
expect_identical(
  parSettingsPost,
  parSettingsPre
)

# getPlottingData --------------------------------------------------------------

plottingData <- getPlottingData(
  testStudyObj,
  modelID = testModelName,
  feature = "feature_0001"
)

samples <- getSamples(testStudyObj, modelID = testModelName)
assays <- getAssays(testStudyObj, modelID = testModelName)

expect_true(
  inherits(plottingData, "data.frame")
)

expect_identical(
  colnames(plottingData),
  c(colnames(samples), "feature")
)

expect_identical(
  plottingData[["feature"]],
  as.numeric(assays["feature_0001", ])
)

plottingData <- getPlottingData(
  testStudyName,
  modelID = testModelName,
  feature = "feature_0001"
)

samples <- getSamples(testStudyName, modelID = testModelName)
assays <- getAssays(testStudyName, modelID = testModelName)

expect_true(
  inherits(plottingData, "data.frame")
)

expect_identical(
  colnames(plottingData),
  c(colnames(samples), "feature")
)

expect_identical(
  plottingData[["feature"]],
  as.numeric(assays["feature_0001", ])
)

# Teardown ---------------------------------------------------------------------

unloadNamespace(testPkgName)
unlink(tmplib, recursive = TRUE, force = TRUE)
.libPaths(libOrig)
