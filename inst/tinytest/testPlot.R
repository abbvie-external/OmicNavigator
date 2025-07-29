# Test custom plots

# Setup ------------------------------------------------------------------------
# source(paste0(getwd(), "/inst/tinytest/tinytestSettings.R"))
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
  "data.table, ggplot2, graphics, plotly, rlang, stats"
)

pkgExports <- sort(getNamespaceExports(testPkgName))
plotsAll <- sort(unlist(lapply(plots, names), use.names = FALSE))

expect_identical_xl(
  pkgExports,
  plotsAll
)

# noMapping (multiModel) -------------------------------------------------------

testStudyObjNoMapping <- testStudyObj
testStudyObjNoMapping[["mapping"]] <- list()

mmodel <- names(testStudyObj[["models"]])[1:2]
mmtestID <- c("test_01", "test_02")
names(mmtestID) <- mmodel

expect_error_xl(
  plotStudy(
    testStudyObjNoMapping,
    modelID = mmodel,
    featureID = "feature_0002",
    plotID = "multiModel_barplot_sf",
    testID = mmtestID
  ),
  "Plot type \"multiModel\" requires mapping object"
)

testStudyObjNoMapping[["mapping"]] <- NULL

expect_error_xl(
  plotStudy(
    testStudyObjNoMapping,
    modelID = mmodel,
    featureID = "feature_0002",
    plotID = "multiModel_barplot_sf",
    testID = mmtestID
  ),
  "Plot type \"multiModel\" requires mapping object"
)

# plotStudy (object) -----------------------------------------------------------

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
#plotly
expect_error_xl(
  plotStudy(testStudyObj, modelID = "model_01", featureID = "feature_0001", plotID = "plotPlotly")
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

# plotStudy (package) ----------------------------------------------------------

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
#plotly
expect_error_xl(
  plotStudy(testStudyName, modelID = "model_04", featureID = "feature_0001", plotID = "plotPlotly")
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

# plotStudy (multiModel) -------------------------------------------------------

mmodel <- names(testStudyObj[["models"]])[1:2]
mmtestID <- c("test_01", "test_02")

expect_warning_xl(
  plotStudy(
    testStudyName,
    modelID = mmodel,
    featureID = c("feature_0002", "feature_0010", "feature_0026"),
    plotID = "multiModel_scatterplot",
    testID = mmtestID
  )
)

expect_error_xl(
  plotStudy(
    testStudyName,
    modelID = mmodel,
    featureID = c("feature_0026", "feature_0001", "feature_0002", "feature_0010"),
    plotID = "multiModel_scatterplot",
    testID = mmtestID[1]
  ),
  "Plot type \"multiModel\" requires testID to be either NULL \\(default\\) or a vector containing at least 2 testIDs"
)

expect_error_xl(
  plotStudy(
    testStudyName,
    modelID = mmodel,
    featureID = c("feature_0026", "feature_0001", "feature_0002", "feature_0010"),
    plotID = "multiModel_scatterplot",
    testID = c("test_01", "test_02")
  ),
  "At least one feature is not present in the first model passed"
)

expect_error_xl(
  plotStudy(
    testStudyName,
    modelID = mmodel,
    featureID = c("feature_0001"),
    plotID = "multiModel_barplot_sf",
    testID = c("test_01", "test_02")
  ),
  "The provided features list does not contain any feature present in model"
)

expect_error_xl(
  plotStudy(
    testStudyName,
    modelID = names(testStudyObj[["models"]]),
    featureID = "feature_0002",
    plotID = "multiModel_barplot_sf",
    testID = mmtestID
  ),
  "For multimodel plots modelID and testID are required to be vectors of the same length or testID to be set to NULL"
)

expect_error_xl(
  plotStudy(
    testStudyName,
    modelID = mmodel,
    featureID = c("feature_0020006", "feature_0001", "feature_0002"),
    plotID = "multiModel_barplot_sf",
    testID = mmtestID
  ),
  "Plot type \"singleFeature\" requires 1 featureID"
)

# plotStudy (testID) -----------------------------------------------------------

expect_silent_xl(
  plotStudy(testStudyName, modelID = "model_01", featureID = "feature_0001",
            plotID = "plotBase", testID = "test_01")
)

expect_error_xl(
  plotStudy(testStudyName, modelID = "model_01", featureID = "feature_0001",
            plotID = "plotBase", testID = "non-existent"),
  "one feature is not available in the results object for model"
)

expect_error_xl(
  plotStudy(testStudyName, modelID = "model_01", featureID = "non-existent",
            plotID = "plotBase", testID = "test_01"),
  "non-existent"
)

# plotStudy (multitest) --------------------------------------------------------
## check plotStudy calls without checking getPlottingData outputs

expect_silent_xl(
  plotStudy(testStudyName, modelID = "model_01", featureID = "feature_0001",
            plotID = "plotMultiTestSf", testID = c("test_01", "test_02"))
)

expect_error_xl(
  plotStudy(testStudyName, modelID = "model_01", featureID = "non-existent",
            plotID = "plotMultiTestSf", testID = c("test_01", "test_02")),
  "non-existent"
)

expect_error_xl(
  plotStudy(testStudyName, modelID = "model_01", featureID = "feature_0001",
            plotID = "plotMultiTestSf", testID = "test_01")
)

expect_silent_xl(
  plotStudy(testStudyName, modelID = "model_01",
            featureID = c("feature_0001", "feature_0002"),
            plotID = "plotMultiTestMf", testID = c("test_01", "test_02"))
)

expect_error_xl(
  plotStudy(testStudyName, modelID = "model_01",
            featureID = "non-existent",
            plotID = "plotMultiTestMf", testID = c("test_01", "test_02"))
)

expect_error_xl(
  plotStudy(testStudyName, modelID = "model_01",
            featureID = c("feature_0001", "feature_0002"),
            plotID = "plotMultiTestMf", testID = "test_01")
)

# getPlottingData (object) -----------------------------------------------------

plottingData <- getPlottingData(
  testStudyObj,
  modelID = testModelName,
  featureID = "feature_0001"
)

assays <- getAssays(testStudyObj, modelID = testModelName)
samples <- getSamples(testStudyObj, modelID = testModelName)
features <- getFeatures(testStudyObj, modelID = testModelName)

expect_true_xl(
  inherits(plottingData, "list")
)

expect_identical_xl(
  names(plottingData),
  c("assays", "samples", "features")
)

expect_true_xl(
  inherits(plottingData[["assays"]], "data.frame")
)

expect_equal_xl(
  plottingData[["assays"]],
  assays["feature_0001", ]
)

expect_true_xl(
  inherits(plottingData[["samples"]], "data.frame")
)

expect_equal_xl(
  plottingData[["samples"]],
  samples
)

expect_true_xl(
  inherits(plottingData[["features"]], "data.frame")
)

expect_equal_xl(
  plottingData[["features"]],
  features[features[[1]] == "feature_0001", ]
)

# getPlottingData (package) ----------------------------------------------------

plottingData <- getPlottingData(
  testStudyName,
  modelID = testModelName,
  featureID = "feature_0001"
)

expect_true_xl(
  inherits(plottingData, "list")
)

expect_identical_xl(
  names(plottingData),
  c("assays", "samples", "features")
)

expect_true_xl(
  inherits(plottingData[["assays"]], "data.frame")
)

expect_equal_xl(
  plottingData[["assays"]],
  assays["feature_0001", ]
)

expect_true_xl(
  inherits(plottingData[["samples"]], "data.frame")
)

expect_equal_xl(
  plottingData[["samples"]],
  samples
)

expect_true_xl(
  inherits(plottingData[["features"]], "data.frame")
)

expect_equal_xl(
  plottingData[["features"]],
  features[features[[1]] == "feature_0001", ]
)

# getPlottingData (object, multiFeature) ---------------------------------------

plottingData <- getPlottingData(
  testStudyObj,
  modelID = testModelName,
  featureID = c("feature_0001", "feature_0002")
)

expect_true_xl(
  inherits(plottingData, "list")
)

expect_identical_xl(
  names(plottingData),
  c("assays", "samples", "features")
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

# getPlottingData (package, multiFeature) --------------------------------------

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

rm(plottingData)

# getPlottingData (object, multiModel) -----------------------------------------

mmodel <- names(testStudyObj[["models"]])[1:2]
mmtestID <- c("test_01", "test_02")

suppressWarnings(plottingData <- getPlottingData(
  testStudyObj,
  modelID = mmodel,
  featureID = c("feature_0010", "feature_0020"),
  testID = mmtestID
))

expect_true_xl(
  inherits(plottingData, "list")
)

expect_identical_xl(
  names(plottingData)[1:2],
  mmodel
)

expect_identical_xl(
  names(plottingData[[1]]),
  c("assays", "samples", "features", "results")
)

expect_identical_xl(
  names(plottingData[[1]][[4]]),
  c("test_01")
)

expect_identical_xl(
  names(plottingData[[2]]),
  c("assays", "samples", "features", "results")
)

expect_identical_xl(
  names(plottingData[[2]][[4]]),
  c("test_02")
)

expect_true_xl(
  inherits(plottingData[[1]][["assays"]], "data.frame")
)

expect_equal_xl(
  nrow(plottingData[[1]][["assays"]]),
  2
)

expect_true_xl(
  inherits(plottingData[[1]][["samples"]], "data.frame")
)

expect_true_xl(
  inherits(plottingData[[1]][["features"]], "data.frame")
)

expect_equal_xl(
  nrow(plottingData[[1]][["features"]]),
  2
)

expect_true_xl(
  inherits(plottingData[[1]][["results"]], "list")
)

rm(plottingData)

# getPlottingData (package, multiModel) ----------------------------------------

mmodel <- names(testStudyObj[["models"]])[1:2]
mmtestID <- c("test_01", "test_02")
names(mmtestID) <- mmodel

suppressWarnings(plottingData <- getPlottingData(
  testStudyName,
  modelID = mmodel,
  featureID = c("feature_0010", "feature_0020"),
  testID = mmtestID
))

expect_true_xl(
  inherits(plottingData, "list")
)

expect_identical_xl(
  names(plottingData)[1:2],
  mmodel
)

expect_identical_xl(
  names(plottingData[[1]]),
  c("assays", "samples", "features", "results")
)

expect_identical_xl(
  names(plottingData[[1]][[4]]),
  c("test_01")
)

expect_identical_xl(
  names(plottingData[[2]]),
  c("assays", "samples", "features", "results")
)

expect_identical_xl(
  names(plottingData[[2]][[4]]),
  c("test_02")
)

expect_true_xl(
  inherits(plottingData[[1]][["assays"]], "data.frame")
)

expect_equal_xl(
  nrow(plottingData[[1]][["assays"]]),
  2
)

expect_true_xl(
  inherits(plottingData[[1]][["samples"]], "data.frame")
)

expect_true_xl(
  inherits(plottingData[[1]][["features"]], "data.frame")
)

expect_equal_xl(
  nrow(plottingData[[1]][["features"]]),
  2
)

expect_true_xl(
  inherits(plottingData[[1]][["results"]], "list")
)

rm(plottingData)

# getPlottingData (edge cases) -------------------------------------------------

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

# Ensure that getPlottingData() sorts the 3 data frames to match
needsSorting <- createStudy("needsSorting")
needsSortingSamples <- data.frame(sampleID = c("three", "one", "two"),
                                  sampleVar = letters[1:3],
                                  stringsAsFactors = FALSE)
needsSorting <- addSamples(needsSorting, list(default = needsSortingSamples))
needsSortingFeatures <- data.frame(featureID = c("f4", "f5", "f1", "f3", "f2"),
                                   featureVar = letters[1:5],
                                   stringsAsFactors = FALSE)
needsSorting <- addFeatures(needsSorting, list(default = needsSortingFeatures))
needsSortingAssays <- matrix(rnorm(5 * 3), nrow = 5, ncol = 3)
needsSortingAssays <- as.data.frame(needsSortingAssays)
row.names(needsSortingAssays) <- paste0("f", 1:5)
names(needsSortingAssays) <- c("one", "two", "three")
needsSorting <- addAssays(needsSorting, list(main = needsSortingAssays))

sortedPlottingData <- getPlottingData(
  needsSorting,
  modelID = "main",
  featureID = c("f2", "f1", "f3")
)

expect_identical_xl(
  sortedPlottingData[["samples"]][[1]],
  colnames(sortedPlottingData[["assays"]]),
  info = "Samples metadata rows should match assays columns"
)

expect_identical_xl(
  sortedPlottingData[["features"]][[1]],
  c("f2", "f1", "f3"),
  info = "Features metadata for plotting are sorted according to input featureIDs"
)

expect_identical_xl(
  rownames(sortedPlottingData[["assays"]]),
  c("f2", "f1", "f3"),
  info = "Assays rows for plotting are sorted according to input featureIDs"
)

# getPlottingData() should send warning if featureID or sampleID is missing
# metadata.
needsSortingAssaysExtra <- cbind(needsSortingAssays, four = rnorm(5))
needsSortingAssaysExtra <- rbind(needsSortingAssaysExtra, f6 = rnorm(4))
needsSorting <- addAssays(needsSorting, list(extra = needsSortingAssaysExtra))

expect_warning(
  getPlottingData(
    needsSorting,
    modelID = "extra",
    featureID = c("f2", "f1", "f3")
  ),
  "Not all of the sampleIDs have metadata",
  info = "Warning when assays has a column that is missing from the samples table"
)

expect_warning(
  getPlottingData(
    needsSorting,
    modelID = "extra",
    featureID = c("f2", "f1", "f3", "f6")
  ),
  "Not all of the featureIDs have metadata",
  info = "Warning when assays has a row that is missing from the features table"
)

# getPlottingData() can work even if the only data returned is "results"
testStudyObjMinimal <- OmicNavigator:::testStudyMinimal()

plottingDataMinimal <- getPlottingData(
  testStudyObjMinimal,
  modelID = testModelName,
  featureID = "feature_0001",
  testID = "test_01"
)

expect_identical_xl(
  plottingDataMinimal[["assays"]],
  list()
)

expect_identical_xl(
  plottingDataMinimal[["samples"]],
  list()
)

expect_identical_xl(
  plottingDataMinimal[["features"]],
  list()
)

expect_true_xl(
  inherits(plottingDataMinimal[["results"]], "data.frame")
)

expect_identical_xl(
  plottingDataMinimal[["results"]][[1]],
  "feature_0001"
)

# getPlottingData (object, testID) ---------------------------------------------

plottingData <- getPlottingData(
  testStudyObj,
  modelID = testModelName,
  featureID = "feature_0001",
  testID = "test_01"
)

assays <- getAssays(testStudyObj, modelID = testModelName)
samples <- getSamples(testStudyObj, modelID = testModelName)
features <- getFeatures(testStudyObj, modelID = testModelName)
results <- getResults(testStudyObj, modelID = testModelName, testID = "test_01")

expect_true_xl(
  inherits(plottingData, "list")
)

expect_identical_xl(
  names(plottingData),
  c("assays", "samples", "features", "results")
)

expect_true_xl(
  inherits(plottingData[["assays"]], "data.frame")
)

expect_equal_xl(
  plottingData[["assays"]],
  assays["feature_0001", ]
)

expect_true_xl(
  inherits(plottingData[["samples"]], "data.frame")
)

expect_equal_xl(
  plottingData[["samples"]],
  samples
)

expect_true_xl(
  inherits(plottingData[["features"]], "data.frame")
)

expect_equal_xl(
  plottingData[["features"]],
  features[features[[1]] == "feature_0001", ]
)

expect_true_xl(
  inherits(plottingData[["results"]], "data.frame")
)

expect_equal_xl(
  plottingData[["results"]],
  results[results[[1]] == "feature_0001", ]
)

# getPlottingData (package, testID) --------------------------------------------

plottingData <- getPlottingData(
  testStudyName,
  modelID = testModelName,
  featureID = "feature_0001",
  testID = "test_01"
)

expect_true_xl(
  inherits(plottingData, "list")
)

expect_identical_xl(
  names(plottingData),
  c("assays", "samples", "features", "results")
)

expect_true_xl(
  inherits(plottingData[["assays"]], "data.frame")
)

expect_equal_xl(
  plottingData[["assays"]],
  assays["feature_0001", ]
)

expect_true_xl(
  inherits(plottingData[["samples"]], "data.frame")
)

expect_equal_xl(
  plottingData[["samples"]],
  samples
)

expect_true_xl(
  inherits(plottingData[["features"]], "data.frame")
)

expect_equal_xl(
  plottingData[["features"]],
  features[features[[1]] == "feature_0001", ]
)

expect_true_xl(
  inherits(plottingData[["results"]], "data.frame")
)

expect_equal_xl(
  plottingData[["results"]],
  results[results[[1]] == "feature_0001", ]
)

# getPlottingData (package, multitest) -----------------------------------------

plottingData <- getPlottingData(
  testStudyName,
  modelID = testModelName,
  featureID = c("feature_0001", "feature_0002"),
  testID = c("test_01", "test_02")
)

expect_true_xl(
  inherits(plottingData, "list")
)

expect_identical_xl(
  names(plottingData),
  c("assays", "samples", "features", "results")
)

expect_true_xl(
  inherits(plottingData[["assays"]], "data.frame")
)

expect_equal_xl(
  plottingData[["assays"]],
  assays[c("feature_0001", "feature_0002"), ]
)

expect_true_xl(
  inherits(plottingData[["samples"]], "data.frame")
)

expect_equal_xl(
  plottingData[["samples"]],
  samples
)

expect_true_xl(
  inherits(plottingData[["features"]], "data.frame")
)

expect_equal_xl(
  plottingData[["features"]],
  features[features[[1]] == c("feature_0001", "feature_0002"), ]
)

expect_true_xl(
  inherits(plottingData[["results"]], "list")
)

result_test01 <- testStudyObj$results[[testModelName]]$test_01
result_test01 <- result_test01[order(result_test01$customID),]
expect_equal_xl(
  plottingData[["results"]][["test_01"]],
  result_test01[result_test01$customID %in% c("feature_0001", "feature_0002"),]
)

result_test02 <- testStudyObj$results[[testModelName]]$test_02
result_test02 <- result_test02[order(result_test02$customID),]
expect_equal_xl(
  plottingData[["results"]][["test_02"]],
  result_test02[result_test02$customID %in% c("feature_0001", "feature_0002"),]
)

# getPlottingData (package, multiFeature, testID) ------------------------------

plottingData <- getPlottingData(
  testStudyName,
  modelID = testModelName,
  featureID = c("feature_0006", "feature_0002"),
  testID = "test_01"
)

expect_identical_xl(
  plottingData[["results"]][[1]],
  c("feature_0006", "feature_0002")
)

# Plotly Plots -----------------------------------------------------------------

json <- plotStudy(testStudyName, modelID = "model_03", featureID = "feature_0001", plotID = "plotPlotly")
expect_true_xl(
  inherits(json, "json")
)

# Catch plots of plotType "plotly" that don't correctly return a plotly plot
notPlotlyFunc <- function(x) graphics::plot(1:10)
notPlotlyFuncList <- list(
  default = list(
    notPlotlyFunc = list(
      displayName = "notPlotly",
      plotType = "plotly",
      packages = "plotly"
    )
  )
)

testStudyNotPloty <- addPlots(testStudyObj, notPlotlyFuncList)
expect_error(
  plotStudy(testStudyNotPloty, modelID = "model_01", featureID = "feature_0001", plotID = "notPlotlyFunc"),
  "The plotID \"notPlotlyFunc\" has plotType \"plotly\" but did not return an object with class \"plotly\"",
  info = "Detect invalid plotly functions"
)


# Teardown ---------------------------------------------------------------------

unloadNamespace(testPkgName)
unlink(tmplib, recursive = TRUE, force = TRUE)
.libPaths(libOrig)

