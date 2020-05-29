library(OmicAnalyzer)
library(tinytest)

testStudyName <- "ABC"
testStudyObj <- OmicAnalyzer:::testStudy(name = testStudyName)
plots <- OmicAnalyzer:::testPlots()
testStudyObj <- addPlots(testStudyObj, plots)
testModelName <- names(testStudyObj[["models"]])[1]

tmplib <- tempfile()
dir.create(tmplib)
libOrig <- .libPaths()
.libPaths(c(tmplib, libOrig))
suppressMessages(OmicAnalyzer::installStudy(testStudyObj))

expect_silent(
  plotStudy(testStudyObj, modelID = "model_01", feature = "feature_0001", plotID = "plotBase")
)

expect_silent(
  plotStudy(testStudyObj, modelID = "model_01", feature = "feature_0001", plotID = "plotGg")
)

expect_error(
  plotStudy(testStudyObj, modelID = "model_01", feature = "feature_0001", plotID = "non-existent"),
  "non-existent"
)

expect_error(
  plotStudy(testStudyObj, modelID = "model_01", feature = "non-existent", plotID = "plotBase"),
  "non-existent"
)

expect_silent(
  plotStudy(testStudyName, modelID = "model_01", feature = "feature_0001", plotID = "plotBase")
)

expect_silent(
  plotStudy(testStudyName, modelID = "model_01", feature = "feature_0001", plotID = "plotGg")
)

expect_error(
  plotStudy(testStudyName, modelID = "model_01", feature = "feature_0001", plotID = "non-existent"),
  "non-existent"
)

expect_error(
  plotStudy(testStudyName, modelID = "model_01", feature = "non-existent", plotID = "plotBase"),
  "non-existent"
)
