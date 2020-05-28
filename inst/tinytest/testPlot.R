library(OmicAnalyzer)
library(tinytest)

study <- OmicAnalyzer:::testStudy("testPlot")
plots <- OmicAnalyzer:::testPlots()
study <- addPlots(study, plots = plots)

expect_silent(
  plotStudy(study, modelID = "model_01", feature = "feature_0001", plotName = "plotBase")
)

expect_silent(
  plotStudy(study, modelID = "model_01", feature = "feature_0001", plotName = "plotGg")
)

expect_error(
  plotStudy(study, modelID = "model_01", feature = "feature_0001", plotName = "non-existent"),
  "non-existent"
)

expect_error(
  plotStudy(study, modelID = "model_01", feature = "non-existent", plotName = "plotBase"),
  "non-existent"
)
