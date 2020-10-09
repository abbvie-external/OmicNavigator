# Test print.oaStudy()

# Setup ------------------------------------------------------------------------

source("tinytestSettings.R")
using(ttdo)

library(OmicAnalyzer)

emptyStudy <- createStudy(name = "empty", description = "An empty study")
testStudy <- OmicAnalyzer:::testStudy(name = "test", description = "A test study")
testStudyPlots <- addPlots(testStudy, OmicAnalyzer:::testPlots())

# Test print.oaStudy() ---------------------------------------------------------

expect_stdout(
  print(emptyStudy),
  "0 models"
)

expect_stdout(
  print(testStudy),
  "3 models"
)

expect_stdout(
  print(testStudyPlots),
  "3 annotations"
)
