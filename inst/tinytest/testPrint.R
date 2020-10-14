# Test print.onStudy()

# Setup ------------------------------------------------------------------------

source("tinytestSettings.R")
using(ttdo)

library(OmicNavigator)

emptyStudy <- createStudy(name = "empty", description = "An empty study")
testStudy <- OmicNavigator:::testStudy(name = "test", description = "A test study")
testStudyPlots <- addPlots(testStudy, OmicNavigator:::testPlots())

# Test print.onStudy() ---------------------------------------------------------

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
