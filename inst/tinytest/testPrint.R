# Test print.onStudy()

# Setup ------------------------------------------------------------------------

source("tinytestSettings.R")
using(ttdo)

library(OmicNavigator)

emptyStudyObj <- createStudy(name = "empty", description = "An empty study")
testStudyObj <- OmicNavigator:::testStudy(name = "test", description = "A test study")
testStudyPlots <- addPlots(testStudyObj, OmicNavigator:::testPlots())

# Test print.onStudy() ---------------------------------------------------------

expect_stdout(
  print(emptyStudyObj),
  "0 models"
)

expect_stdout(
  print(testStudyObj),
  "3 models"
)

expect_stdout(
  print(testStudyPlots),
  "3 annotations"
)
