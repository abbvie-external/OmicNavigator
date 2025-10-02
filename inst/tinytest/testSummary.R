# Test summary.onStudy()

# Setup ------------------------------------------------------------------------

source("tinytestSettings.R")
using(ttdo)

library(OmicNavigator)

emptyStudyObj <- createStudy(name = "empty", description = "An empty study")
testStudyObj <- OmicNavigator:::testStudy(name = "test", description = "A test study")
testStudyPlots <- addPlots(testStudyObj, OmicNavigator:::testPlots())

# Test summary.onStudy() -------------------------------------------------------

expect_stdout(
  summary(emptyStudyObj),
  "empty"
)

expect_stdout(
  summary(testStudyObj),
  "\\|-reports \\(2\\)"
)

expect_stdout(
  summary(testStudyPlots),
  "\\|-plots \\(2\\)"
)

expect_stdout(
  summary(testStudyPlots, elements = c("annotations", "enrichments")),
  "annotations"
)
