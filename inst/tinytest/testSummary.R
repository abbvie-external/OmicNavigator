# Test summary.onStudy()

# Setup ------------------------------------------------------------------------

source("tinytestSettings.R")
using(ttdo)

library(OmicNavigator)

emptyStudy <- createStudy(name = "empty", description = "An empty study")
testStudy <- OmicNavigator:::testStudy(name = "test", description = "A test study")
testStudyPlots <- addPlots(testStudy, OmicNavigator:::testPlots())

# Test summary.onStudy() -------------------------------------------------------

expect_stdout(
  summary(emptyStudy),
  "empty"
)

expect_stdout(
  summary(testStudy),
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
