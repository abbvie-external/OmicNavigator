# Test summary.oaStudy()

# Setup ------------------------------------------------------------------------

source("tinytestSettings.R")
using(ttdo)

library(OmicAnalyzer)

emptyStudy <- createStudy(name = "empty", description = "An empty study")
testStudy <- OmicAnalyzer:::testStudy(name = "test", description = "A test study")
testStudyPlots <- addPlots(testStudy, OmicAnalyzer:::testPlots())

# Test summary.oaStudy() -------------------------------------------------------

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
