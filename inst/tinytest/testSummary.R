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
  concatenateOutput(summary(emptyStudy))
)

expect_stdout(
  concatenateOutput(summary(testStudy))
)

expect_stdout(
  concatenateOutput(summary(testStudyPlots))
)

expect_stdout(
  concatenateOutput(summary(testStudyPlots, elements = c("annotations", "enrichments")))
)
