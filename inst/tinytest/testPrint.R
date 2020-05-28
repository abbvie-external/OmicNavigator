library(OmicAnalyzer)
library(tinytest)

emptyStudy <- createStudy(name = "empty", description = "An empty study")
testStudy <- OmicAnalyzer:::testStudy(name = "test", description = "A test study")
testStudy <- addPlots(testStudy, OmicAnalyzer:::testPlots())

concatenateOutput <- function(x) {
  output <- utils::capture.output(print(x))
  output <- paste(output, collapse = "\n")
  return(output)
}

expect_stdout(
  concatenateOutput(emptyStudy)
)

expect_stdout(
  concatenateOutput(testStudy)
)
