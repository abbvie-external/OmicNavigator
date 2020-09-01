# Test validation step

# Setup ------------------------------------------------------------------------

source("tinytestSettings.R")
using(ttdo)

library(OmicAnalyzer)

testStudyName <- "ABC"
testStudyObj <- OmicAnalyzer:::testStudy(name = testStudyName, version = "0.3")

# Results ----------------------------------------------------------------------

expect_true(validateStudy(testStudyObj))

invalidResults <- testStudyObj
colnames(invalidResults[["results"]][[1]][[1]])[1] <- "wrongFeatureID"

expect_error_xl(
  validateStudy(invalidResults),
  "Name of features column doesn't match between results and features tables"
)
