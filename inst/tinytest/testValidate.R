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

# Assays -----------------------------------------------------------------------

invalidAssaysRow <- testStudyObj
row.names(invalidAssaysRow[["assays"]][[1]])[3] <- "wrongFeatureID"

expect_error_xl(
  validateStudy(invalidAssaysRow),
  "Row names of assays do not match featureID in features table"
)

invalidAssaysCol <- testStudyObj
colnames(invalidAssaysCol[["assays"]][[1]])[3] <- "wrongSampleID"

expect_error_xl(
  validateStudy(invalidAssaysCol),
  "Column names of assays do not match sampleID in samples table"
)
