# Test validation step

# Setup ------------------------------------------------------------------------

source("tinytestSettings.R")
using(ttdo)

library(OmicNavigator)

testStudyName <- "ABC"
testStudyObj <- OmicNavigator:::testStudy(name = testStudyName, version = "0.3")

# Results ----------------------------------------------------------------------

expect_true(validateStudy(testStudyObj))

# Throw warning if no common columns across tests of a model
noCommonCols <- testStudyObj
colnames(noCommonCols[["results"]][[1]][[1]])[2:4] <- c("B0", "B1", "P")

expect_warning_xl(
  validateStudy(noCommonCols),
  "The results tables for the tests of modelID"
)

# Invalid column name for featureID
invalidResults <- testStudyObj
colnames(invalidResults[["results"]][[1]][[1]])[1] <- "wrongFeatureID"

expect_error_xl(
  validateStudy(invalidResults),
  "Name of featureID column doesn't match between results and features tables"
)

# The features are completely wrong
invalidResultsFeatures <- testStudyObj
invalidResultsFeatures[["results"]][[1]][[1]][, 1] <- paste0("wrong", invalidResultsFeatures[["results"]][[1]][[1]][, 1])

expect_error_xl(
  validateStudy(invalidResultsFeatures),
  "The features in the results table do not match the featureID column in the features table"
)

# Some of the features are missing. Add an extra feature to results table that
# isn't present in features table.
missingFeatures <- testStudyObj
missingFeatures[["results"]][[1]][[1]] <- rbind(
  missingFeatures[["results"]][[1]][[1]][1, ],
  missingFeatures[["results"]][[1]][[1]]
)
missingFeatures[["results"]][[1]][[1]][1, 1] <- "missingInFeaturesTable"

expect_error_xl(
  validateStudy(missingFeatures),
  "Some of the features in the assays table are missing from the featureID column in the features table"
)

# Assays -----------------------------------------------------------------------

# The row names are completely wrong
invalidAssaysRow <- testStudyObj
row.names(invalidAssaysRow[["assays"]][[1]]) <- NULL

expect_error_xl(
  validateStudy(invalidAssaysRow),
  "The row names of the assays table do not match the featureID column in the features table"
)

# Some of the row names are missing from the features table
invalidAssaysRowMissing <- testStudyObj
row.names(invalidAssaysRowMissing[["assays"]][[1]])[3] <- "wrongFeatureID"

expect_error_xl(
  validateStudy(invalidAssaysRowMissing),
  "Some of the row names of the assays table are missing from the featureID column in the features table"
)

# The column names are completely wrong
invalidAssaysCol <- testStudyObj
colnames(invalidAssaysCol[["assays"]][[1]]) <- paste0("wrong", colnames(invalidAssaysCol[["assays"]][[1]]))

expect_error_xl(
  validateStudy(invalidAssaysCol),
  "The column names of the assays table do not match the sampleID column in the samples table"
)

# Some of the column names are missing from the samples table
invalidAssaysColMissing <- testStudyObj
colnames(invalidAssaysColMissing[["assays"]][[1]])[3] <- "wrongSampleID"

expect_error_xl(
  validateStudy(invalidAssaysColMissing),
  "Some of the column names of the assays table are missing from the sampleID column in the samples table"
)

# metaFeatures -----------------------------------------------------------------

# metaFeatures table contains a row with a featureID not in features table
invalidMetaFeatures <- testStudyObj
invalidMetaFeatures[["metaFeatures"]][[1]] <- rbind(
  invalidMetaFeatures[["metaFeatures"]][[1]][1, ],
  invalidMetaFeatures[["metaFeatures"]][[1]]
)
invalidMetaFeatures[["metaFeatures"]][[1]][1, 1] <- "missingInFeaturesTable"

expect_error_xl(
  validateStudy(invalidMetaFeatures),
  "It contains 1 row where the featureID is not available in the corresponding features table"
)

# Results Linkouts -------------------------------------------------------------

# A results linkout refers to a non-existing column in the features table
invalidResultsLinkouts <- testStudyObj
names(invalidResultsLinkouts[["resultsLinkouts"]][["model_03"]]) <- "non-existent-column"

expect_error_xl(
  validateStudy(invalidResultsLinkouts),
  "Invalid results table linkout for modelID \"model_03\""
)

expect_error_xl(
  validateStudy(invalidResultsLinkouts),
  "\"non-existent-column\" is not the name of an available feature"
)

# There is no features table, but the results linkouts only refer to featureID
# column, so this is fine.
resultsLinkoutsNoFeatures <- OmicNavigator:::testStudyMinimal()
resultsLinkouts <- list(
  model_01 = list(
    customID = "https://www.ncbi.nlm.nih.gov/gene/"
  )
)
resultsLinkoutsNoFeatures <- addResultsLinkouts(resultsLinkoutsNoFeatures, resultsLinkouts)
expect_true(validateStudy(resultsLinkoutsNoFeatures))

# Enrichments Linkouts ---------------------------------------------------------

# An enrichments linkout refers to a non-existing annotationID
invalidEnrichmentsLinkouts <- testStudyObj
names(invalidEnrichmentsLinkouts[["enrichmentsLinkouts"]])[1] <- "non-existent-annotationID"

expect_error_xl(
  validateStudy(invalidEnrichmentsLinkouts),
  "The annotationID \"non-existent-annotationID\" is not an available annotation\n"
)
