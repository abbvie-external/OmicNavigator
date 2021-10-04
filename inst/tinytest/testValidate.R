# Test validation step

# Setup ------------------------------------------------------------------------

source("tinytestSettings.R")
using(ttdo)

library(OmicNavigator)

testStudyName <- "ABC"
testStudyObj <- OmicNavigator:::testStudy(name = testStudyName, version = "0.3")
testStudyObj <- addPlots(testStudyObj, OmicNavigator:::testPlots())
minimalStudyObj <- OmicNavigator:::testStudyMinimal()
emptyStudy <- createStudy(name = "empty", description = "An empty study")

# Results ----------------------------------------------------------------------

expect_true_xl(
  validateStudy(testStudyObj),
  info = "A valid study should pass"
)

expect_true_xl(
  validateStudy(minimalStudyObj),
  info = "A minimal study should pass"
)

expect_error_xl(
  validateStudy(emptyStudy),
  "No results",
  info = "A valid study requires at least one results table"
)

# Throw warning if no common columns across tests of a model
noCommonCols <- testStudyObj
colnames(noCommonCols[["results"]][[1]][[1]])[2:4] <- c("B0", "B1", "P")

expect_message_xl(
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

# This now sends a message instead of an error
expect_message_xl(
  validateStudy(missingFeatures),
  "Some of the features in the results table are missing from the featureID column in the features table"
)

# The features table shares multiple column names with the results table. It
# should only have the featureID column in common.
sharedColumnNames <- testStudyObj
sharedColumnNames[["features"]][[1]] <- cbind(
  sharedColumnNames[["features"]][[1]],
  beta = "a", p_val = "b",
  stringsAsFactors = FALSE)

expect_error_xl(
  validateStudy(sharedColumnNames),
  "The results and features tables can only have one shared column name"
)

expect_error_xl(
  validateStudy(sharedColumnNames),
  "modelID: model_01, testID: test_01\n"
)

expect_error_xl(
  validateStudy(sharedColumnNames),
  "Shared columns: beta, p_val"
)

# Assays -----------------------------------------------------------------------

# The row names are completely wrong
invalidAssaysRow <- testStudyObj
row.names(invalidAssaysRow[["assays"]][[1]]) <- NULL

expect_error_xl(
  validateStudy(invalidAssaysRow),
  "The featureID column in the results table does not match the row names of the assays table"
)

# Some of the row names are missing from the features table
invalidAssaysRowMissing <- testStudyObj
row.names(invalidAssaysRowMissing[["assays"]][[1]])[3] <- "wrongFeatureID"

expect_error_xl(
  validateStudy(invalidAssaysRowMissing),
  "Some of the featureIDs in the results table are missing from the row names of the assays table"
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

# This is ok now. Extra rows in the metaFeatures table won't affect anything in
# the app
expect_true_xl(
  validateStudy(invalidMetaFeatures)
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
expect_true_xl(validateStudy(resultsLinkoutsNoFeatures))

# Enrichments Linkouts ---------------------------------------------------------

# An enrichments linkout refers to a non-existing annotationID
invalidEnrichmentsLinkouts <- testStudyObj
names(invalidEnrichmentsLinkouts[["enrichmentsLinkouts"]])[1] <- "non-existent-annotationID"

expect_error_xl(
  validateStudy(invalidEnrichmentsLinkouts),
  "The annotationID \"non-existent-annotationID\" is not an available annotation"
)

# The annotationID referenced by the enrichmentLinkout must be included in the
# enrichments. However, it's not required to contain the extra metadata added by
# addAnnotations(). The latter is only required for the network view.
enrichmentWithoutAnnotation <- testStudyObj
enrichmentWithoutAnnotation[["annotations"]][["annotation_01"]] <- NULL

expect_silent_xl(
  validateStudy(enrichmentWithoutAnnotation)
)

# MetaFeatures Linkouts --------------------------------------------------------

# A metaFeatures linkout refers to a non-existing column in the metaFeatures table
invalidMetaFeaturesLinkouts <- testStudyObj
names(invalidMetaFeaturesLinkouts[["metaFeaturesLinkouts"]][["model_03"]]) <- "non-existent-column"

expect_error_xl(
  validateStudy(invalidMetaFeaturesLinkouts),
  "Invalid metaFeatures table linkout for modelID \"model_03\""
)

expect_error_xl(
  validateStudy(invalidMetaFeaturesLinkouts),
  "\"non-existent-column\" is not the name of an available metaFeature"
)

# Plots ------------------------------------------------------------------------

# A study no longer has to have assays and samples for custom plots since they
# may only be plotting columns from the results table

studyNoAssaysNoSamples <- testStudyObj
studyNoAssaysNoSamples[["assays"]] <- list()
studyNoAssaysNoSamples[["samples"]] <- list()

expect_true_xl(
  validateStudy(studyNoAssaysNoSamples),
  info = "Custom plots may only plot results data. Assays and samples not required"
)

# A study can also have assays but not samples
studyNoSamples <- testStudyObj
studyNoSamples[["samples"]] <- list()

expect_true_xl(
  validateStudy(studyNoSamples),
  info = "Samples not required to plot assays data"
)
