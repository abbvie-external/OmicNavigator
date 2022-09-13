# Test squashed bugs to make sure they don't reappear

# Setup ------------------------------------------------------------------------

source("tinytestSettings.R")
using(ttdo)

library(OmicNavigator)

# Invalid featureIDs in the assays table ---------------------------------------

# Make sure we are catching invalid featureIDs in the assays table
# https://github.com/abbvie-external/OmicNavigatorWebApp/issues/57

x <- createStudy("test")
assays <- OmicNavigator:::testAssays()
assays[[1]][["id"]] <- rownames(assays[[1]])

expect_error_xl(
  addAssays(x, assays),
  "The columns of the assays data frame must all be numeric.",
  info = "Catch non-numeric columns in assays table at the addAssays() step"
)

# Even if somehow the invalid assays table got past the addAssays() step, this
# should still be caught during the validation step

x <- OmicNavigator:::testStudy("test")
x[["assays"]][[1]][["id"]] <- rownames(x[["assays"]][[1]])

expect_error_xl(
  validateStudy(x),
  "The columns of the assays data frame must all be numeric.",
  info = "validateStudy() starts by running the checkX() funcs, so this is caught early"
)

x <- addPlots(x, OmicNavigator:::testPlots())

expect_error_xl(
  OmicNavigator:::validatePlots(x),
  "Some of the column names of the assays table are missing",
  info = "It doesn't like the new column name 'id'"
)

rm(x)

# Custom plots with no assays --------------------------------------------------

# Now that custom plots can include data from the results table, it's no longer
# required to have assays data.

studyNoAssays <- OmicNavigator:::testStudyMinimal()

plotBetas <- function(x) {
  plot(x[["results"]][["beta"]])
}

studyNoAssays <- addPlots(
  studyNoAssays,
  plots = list(
    default = list(
      plotBetas = list(
        displayName = "Plot the betas",
        plotType = "multiFeature"
      )
    )
  )
)

expect_message_xl(
  plotStudy(
    studyNoAssays,
    modelID = names(studyNoAssays[["results"]])[1],
    featureID = studyNoAssays[["results"]][[1]][[1]][[1]][1:5], # first 5 featureIDs
    plotID = "plotBetas",
    testID = names(studyNoAssays[["results"]][[1]])[1]
  ),
  "No assays available for modelID",
  info = "Send message for missing assays data when testID is defined"
)

expect_error_xl(
  plotStudy(
    studyNoAssays,
    modelID = names(studyNoAssays[["results"]])[1],
    featureID = studyNoAssays[["results"]][[1]][[1]][[1]][1:5], # first 5 featureIDs
    plotID = "plotBetas",
    testID = NULL
  ),
  "Add assays data with addAssays()",
  info = "Throw error for missing assays data when testID is **not** defined"
)

expect_true_xl(
  OmicNavigator:::validatePlots(studyNoAssays)
)

expect_message_xl(
  OmicNavigator:::validateStudy(studyNoAssays),
  "Custom plots often use assays"
)

rm(studyNoAssays, plotBetas)

# Results table with only 2 columns --------------------------------------------

# Have to set drop=FALSE in getUpsetCols() when the results table(s) only have 2
# columns total. This is because after the featureID column is removed, then the
# 1 remaining results column is converted to a vector. Then colnames() returns
# NULL (since it's a vector), and no common column names are found, thus
# disabling the set intersection features
#
# Creating an entire test study to confirm this well known error seems overkill
# (even for me). Here I simply confirm that drop=FALSE solves this type of issue

local({
  results <- list(
    test1 = data.frame(id = 1:3, stat = 1:3),
    test2 = data.frame(id = 1:3, stat = 1:3)
  )
  colsAll <- lapply(results, function(x) colnames(x[, -1, drop = FALSE]))
  colsCommon <- Reduce(intersect, colsAll)
  expect_identical_xl(
    colsCommon,
    "stat"
  )
})

# Empty sub-lists --------------------------------------------------------------

# Empty lists at the top-level are fine since this is the default when there is
# no data for that element. However, sub-lists in big nested lists like
# enrichments can cause problems. Because the loop is never entered, they are
# simply skipped by the checkX() functions. But when it comes time to export
# the study, these empty sub-lists can be mistakenly interpreted as containing
# data.

local({
  models <- OmicNavigator:::testModels()
  models[["empty"]] <- list()
  expect_error_xl(
    OmicNavigator:::checkModels(models),
    "An empty list is not allowed in this context"
  )

  tests <- OmicNavigator:::testTests()
  tests[["empty"]] <- list()
  expect_error_xl(
    OmicNavigator:::checkTests(tests),
    "An empty list is not allowed in this context"
  )

  annotations <- OmicNavigator:::testAnnotations()
  annotations[["empty"]] <- list()
  expect_error_xl(
    OmicNavigator:::checkAnnotations(annotations),
    "An empty list is not allowed in this context"
  )

  results <- OmicNavigator:::testResults()
  results[["empty"]] <- list()
  expect_error_xl(
    OmicNavigator:::checkResults(results),
    "An empty list is not allowed in this context"
  )

  enrichments <- OmicNavigator:::testEnrichments()
  enrichments[[1]][["empty"]] <- list()
  expect_error_xl(
    OmicNavigator:::checkEnrichments(enrichments),
    "An empty list is not allowed in this context"
  )

  plots <- OmicNavigator:::testPlots()
  plots[["empty"]] <- list()
  expect_error_xl(
    OmicNavigator:::checkPlots(plots),
    "An empty list is not allowed in this context"
  )

  barcodes <- OmicNavigator:::testBarcodes()
  barcodes[["empty"]] <- list()
  expect_error_xl(
    OmicNavigator:::checkBarcodes(barcodes),
    "An empty list is not allowed in this context"
  )

  resultsLinkouts <- OmicNavigator:::testResultsLinkouts()
  resultsLinkouts[["empty"]] <- list()
  expect_error_xl(
    OmicNavigator:::checkResultsLinkouts(resultsLinkouts),
    "An empty list is not allowed in this context"
  )

  metaFeaturesLinkouts <- OmicNavigator:::testMetaFeaturesLinkouts()
  metaFeaturesLinkouts[["empty"]] <- list()
  expect_error_xl(
    OmicNavigator:::checkMetaFeaturesLinkouts(metaFeaturesLinkouts),
    "An empty list is not allowed in this context"
  )
})

# Missing values in character columns ------------------------------------------

# Marco discovered in PR #11 that missing values in character columns are not
# preserved when reading from plain text files installed in study packages. This
# is because by default data.table::fwrite() writes missing values as empty
# strings (`na = ""`). When data.table::fread() reads the data, it converts
# empty strings in numeric columns to NA. However, empty strings in character
# columns are maintained as missing strings. I fixed this for future packages by
# explicitly setting `na = "NA"`. I also explicitly set fread()'s na.strings to
# c("NA", "") because it is set to change to only "" by default in a future
# version. The test below confirms that legacy study packages with empty strings
# can still be read. Also, since I included "" in na.strings, they are properly
# read as missing values (whereas before they wouldn't have been)

x <- data.frame(
  int = c(1:3, NA),
  num = c(1.2, -3.9, 10.0, NA),
  char = c(letters[1:3], NA),
  stringsAsFactors = FALSE
)
f <- tempfile()
data.table::fwrite(x, file = f, sep = "\t", quote = TRUE, na = "")

expect_equal_xl(
  OmicNavigator:::readTable(f),
  x,
  info = "Missing values are preserved when reading plain text files from study packages created prior to version 1.12.1"
)

