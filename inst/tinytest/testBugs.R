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
