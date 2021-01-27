# Test checkX() methods

# Setup ------------------------------------------------------------------------

source("tinytestSettings.R")
using(ttdo)

library(OmicNavigator)

study <- createStudy(name = "test")

# checkStudy -------------------------------------------------------------------

invalidStudy <- list(nameIncorrect = "incorrect")
class(invalidStudy) <- "onStudy"

expect_error_xl(
  addFeatures(invalidStudy, features = data.frame()),
  "name"
)

# checkName --------------------------------------------------------------------

expect_error_xl(
  createStudy(name = TRUE)
)

expect_error_xl(
  createStudy(name = c("one", "two"))
)

expect_error_xl(
  createStudy(name = ""),
  "Invalid name for a study package"
)

expect_error_xl(
  createStudy(name = ".invalid"),
  "Invalid name for a study package"
)

expect_error_xl(
  createStudy(name = "invalid."),
  "Invalid name for a study package"
)
expect_error_xl(
  createStudy(name = "0invalid"),
  "Invalid name for a study package"
)

expect_error_xl(
  createStudy(name = "in-valid"),
  "Invalid name for a study package"
)

# checkDescription -------------------------------------------------------------

expect_error_xl(
  createStudy(name = "ok", description = TRUE)
)

expect_error_xl(
  createStudy(name = "ok", description = c("one", "two"))
)

# checkVersion -----------------------------------------------------------------

expect_error_xl(
  createStudy(name = "ok", version = TRUE)
)

expect_error_xl(
  createStudy(name = "ok", version = 1)
)

expect_error_xl(
  createStudy(name = "ok", version = c("one", "two"))
)

expect_error_xl(
  createStudy(name = "ok", version = "1"),
  "Invalid version for a study package"
)

expect_error_xl(
  createStudy(name = "ok", version = "1/1"),
  "Invalid version for a study package"
)

expect_error_xl(
  createStudy(name = "ok", version = "1a-1"),
  "Invalid version for a study package"
)

expect_error_xl(
  createStudy(name = "ok", version = "1.1-"),
  "Invalid version for a study package"
)

expect_error_xl(
  createStudy(name = "ok", version = "-1.1"),
  "Invalid version for a study package"
)

# checkX -----------------------------------------------------------------------

expect_error_xl(
  addFeatures(study, features = NULL)
)

# Confirm warning if non-character columns passed in features table
nonCharacterFeatures <- list(
  default = data.frame(
    x = 1:3,
    y = letters[1:3],
    z = as.factor(1:3),
    stringsAsFactors = FALSE
  )
)
expect_warning_xl(
  addFeatures(study, features = nonCharacterFeatures),
  ".+non-character.+x.+z"
)

expect_error_xl(
  addSamples(study, samples = NULL)
)

expect_error_xl(
  addModels(study, models = NULL)
)

expect_error_xl(
  addAssays(study, assays = NULL)
)

expect_error_xl(
  addTests(study, tests = NULL)
)

expect_error_xl(
  addAnnotations(study, annotations = NULL)
)

expect_error_xl(
  addResults(study, results = NULL)
)

expect_error_xl(
  addEnrichments(study, enrichments = NULL)
)

expect_error_xl(
  addMetaFeatures(study, metaFeatures = NULL)
)

expect_warning_xl(
  addMetaFeatures(study, metaFeatures = nonCharacterFeatures),
  ".+non-character.+x.+z"
)

expect_error_xl(
  addPlots(study, plots = NULL)
)

functionWithNoArgs <- function() {}
plots = list(
  default = list(
    functionWithNoArgs = list(
      displayName = "error"
    )
  )
)
expect_error_xl(
  addPlots(study, plots = plots),
  "has no arguments",
  info = "Custom plotting functions are required to have at least one argument"
)
rm(functionWithNoArgs)

functionWithNonDefaultArgs <- function(w, x = 1, y, z = 2) {}
plots = list(
  default = list(
    functionWithNonDefaultArgs = list(
      displayName = "error"
    )
  )
)
expect_error_xl(
  addPlots(study, plots = plots),
  "Only the first argument can be a required argument",
  info = "Custom plotting functions can only have the first argument be required"
)
rm(functionWithNonDefaultArgs)

# While not encouraged, as long as plotStudy() can pass the data to the first
# argument, custom plotting functions can have other arguments with default
# values, even the first one.
functionUnusualButValid <- function(w = 1, x = 2, y = 3, z = 4) {}
plots = list(
  default = list(
    functionUnusualButValid = list(
      displayName = "error"
    )
  )
)
expect_silent_xl(
  addPlots(study, plots = plots)
)
rm(functionUnusualButValid)

expect_error_xl(
  addBarcodes(study, barcodes = NULL)
)

expect_error_xl(
  addReports(study, reports = NULL)
)

expect_error_xl(
  addReports(study, reports = list(modelID = list("https://www.domain.com/report.html"))),
  "is.character"
)

expect_error_xl(
  addReports(study, reports = list(modelID = c("https://www.domain.com/report1.html",
                                               "https://www.domain.com/report2.html"))),
  "length"
)

expect_error_xl(
  addReports(study, reports = list(modelID = "C:/path/to/non-existent/file")),
  "Report must be a URL or a path to an existing file"
)

expect_error_xl(
  addResultsLinkouts(study, resultsLinkouts = NULL)
)

expect_error_xl(
  addEnrichmentsLinkouts(study, enrichmentsLinkouts = NULL)
)
