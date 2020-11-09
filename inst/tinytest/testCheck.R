# Test checkX() methods

# Setup ------------------------------------------------------------------------

source("tinytestSettings.R")
using(ttdo)

library(OmicNavigator)

study <- createStudy(name = "test")

# checkStudy -------------------------------------------------------------------

invalidStudy <- list(nameIncorrect = "incorrect")
class(invalidStudy) <- "onStudy"

expect_error(
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

expect_error(
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

expect_error(
  addSamples(study, samples = NULL)
)

expect_error(
  addModels(study, models = NULL)
)

expect_error(
  addAssays(study, assays = NULL)
)

expect_error(
  addTests(study, tests = NULL)
)

expect_error(
  addAnnotations(study, annotations = NULL)
)

expect_error(
  addResults(study, results = NULL)
)

expect_error(
  addEnrichments(study, enrichments = NULL)
)

expect_error(
  addMetaFeatures(study, metaFeatures = NULL)
)

expect_warning_xl(
  addMetaFeatures(study, metaFeatures = nonCharacterFeatures),
  ".+non-character.+x.+z"
)

expect_error(
  addPlots(study, plots = NULL)
)

expect_error(
  addBarcodes(study, barcodes = NULL)
)

expect_error(
  addReports(study, reports = NULL)
)

expect_error(
  addReports(study, reports = list(modelID = list("https://www.domain.com/report.html"))),
  "is.character"
)

expect_error(
  addReports(study, reports = list(modelID = c("https://www.domain.com/report1.html",
                                               "https://www.domain.com/report2.html"))),
  "length"
)

expect_error(
  addReports(study, reports = list(modelID = "C:/path/to/non-existent/file")),
  "Report must be a URL or a path to an existing file"
)

expect_error(
  addResultsLinkouts(study, resultsLinkouts = NULL)
)

expect_error(
  addEnrichmentsLinkouts(study, enrichmentsLinkouts = NULL)
)
