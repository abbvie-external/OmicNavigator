# Test checkX() methods

# Setup ------------------------------------------------------------------------
# source(paste0(getwd(), "/inst/tinytest/tinytestSettings.R"))
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

# checkMaintainer -----------------------------------------------------------------

expect_silent_xl(
  createStudy(name = "ok", maintainer = "my name")
)

expect_error_xl(
  createStudy(name = "error", maintainer = TRUE)
)

expect_error_xl(
  createStudy(name = "error", maintainer = c("my", "name"))
)

# checkMaintainerEmail ---------------------------------------------------------

expect_silent_xl(
  createStudy(name = "ok", maintainerEmail = "me@email.com")
)

expect_silent_xl(
  createStudy(name = "ok", maintainerEmail = "@me@email.com")
)

expect_silent_xl(
  createStudy(name = "ok", maintainerEmail = "me@me@email.com"),
  info = "The local-part can contain '@'"
)

expect_error_xl(
  createStudy(name = "error", maintainerEmail = TRUE)
)

expect_error_xl(
  createStudy(name = "error", maintainerEmail = c("my", "name"))
)

expect_error_xl(
  createStudy(name = "error", maintainerEmail = "me_email.com"),
  "A valid email address should contain at least one '@'"
)

expect_error_xl(
  createStudy(name = "error", maintainerEmail = "me@"),
  "Invalid maintainer email"
)

expect_error_xl(
  createStudy(name = "error", maintainerEmail = "@email.com"),
  "Invalid maintainer email"
)

expect_error_xl(
  createStudy(name = "error", maintainerEmail = "@email.com@"),
  "Invalid maintainer email"
)

# checkStudyMeta ---------------------------------------------------------------

expect_silent_xl(
  createStudy(name = "ok", studyMeta = list(a = "a"))
)

expect_error_xl(
  createStudy(name = "error", studyMeta = list("a space" = "a")),
  "cannot contain whitespace"
)

expect_error_xl(
  createStudy(name = "error", studyMeta = list("a:" = "a")),
  "cannot contain colons"
)

expect_error_xl(
  createStudy(name = "error", studyMeta = list("#a" = "a")),
  "cannot start with a comment character"
)

expect_error_xl(
  createStudy(name = "error", studyMeta = list("-a" = "a")),
  "cannot start with a dash"
)

expect_error_xl(
  createStudy(name = "error", studyMeta = list(a = c("a", "b"))),
  "single values"
)

expect_error_xl(
  createStudy(name = "error", studyMeta = list(Author = "A Name")),
  "reserved fields for R's DESCRIPTION file",
  info = "R's DESCRIPTION fields are not allowed"
)

expect_silent_xl(
  createStudy(name = "ok", studyMeta = list(author = "A Name")),
  info = "Allowed to use lowercase versions of R's DESCRIPTION fields"
)

expect_error_xl(
  createStudy(name = "error", studyMeta = list("Classification/ACM" = "topic")),
  "reserved fields for R's DESCRIPTION file",
  info = "R's DESCRIPTION fields are not allowed"
)

# checkFeatures ----------------------------------------------------------------

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

featuresMissing <-  list(
  default = data.frame(
    x = c("a", NA, "c"),
    y = letters[1:3],
    stringsAsFactors = FALSE
  )
)

expect_error_xl(
  addFeatures(study, features = featuresMissing),
  "missing values",
  info = "A single missing value would still be unique. Error if it is found"
)

# checkSamples -----------------------------------------------------------------

expect_error_xl(
  addSamples(study, samples = NULL)
)

samplesMissing <-  list(
  default = data.frame(
    x = c("a", NA, "c"),
    y = letters[1:3],
    stringsAsFactors = FALSE
  )
)

expect_error_xl(
  addSamples(study, samples = samplesMissing),
  "missing values",
  info = "A single missing value would still be unique. Error if it is found"
)

# checkModels ------------------------------------------------------------------

expect_error_xl(
  addModels(study, models = NULL)
)

expect_silent_xl(
  addModels(study, models = list(model_01 = "tooltip")),
  info = "addModels() accepts a single string per modelID"
)

expect_silent_xl(
  addModels(study, models = list(model_01 = list(description = "tooltip"))),
  info = "addModels() accepts a named list per modelID"
)

expect_error_xl(
  addModels(study, models = list(model_01 = list("tooltip"))),
  "must be named",
  info = "The list elements must be named"
)

expect_error_xl(
  addModels(study, models = list(model_01 = data.frame(a = 1))),
  "must be a list, not a data frame"
)

# checkAssays ------------------------------------------------------------------

expect_error_xl(
  addAssays(study, assays = NULL)
)

assaysWithNonNumeric <- list(
  default = data.frame(
    one = letters,
    two = 1:26
  )
)

expect_error_xl(
  addAssays(study, assays = assaysWithNonNumeric)
)

# checkTests -------------------------------------------------------------------

expect_error_xl(
  addTests(study, tests = NULL)
)

expect_silent_xl(
  addTests(study, tests = list(model_01 = list(test_01 = "tooltip"))),
  info = "addTests() accepts a single string per testID"
)

expect_silent_xl(
  addTests(study, tests = list(model_01 = list(test_01 = list(description = "tooltip")))),
  info = "addTests() accepts a named list per testID"
)

expect_error_xl(
  addTests(study, tests = list(model_01 = list(test_01 = list("tooltip")))),
  "must be named",
  info = "The list elements must be named"
)

expect_error_xl(
  addTests(study, tests = list(model_01 = list(test_01 = data.frame(a = 1)))),
  "must be a list, not a data frame"
)

# checkAnnotations -------------------------------------------------------------

expect_error_xl(
  addAnnotations(study, annotations = NULL)
)

# checkResults -----------------------------------------------------------------

expect_error_xl(
  addResults(study, results = NULL)
)

resultsMissing <-  list(
  m1 = list(
    t1 = data.frame(
      x = c("a", NA, "c"),
      y = rnorm(3),
      stringsAsFactors = FALSE
    )
  )
)

expect_error_xl(
  addResults(study, results = resultsMissing),
  "missing values",
  info = "A single missing value would still be unique. Error if it is found"
)

resultsDefault <- list(
  default = list(
    t1 = data.frame(
      x = c("a", "b", "c"),
      y = rnorm(3),
      stringsAsFactors = FALSE
    )
  )
)

expect_error_xl(
  addResults(study, results = resultsDefault),
  'The results cannot be shared using the modelID \"default\"'
)

# checkEnrichments -------------------------------------------------------------

expect_error_xl(
  addEnrichments(study, enrichments = NULL)
)

enrichmentDefault <- list(
  default = list(
    t1 = NULL
  )
)

expect_error_xl(
  addEnrichments(study, enrichments = enrichmentDefault),
  "The enrichments cannot be shared using the modelID \"default\""
)

enrichment_numTest <- OmicNavigator:::testEnrichments()
enrichment_numTest$model_01$annotation_01$test_01$nominal <-
  as.character(enrichment_numTest$model_01$annotation_01$test_01$nominal)

expect_error_xl(
  addEnrichments(study, enrichments = enrichment_numTest),
  "Column 'nominal' from enrichments must be numeric"
)

enrichment_numTest <- OmicNavigator:::testEnrichments()
enrichment_numTest$model_01$annotation_01$test_01$adjusted <-
  as.character(enrichment_numTest$model_01$annotation_01$test_01$adjusted)

expect_error_xl(
  addEnrichments(study, enrichments = enrichment_numTest),
  "Column 'adjusted' from enrichments must be numeric"
)

# checkMetaFeatures ------------------------------------------------------------

expect_error_xl(
  addMetaFeatures(study, metaFeatures = NULL)
)

expect_warning_xl(
  addMetaFeatures(study, metaFeatures = nonCharacterFeatures),
  ".+non-character.+x.+z"
)

# checkPlots -------------------------------------------------------------------

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

# Functions cannot be named the same as any functions in package:base. Otherwise
# getPlotFunction() returns the function in package:base instead of the user
# defined function.

# base::sign()
expect_error_xl(
  addPlots(study, plots = list(default = list(sign = list(displayName = "a plot named sign")))),
  "package:base"
)

# base::plot() was added to base in R 4.0.0
if (getRversion() > "4") {
  expect_error_xl(
    addPlots(study, plots = list(default = list(plot = list(displayName = "a plot named plot")))),
    "package:base"
  )
}

# checkMapping -----------------------------------------------------------------

expect_error_xl(
  addMapping(study, mapping = NULL)
)

tempMapping <- list(data.frame(model_01 = c("feature_01", "feature_02"),
                               model_02 = c("feature_01", "feature_02"),
                               stringsAsFactors = FALSE))
names(tempMapping) <- "default"
expect_silent_xl(
  addMapping(study, mapping = tempMapping)
)

tempMapping <- list(data.frame(model_01 = c("feature_01", "feature_02"),
                               model_02 = c("feature_01", NA),
                               stringsAsFactors = FALSE))
names(tempMapping) <- "default"
expect_silent_xl(
  addMapping(study, mapping = tempMapping)
)

# check mapping with no named list
tempMapping <- list(data.frame(model_01 = c("feature_01", "feature_02"),
                               model_02 = c("feature_01", "feature_02"),
                               stringsAsFactors = FALSE))
expect_error_xl(
  addMapping(study, mapping = tempMapping),
  "The elements of list \"mapping\" must be named"
)

# check mapping with one model having only NAs
tempMapping <- list(data.frame(model_01 = c("feature_01", "feature_02"),
                               model_02 = c(NA, NA),
                               stringsAsFactors = FALSE))
names(tempMapping) <- "default"
expect_error_xl(
  addMapping(study, mapping = tempMapping),
  "mapping object requires at least one feature per model"
)

# check mapping with one single element
tempMapping <- list(data.frame(model_01 = c("feature_01", "feature_02"),
                               stringsAsFactors = FALSE))
names(tempMapping) <- "default"
expect_error_xl(
  addMapping(study, mapping = tempMapping),
  "mapping object requires at least two models and one feature"
)

# check mapping features that do not match across models
tempMapping <- list(data.frame(model_01 = c("feature_01", "feature_02", NA, NA),
                               model_02 = c(NA, NA, "feature_05", "feature_06"),
                               stringsAsFactors = FALSE))
names(tempMapping) <- "default"
expect_error_xl(
  addMapping(study, mapping = tempMapping),
  "does not present any feature mapped to another model"
)

# checkBarcodes ----------------------------------------------------------------

expect_error_xl(
  addBarcodes(study, barcodes = NULL)
)

# checkReports -----------------------------------------------------------------

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

# checkResultsLinkouts ---------------------------------------------------------

expect_error_xl(
  addResultsLinkouts(study, resultsLinkouts = NULL)
)

# checkEnrichmentsLinkouts -----------------------------------------------------

expect_error_xl(
  addEnrichmentsLinkouts(study, enrichmentsLinkouts = NULL)
)

# checkMetaFeaturesLinkouts ----------------------------------------------------

expect_error_xl(
  addMetaFeaturesLinkouts(study, metaFeaturesLinkouts = NULL)
)

