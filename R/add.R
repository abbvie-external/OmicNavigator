#' Create a study
#'
#' @param name Name of the study
#' @param description Description of the study
#' @param version (Optional) Include a version number to track the updates to
#'   your study package. If you export the study to a package, the version is
#'   used as the package version.
#' @inheritParams addSamples
#' @inheritParams addFeatures
#' @inheritParams addModels
#' @inheritParams addAssays
#' @inheritParams addTests
#' @inheritParams addAnnotations
#' @inheritParams addResults
#' @inheritParams addEnrichments
#' @inheritParams addMetaFeatures
#' @inheritParams addPlots
#'
#' @seealso
#'   \code{\link{addSamples}},
#'   \code{\link{addFeatures}},
#'   \code{\link{addModels}},
#'   \code{\link{addAssays}},
#'   \code{\link{addTests}},
#'   \code{\link{addAnnotations}},
#'   \code{\link{addResults}},
#'   \code{\link{addEnrichments}},
#'   \code{\link{addMetaFeatures}},
#'   \code{\link{addPlots}},
#'   \code{\link{exportStudy}},
#'   \code{\link{installStudy}}
#'
#' @examples
#'
#' study <- createStudy(name = "ABC",
#'                      description = "An analysis of ABC")
#'
#' @export
createStudy <- function(name,
                        description = name,
                        samples = list(),
                        features = list(),
                        models = list(),
                        assays = list(),
                        tests = list(),
                        annotations = list(),
                        results = list(),
                        enrichments = list(),
                        metaFeatures = list(),
                        plots = list(),
                        version = NULL)
{
  stopifnot(is.character(name), is.character(description))

  study <- list(name = name,
                description = description,
                samples = list(),
                features = list(),
                models = list(),
                assays = list(),
                tests = list(),
                annotations = list(),
                results = list(),
                enrichments = list(),
                metaFeatures = list(),
                plots = list(),
                version = version)
  class(study) <- "oaStudy"

  if (!isEmpty(samples)) study <- addSamples(study, samples = samples)
  if (!isEmpty(features)) study <- addFeatures(study, features = features)
  if (!isEmpty(models)) study <- addModels(study, models = models)
  if (!isEmpty(assays)) study <- addAssays(study, assays = assays)
  if (!isEmpty(tests)) study <- addTests(study, tests = tests)
  if (!isEmpty(annotations)) study <- addAnnotations(study, annotations = annotations)
  if (!isEmpty(results)) study <- addResults(study, results = results)
  if (!isEmpty(enrichments)) study <- addEnrichments(study, enrichments = enrichments)
  if (!isEmpty(metaFeatures)) study <- addMetaFeatures(study, metaFeatures = metaFeatures)
  if (!isEmpty(plots)) study <- addPlots(study, plots = plots)

  return(study)
}

#' Add sample metadata
#'
#' @param samples The metadata variables that describe the samples in the study.
#'   The input object is a named list of data frames (one per model). The first
#'   column of each data frame is used as the sample ID, so it must contain
#'   unique values. To share a data frame across multiple models, use the model
#'   ID "default".
#' @inheritParams shared-add
#'
#' @export
addSamples <- function(study, samples, overwrite = FALSE) {
  checkStudy(study)
  checkSamples(samples)

  study[["samples"]] <- addToList(study[["samples"]], samples, overwrite = overwrite)

  return(study)
}

#' Add feature metadata
#'
#' @param features The metadata variables that describe the features in the
#'   study. The input object is a list of data frames (one per model). The first
#'   column of each data frame is used as the feature ID, so it must contain
#'   unique values. To share a data frame across multiple models, use the model
#'   ID "default".
#' @inheritParams shared-add
#'
#' @export
addFeatures <- function(study, features, overwrite = FALSE) {
  checkStudy(study)
  checkFeatures(features)

  study[["features"]] <- addToList(study[["features"]], features, overwrite = overwrite)

  return(study)
}

#' Add models
#'
#' @param models The models analyzed in the study. The input is a named list.
#'   The names correspond to the names of the models. The elements correspond to
#'   the descriptions of the models.
#' @inheritParams shared-add
#'
#' @export
addModels <- function(study, models, overwrite = FALSE) {
  checkStudy(study)
  checkModels(models)

  study[["models"]] <- addToList(study[["models"]], models, overwrite = overwrite)

  return(study)
}

#' Add assays
#'
#' @param assays The assays from the study. The input object is a list of data
#'   frames (one per model). The row names should correspond to the feature IDs
#'   (\code{\link{addFeatures}}). The column names should corresond to the
#'   sample IDs (\code{\link{addSamples}}). The data frame should only contain
#'   numeric values. To share a data frame across multiple models, use the model
#'   ID "default".
#' @inheritParams shared-add
#'
#' @export
addAssays <- function(study, assays, overwrite = FALSE) {
  checkStudy(study)
  checkAssays(assays)

  study[["assays"]] <- addToList(study[["assays"]], assays, overwrite = overwrite)

  return(study)
}

#' Add tests
#'
#' @param tests The tests from the study. The input object is a list of data
#'   frames (one per model). The first column should contain the unique ID for
#'   each test. The second column should contain a description of the test. Any
#'   additional columns will be removed. The column names will be set to
#'   "testID" and "desription". To share a data frame across multiple models,
#'   use the model ID "default".
#' @inheritParams shared-add
#'
#' @export
addTests <- function(study, tests, overwrite = FALSE) {
  checkStudy(study)
  checkTests(tests)

  for (i in seq_along(tests)) {
    tests[[i]] <- tests[[i]][, 1:2]
    colnames(tests[[i]]) <- c("testID", "description")
  }

  study[["tests"]] <- addToList(study[["tests"]], tests, overwrite = overwrite)

  return(study)
}

#' Add annotations
#'
#' @param annotations The annotations used for the enrichment analyses. The
#'   input is a nested list. The top-level list contains one entry per
#'   annotation database, e.g. reactome. The names correspond to the name of
#'   each annotation database. Each of these elements should be list of that
#'   contains more information about each annotation database. Specifically the
#'   sublist should contain 1) \code{description}, a character vector that
#'   describes the resource, 2) \code{featureID}, the name of the column in the
#'   features table that was used for the enrichment analysis, and 3) \code{terms},
#'   a list of annotation terms. The names of \code{terms} sublist correspond to
#'   the name of the annotation terms. Each of the annotation terms should be a
#'   character vector of feature IDs.
#' @inheritParams shared-add
#'
#' @export
addAnnotations <- function(study, annotations, overwrite = FALSE) {
  checkStudy(study)
  checkAnnotations(annotations)

  study[["annotations"]] <- addToList(study[["annotations"]], annotations, overwrite = overwrite)

  return(study)
}

#' Add inference results
#'
#' @param results The inference results from each model. The input is a
#'   nested named list. The names of the list correspond to the model names.
#'   Each element in the list should be a list of data frames with inference
#'   results, one for each test. In each data frame, the feature ID must be in
#'   the first column, and all other columns must be numeric.
#' @inheritParams shared-add
#'
#' @export
addResults <- function(study, results, overwrite = FALSE) {
  checkStudy(study)
  checkResults(results)

  study[["results"]] <- addToList(study[["results"]], results, overwrite = overwrite)

  return(study)
}

#' Add enrichment results
#'
#' @param enrichments The enrichment results from each model. The input is a
#'   nested named list. The names of the list correspond to the model names.
#'   Each list element should be a list of the annotation databases tested
#'   (\code{\link{addAnnotations}}). The names of the list correspond to the
#'   annotation databases. Each list element should be another list of tests
#'   (\code{\link{addTests}}). The names correspond to the tests performed. Each
#'   of these elements should be a data frame with enrichment results. Each
#'   table must contain the following columns: "termID", "description",
#'   "nominal" (the nominal statistics), and "adjusted" (the statistics after
#'   adjusting for multiple testing). Any additional columns are ignored.
#' @inheritParams shared-add
#'
#' @export
addEnrichments <- function(study, enrichments, overwrite = FALSE) {
  checkStudy(study)
  checkEnrichments(enrichments)

  study[["enrichments"]] <- addToList(study[["enrichments"]], enrichments, overwrite = overwrite)

  return(study)
}

#' Add meta-feature metadata
#'
#' The meta-features table is useful anytime there are metadata variables that
#' cannot be mapped 1:1 to your features. For example, a peptide may be
#' associated with multiple proteins.
#'
#' @param metaFeatures The metadata variables that describe the meta-features in
#'   the study. The input object is a list of data frames (one per model). The
#'   first column of each data frame is used as the feature ID, so it must
#'   contain the same IDs as the corresponding features data frame
#'   (\code{\link{addFeatures}}). To share a data frame across multiple models,
#'   use the model ID "default".
#' @inheritParams shared-add
#'
#' @export
addMetaFeatures <- function(study, metaFeatures, overwrite = FALSE) {
  checkStudy(study)
  checkMetaFeatures(metaFeatures)

  study[["metaFeatures"]] <- addToList(study[["metaFeatures"]], metaFeatures, overwrite = overwrite)

  return(study)
}

#' Add custom plotting functions
#'
#' Include custom plots that the app will display when a feature is selected by
#' the user.
#'
#' All custom plotting functions are required to have the same function
#' signature. The first argument is always \code{x}, which will be a data frame
#' that combines the sample metadata with a column containing the assay
#' measurements for a specific feature. That column will always be named
#' \code{feature}. The second argument, \code{featureName}, is the unique ID of
#' the feature (idea: pass in the entire row from the features table so that the
#' user can decide what metadata to display). This is for labeling the plot.
#'
#' Note that any ggplot2 plots will require extra care. This is because the
#' plotting code will be inserted into a study package, and thus must follow the
#' \href{https://ggplot2.tidyverse.org/articles/ggplot2-in-packages.html#using-aes-and-vars-in-a-package-function-1}{best
#' practices for using ggplot2 within packages}. Specifically, when you refer to
#' columns of the data frame, e.g. \code{aes(x = group)}, you need to prefix it
#' with \code{.data$}, so that it becomes \code{aes(x = .data$group)}.
#' Fortunately this latter code will also run fine as you interactively develop
#' the function.
#'
#' @param plots Custom plotting functions for the study. The input object is a
#'   named list of lists (one per model). The names of the sublists should
#'   correspond to functions defined in the current R session. Each sublist must
#'   define a \code{displayName} to control how the plot will be named in the
#'   app. Optionally, if the plottting function requires external packages,
#'   these can be defined in the element \code{packages}. To share plots across
#'   multiple models, use the model ID "default".
#' @inheritParams shared-add
#'
#' @export
addPlots <- function(study, plots, overwrite = FALSE) {
  checkStudy(study)
  checkPlots(plots)

  study[["plots"]] <- addToList(study[["plots"]], plots, overwrite = overwrite)

  return(study)
}
