#' Create a study
#'
#' @param name Name of the study
#' @param description Description of the study
#' @param featureID The column name that contains the unique identifiers for the
#'   features measured in the study
#' @param sampleID The column name that contains the unique identifiers for the
#'   samples measured in the study
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
                        samples = NULL,
                        features = NULL,
                        models = NULL,
                        assays = NULL,
                        tests = NULL,
                        annotations = NULL,
                        results = NULL,
                        enrichments = NULL,
                        metaFeatures = NULL,
                        plots = NULL,
                        featureID = "featureID",
                        sampleID = "sampleID",
                        version = NULL)
{
  stopifnot(is.character(name), is.character(description))

  study <- list(name = name,
                description = description,
                samples = NULL,
                features = NULL,
                models = NULL,
                assays = NULL,
                tests = NULL,
                annotations = NULL,
                results = NULL,
                enrichments = NULL,
                metaFeatures = NULL,
                plots = NULL,
                featureID = featureID,
                sampleID = sampleID,
                version = version)
  class(study) <- "oaStudy"

  if (!is.null(samples)) study <- addSamples(study, samples = samples)
  if (!is.null(features)) study <- addFeatures(study, features = features)
  if (!is.null(models)) study <- addModels(study, models = models)
  if (!is.null(assays)) study <- addAssays(study, assays = assays)
  if (!is.null(tests)) study <- addTests(study, tests = tests)
  if (!is.null(annotations)) study <- addAnnotations(study, annotations = annotations)
  if (!is.null(results)) study <- addResults(study, results = results)
  if (!is.null(enrichments)) study <- addEnrichments(study, enrichments = enrichments)
  if (!is.null(metaFeatures)) study <- addMetaFeatures(study, metaFeatures = metaFeatures)
  if (!is.null(plots)) study <- addPlots(study, plots = plots)

  return(study)
}

#' Add sample metadata
#'
#' @param samples The metadata variables that describe the samples in the study.
#'   The input object can be a single data frame or a list of data frames (one
#'   per model). The table must contain the unique sampleID used for the study.
#' @inheritParams shared-add
#'
#' @export
addSamples <- function(study, samples, overwrite = FALSE) {
  checkStudy(study)
  checkSamples(samples, study)

  if (overwrite || is.null(study[["samples"]])) {
    study[["samples"]] <- samples
    # class(study[["samples"]]) <- "oaSamples"
  } else {
    stop("Sample metadata already exists. Set overwrite=TRUE to overwrite.")
  }

  return(study)
}

#' Add feature metadata
#'
#' @param features A table of metadata variables that describe the features in the
#'   study. The table must contain the unique featureID used for the study. Also,
#'   the object must inherit from the class data.frame.
#' @inheritParams shared-add
#'
#' @export
addFeatures <- function(study, features, overwrite = FALSE) {
  checkStudy(study)
  checkFeatures(features, study)

  if (overwrite || is.null(study[["features"]])) {
    study[["features"]] <- features
  } else {
    stop("Feature metadata already exists. Set overwrite=TRUE to overwrite.")
  }

  return(study)
}

#' Add models
#'
#' @param models The models analyzed in the study. The input is a named
#'   list. The names correspond to the names of the models. The
#'   elements correspond to the descriptions of the models.
#' @inheritParams shared-add
#'
#' @export
addModels <- function(study, models, overwrite = FALSE) {
  checkStudy(study)
  checkModels(models, study)

  if (overwrite || is.null(study[["models"]])) {
    study[["models"]] <- models
  } else {
    stop("Models metadata already exists. Set overwrite=TRUE to overwrite.")
  }

  return(study)
}

#' Add assays
#'
#' @param assays The assays from the study. The input is a named list. The names
#'   of the list correspond to the model names. Each element in the list should
#'   be a matrix of quantifications for the assays. The column names should be
#'   the sample IDs and the rows should be the feature IDs.
#' @inheritParams shared-add
#'
#' @export
addAssays <- function(study, assays, overwrite = FALSE) {
  checkStudy(study)
  checkAssays(assays, study)

  if (overwrite || is.null(study[["assays"]])) {
    study[["assays"]] <- assays
  } else {
    stop("assays metadata already exists. Set overwrite=TRUE to overwrite.")
  }

  return(study)
}

#' Add tests
#'
#' @param tests The tests tested by the model(s). A named list.
#'   The names correspond to the name of each test. The elements
#'   correspond to the description of each test.
#' @inheritParams shared-add
#'
#' @export
addTests <- function(study, tests, overwrite = FALSE) {
  checkStudy(study)
  checkTests(tests, study)

  if (overwrite || is.null(study[["tests"]])) {
    study[["tests"]] <- tests
  } else {
    stop("The tests already exist. Set overwrite=TRUE to overwrite.")
  }

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
#'   features table that was used for the enrichment analysis (if omitted, it is
#'   assumed to be the main featureID used for the study), and 3) \code{terms},
#'   a list of annotation terms. The names of \code{terms} sublist correspond to
#'   the name of the annotation terms. Each of the annotation terms should be a
#'   character vector of feature IDs.
#' @inheritParams shared-add
#'
#' @export
addAnnotations <- function(study, annotations, overwrite = FALSE) {
  checkStudy(study)
  checkAnnotations(annotations, study)

  if (overwrite || is.null(study[["annotations"]])) {
    study[["annotations"]] <- annotations
  } else {
    stop("The annotations already exist. Set overwrite=TRUE to overwrite.")
  }

  return(study)
}

#' Add inference results
#'
#' @param results The inference results from each model. The input is a
#'   nested named list. The names of the list correspond to the model names.
#'   Each element in the list should be a list of data frames with inference
#'   results, one for each test. The featureID column needs to be included
#'   in each table.
#' @inheritParams shared-add
#'
#' @export
addResults <- function(study, results, overwrite = FALSE) {
  checkStudy(study)
  checkResults(results, study)

  if (overwrite || is.null(study[["results"]])) {
    study[["results"]] <- results
  } else {
    stop("The result results already exist. Set overwrite=TRUE to overwrite.")
  }

  return(study)
}

#' Add enrichment results
#'
#' @param enrichments The enrichment results from each model. The input is a
#'   nested named list. The names of the list correspond to the model names.
#'   Each list element should be a list of the tests tested. The names
#'   correspond to the test names. Each list element should be another list of
#'   annotation databases. The names correspond to the annotation databases.
#'   Each of these elements should be a data frame with enrichment results. Each
#'   table must contain the following columns: "termID", "description",
#'   "nominal" (the nominal statistics), and "adjusted" (the statistics after
#'   adjusting for multiple testing). Any additional columns are ignored.
#' @inheritParams shared-add
#'
#' @export
addEnrichments <- function(study, enrichments, overwrite = FALSE) {
  checkStudy(study)
  checkEnrichments(enrichments, study)

  if (overwrite || is.null(study[["enrichments"]])) {
    study[["enrichments"]] <- enrichments
  } else {
    stop("The enrichment results already exist. Set overwrite=TRUE to overwrite.")
  }

  return(study)
}

#' Add meta-feature metadata
#'
#' @param metaFeatures A table of metadata variables that describe the
#'   meta-features in the study. This is useful anytime there are metadata
#'   variables that cannot be mapped 1:1 to your features. For example, a
#'   peptide may be associated with multiple proteins. The table must contain
#'   the unique featureID used for the study. Also, the object must inherit from
#'   the class data.frame.
#' @inheritParams shared-add
#'
#' @export
addMetaFeatures <- function(study, metaFeatures, overwrite = FALSE) {
  checkStudy(study)
  checkMetaFeatures(metaFeatures, study)

  if (overwrite || is.null(study[["metaFeatures"]])) {
    study[["metaFeatures"]] <- metaFeatures
  } else {
    stop("Feature metadata already exists. Set overwrite=TRUE to overwrite.")
  }

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
#' @param plots Custom plotting functions. The input is a nested list. Each
#'   element of the list defines a custom plotting function via a list with
#'   multiple options. The only required option is \code{definition}, which
#'   contains the function definition. You can optionally include
#'   \code{displayName} to control how the plot will be named in the app.
#'   Lastly, if the plottting function requires external packages, these can be
#'   defined in the argument \code{packages}.
#' @inheritParams shared-add
#'
#' @export
addPlots <- function(study, plots, overwrite = FALSE) {
  checkStudy(study)
  checkPlots(plots, study)

  if (overwrite || is.null(study[["plots"]])) {
    study[["plots"]] <- plots
  } else {
    stop("The plots already exist. Set overwrite=TRUE to overwrite.")
  }

  return(study)
}
