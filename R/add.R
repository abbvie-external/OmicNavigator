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
#' @inheritParams addBarcodes
#' @inheritParams addReports
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
#'   \code{\link{addBarcodes}},
#'   \code{\link{addReports}},
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
                        barcodes = list(),
                        reports = list(),
                        version = NULL)
{
  checkName(name)
  checkDescription(description)
  checkVersion(version)

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
                barcodes = list(),
                reports = list(),
                overlaps = list(),
                version = version)
  class(study) <- "oaStudy"

  study <- addSamples(study, samples = samples)
  study <- addFeatures(study, features = features)
  study <- addModels(study, models = models)
  study <- addAssays(study, assays = assays)
  study <- addTests(study, tests = tests)
  study <- addAnnotations(study, annotations = annotations)
  study <- addResults(study, results = results)
  study <- addEnrichments(study, enrichments = enrichments)
  study <- addMetaFeatures(study, metaFeatures = metaFeatures)
  study <- addPlots(study, plots = plots)
  study <- addBarcodes(study, barcodes = barcodes)
  study <- addReports(study, reports = reports)

  return(study)
}

#' Shared parameters for add functions
#'
#' @name shared-add
#'
#' @param study An OmicNavigator study created with \code{\link{createStudy}}
#'
#' @keywords internal
NULL

#' Add sample metadata
#'
#' @param samples The metadata variables that describe the samples in the study.
#'   The input object is a named list of data frames (one per model). The first
#'   column of each data frame is used as the sampleID, so it must contain
#'   unique values. To share a data frame across multiple models, use the
#'   modelID "default".
#' @inheritParams shared-add
#'
#' @export
addSamples <- function(study, samples) {
  checkStudy(study)
  checkSamples(samples)

  study[["samples"]] <- utils::modifyList(study[["samples"]], samples)

  return(study)
}

#' Add feature metadata
#'
#' @param features The metadata variables that describe the features in the
#'   study. The input object is a list of data frames (one per model). The first
#'   column of each data frame is used as the featureID, so it must contain
#'   unique values. To share a data frame across multiple models, use the
#'   modelID "default". All columns will be coerced to character strings.
#' @inheritParams shared-add
#'
#' @export
addFeatures <- function(study, features) {
  checkStudy(study)
  checkFeatures(features)

  features <- lapply(features, coerceColsToCharacter)

  study[["features"]] <- utils::modifyList(study[["features"]], features)

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
addModels <- function(study, models) {
  checkStudy(study)
  checkModels(models)

  study[["models"]] <- utils::modifyList(study[["models"]], models)

  return(study)
}

#' Add assays
#'
#' @param assays The assays from the study. The input object is a list of data
#'   frames (one per model). The row names should correspond to the featureIDs
#'   (\code{\link{addFeatures}}). The column names should correspond to the
#'   sampleIDs (\code{\link{addSamples}}). The data frame should only contain
#'   numeric values. To share a data frame across multiple models, use the
#'   modelID "default".
#' @inheritParams shared-add
#'
#' @export
addAssays <- function(study, assays) {
  checkStudy(study)
  checkAssays(assays)

  study[["assays"]] <- utils::modifyList(study[["assays"]], assays)

  return(study)
}

#' Add tests
#'
#' @param tests The tests from the study. The input object is a list of lists.
#'   Each element of the top-level list is a model. The names should be the
#'   modelIDs. For each modelID, each element of the nested list is a test. The
#'   names should be the testIDs. The value should be a single character string
#'   describing the testID. To share tests across multiple models, use the
#'   modelID "default".
#' @inheritParams shared-add
#'
#' @examples
#'   study <- createStudy("example")
#'   tests <- list(
#'     default = list(
#'       test1 = "Name of first test",
#'       test2 = "Name of second test"
#'     )
#'   )
#'   study <- addTests(study, tests)
#'
#' @export
addTests <- function(study, tests) {
  checkStudy(study)
  checkTests(tests)

  study[["tests"]] <- utils::modifyList(study[["tests"]], tests)

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
#'   character vector of featureIDs.
#' @inheritParams shared-add
#'
#' @export
addAnnotations <- function(study, annotations) {
  checkStudy(study)
  checkAnnotations(annotations)

  study[["annotations"]] <- utils::modifyList(study[["annotations"]], annotations)

  return(study)
}

#' Add inference results
#'
#' @param results The inference results from each model. The input is a
#'   nested named list. The names of the list correspond to the model names.
#'   Each element in the list should be a list of data frames with inference
#'   results, one for each test. In each data frame, the featureID must be in
#'   the first column, and all other columns must be numeric.
#' @inheritParams shared-add
#'
#' @export
addResults <- function(study, results) {
  checkStudy(study)
  checkResults(results)

  study[["results"]] <- utils::modifyList(study[["results"]], results)

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
addEnrichments <- function(study, enrichments) {
  checkStudy(study)
  checkEnrichments(enrichments)

  study[["enrichments"]] <- utils::modifyList(study[["enrichments"]], enrichments)

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
#'   first column of each data frame is used as the featureID, so it must
#'   contain the same IDs as the corresponding features data frame
#'   (\code{\link{addFeatures}}). To share a data frame across multiple models,
#'   use the modelID "default". All columns will be coerced to character
#'   strings.
#' @inheritParams shared-add
#'
#' @export
addMetaFeatures <- function(study, metaFeatures) {
  checkStudy(study)
  checkMetaFeatures(metaFeatures)

  metaFeatures <- lapply(metaFeatures, coerceColsToCharacter)

  study[["metaFeatures"]] <- utils::modifyList(study[["metaFeatures"]], metaFeatures)

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
#' measurements for a specific featureID. That column will always be named
#' \code{feature}. The second argument, \code{featureID}, is the unique ID of
#' the feature. This is for labeling the plot.
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
#'   multiple models, use the modelID "default".
#' @inheritParams shared-add
#'
#' @export
addPlots <- function(study, plots) {
  checkStudy(study)
  checkPlots(plots)

  study[["plots"]] <- utils::modifyList(study[["plots"]], plots)

  return(study)
}

#' Add barcode plot metadata
#'
#' The app can display a barcode plot of the enrichment results for a given
#' annotation term. The metadata in `barcodes` instructs the app how to create
#' and label the barcode plot.
#'
#' @param barcodes The metadata variables that describe the barcode plot.
#'   The input object is a list of lists (one per model). Each sublist must
#'   contain the element \code{statistic}, which is the column name in the
#'   results table to use to construct the barcode plot. Each sublist may
#'   additionally contain any of the following optional elements:
#'   1) \code{absolute} - Should the statistic be converted to its absolute
#'   value (default is \code{TRUE}).
#'   2) \code{logFoldChange} - The column name in the results table that contains
#'   the log fold change values.
#'   3) \code{labelStat} - The x-axis label to describe the statistic.
#'   4) \code{labelLow} - The left-side label to describe low values of the statistic.
#'   5) \code{labelHigh} - The right-side label to describe high values of the statistic.
#'   6) \code{featureDisplay} - The feature variable to use to label the barcode plot
#'   on hover.
#'   To share metadata across multiple models, use the modelID "default".
#' @inheritParams shared-add
#'
#' @export
addBarcodes <- function(study, barcodes) {
  checkStudy(study)
  checkBarcodes(barcodes)

  study[["barcodes"]] <- utils::modifyList(study[["barcodes"]], barcodes)

  return(study)
}

#' Add reports
#'
#' You can include reports of the analyses you performed to generate the
#' results.
#'
#' @param reports The analysis report(s) that explain how the study results were
#'   generated. The input object is a list of character vectors (one per model).
#'   Each element should be either a URL or a path to a file on your computer.
#'   If it is a path to a file, this file will be included in the exported study
#'   package. To share a report across multiple models, use the modelID
#'   "default".
#' @inheritParams shared-add
#'
#' @export
addReports <- function(study, reports) {
  checkStudy(study)
  checkReports(reports)

  study[["reports"]] <- utils::modifyList(study[["reports"]], reports)

  return(study)
}
