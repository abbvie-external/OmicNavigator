#' Create a study
#'
#' Create a new OmicNavigator study.
#'
#' You can add metadata to describe your study by passing a named list to to the
#' argument \code{studyMeta}. The names of the list cannot contain spaces or
#' colons, and they can't start with \code{#} or \code{-}. The values of each
#' list should be a single value. Also, your metadata fields cannot use any of
#' the
#' \href{https://gist.github.com/jdblischak/f9d946327c9991fb57dde1e6f2bff1c2}{reserved
#' fields for R's DESCRIPTION file}.
#'
#' @param name Name of the study
#' @param description Description of the study
#' @param version (Optional) Include a version number to track the updates to
#'   your study package. If you export the study to a package, the version is
#'   used as the package version.
#' @param maintainer (Optional) Include the name of the study package's
#'   maintainer
#' @param maintainerEmail (Optional) Include the email of the study package's
#'   maintainer
#' @param studyMeta (Optional) Define metadata about your study. The input is a
#'   list of key:value pairs. See below for more details.
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
#' @inheritParams addMapping
#' @inheritParams addBarcodes
#' @inheritParams addReports
#' @inheritParams addResultsLinkouts
#' @inheritParams addEnrichmentsLinkouts
#' @inheritParams addMetaFeaturesLinkouts
#'
#' @return Returns a new OmicNavigator study object, which is a named nested
#'   list with class \code{onStudy}
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
#'   \code{\link{addMapping}},
#'   \code{\link{addBarcodes}},
#'   \code{\link{addReports}},
#'   \code{\link{addResultsLinkouts}},
#'   \code{\link{addEnrichmentsLinkouts}},
#'   \code{\link{addMetaFeaturesLinkouts}},
#'   \code{\link{exportStudy}},
#'   \code{\link{installStudy}}
#'
#' @examples
#'
#' study <- createStudy(name = "ABC",
#'                      description = "An analysis of ABC")
#'
#' # Define a version and study metadata
#' study <- createStudy(name = "ABC",
#'                      description = "An analysis of ABC",
#'                      version = "0.1.0",
#'                      maintainer = "My Name",
#'                      maintainerEmail = "me@email.com",
#'                      studyMeta = list(department = "immunology",
#'                                       organism = "Mus musculus"))
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
                        mapping = list(),
                        barcodes = list(),
                        reports = list(),
                        resultsLinkouts = list(),
                        enrichmentsLinkouts = list(),
                        metaFeaturesLinkouts = list(),
                        version = NULL,
                        maintainer = NULL,
                        maintainerEmail = NULL,
                        studyMeta = list())
{
  checkName(name)
  checkDescription(description)
  checkVersion(version)
  checkMaintainer(maintainer)
  checkMaintainerEmail(maintainerEmail)
  checkStudyMeta(studyMeta)

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
                mapping = list(),
                barcodes = list(),
                reports = list(),
                resultsLinkouts = list(),
                enrichmentsLinkouts = list(),
                metaFeaturesLinkouts = list(),
                overlaps = list(),
                version = version,
                maintainer = maintainer,
                maintainerEmail = maintainerEmail,
                studyMeta = studyMeta)
  class(study) <- "onStudy"

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
  study <- addMapping(study, mapping = mapping)
  study <- addBarcodes(study, barcodes = barcodes)
  study <- addReports(study, reports = reports)
  study <- addResultsLinkouts(study, resultsLinkouts = resultsLinkouts)
  study <- addEnrichmentsLinkouts(study, enrichmentsLinkouts = enrichmentsLinkouts)
  study <- addMetaFeaturesLinkouts(study, metaFeaturesLinkouts = metaFeaturesLinkouts)

  return(study)
}

#' Shared parameters for add functions
#'
#' @name shared-add
#'
#' @param study An OmicNavigator study created with \code{\link{createStudy}}
#' @param reset Reset the data prior to adding the new data (default:
#'   \code{FALSE}). The default is to add to or modify any previously added data
#'   (if it exists). Setting \code{reset = TRUE} enables you to remove existing
#'   data you no longer want to include in the study.
#'
#' @return Returns the original \code{onStudy} object passed to the argument
#'   \code{study}, but modified to include the newly added data
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
#' @inherit shared-add
#'
#' @export
addSamples <- function(study, samples, reset = FALSE) {
  addElements(study, samples, reset)
}

#' Add feature metadata
#'
#' @param features The metadata variables that describe the features in the
#'   study. The input object is a list of data frames (one per model). The first
#'   column of each data frame is used as the featureID, so it must contain
#'   unique values. To share a data frame across multiple models, use the
#'   modelID "default". All columns will be coerced to character strings.
#' @inherit shared-add
#'
#' @export
addFeatures <- function(study, features, reset = FALSE) {
  addElements(study, features, reset)
}

#' Add models
#'
#' @param models The models analyzed in the study. The input is a named list.
#'   The names correspond to the names of the models. The elements correspond to
#'   the descriptions of the models. Alternatively, instead of a single
#'   character string, you can provide a list of metadata fields about each
#'   model. The field "description" will be used to derive the tooltip displayed
#'   in the app.
#' @inherit shared-add
#'
#' @examples
#'   study <- createStudy("example")
#'   models <- list(
#'     model_01 = "Name of first model",
#'     model_02 = "Name of second model"
#'   )
#'   study <- addModels(study, models)
#'
#'   # Alternative: provide additional metadata about each model
#'   models <- list(
#'     model_01 = list(
#'       description = "Name of first model",
#'       data_type = "transcriptomics"
#'     ),
#'     model_02 = list(
#'       description = "Name of second model",
#'       data_type = "proteomics"
#'     )
#'   )
#'
#' @export
addModels <- function(study, models, reset = FALSE) {
  addElements(study, models, reset)
}

#' Add assays
#'
#' @param assays The assays from the study. The input object is a list of data
#'   frames (one per model). The row names should correspond to the featureIDs
#'   (\code{\link{addFeatures}}). The column names should correspond to the
#'   sampleIDs (\code{\link{addSamples}}). The data frame should only contain
#'   numeric values. To share a data frame across multiple models, use the
#'   modelID "default".
#' @inherit shared-add
#'
#' @export
addAssays <- function(study, assays, reset = FALSE) {
  addElements(study, assays, reset)
}

#' Add tests
#'
#' @param tests The tests from the study. The input object is a list of lists.
#'   Each element of the top-level list is a model. The names should be the
#'   modelIDs. For each modelID, each element of the nested list is a test. The
#'   names should be the testIDs. The value should be a single character string
#'   describing the testID. To share tests across multiple models, use the
#'   modelID "default". Instead of a single character string, you can provide a
#'   list of metadata fields about each test. The field "description" will be
#'   used to derive the tooltip displayed in the app.
#' @inherit shared-add
#'
#' @examples
#'   study <- createStudy("example")
#'   tests <- list(
#'     default = list(
#'       test_01 = "Name of first test",
#'       test_02 = "Name of second test"
#'     )
#'   )
#'   study <- addTests(study, tests)
#'
#'   # Alternative: provide additional metadata about each test
#'   tests <- list(
#'     default = list(
#'       test_01 = list(
#'         description = "Name of first test",
#'         comparison_type = "treatment vs control",
#'         effect_size = "beta"
#'       ),
#'       test_02 = list(
#'         description = "Name of second test",
#'         comparison_type = "treatment vs control",
#'         effect_size = "logFC"
#'       )
#'     )
#'   )
#'
#' @export
addTests <- function(study, tests, reset = FALSE) {
  addElements(study, tests, reset)
}

#' Add annotations
#'
#' @param annotations The annotations used for the enrichment analyses. The
#'   input is a nested list. The top-level list contains one entry per
#'   annotation database, e.g. reactome. The names correspond to the name of
#'   each annotation database. Each of these elements should be a list that
#'   contains more information about each annotation database. Specifically the
#'   sublist should contain 1) \code{description}, a character vector that
#'   describes the resource, 2) \code{featureID}, the name of the column in the
#'   features table that was used for the enrichment analysis, and 3) \code{terms},
#'   a list of annotation terms. The names of \code{terms} sublist correspond to
#'   the name of the annotation terms. Each of the annotation terms should be a
#'   character vector of featureIDs.
#' @inherit shared-add
#'
#' @export
addAnnotations <- function(study, annotations, reset = FALSE) {
  addElements(study, annotations, reset)
}

#' Add inference results
#'
#' @param results The inference results from each model. The input is a
#'   nested named list. The names of the list correspond to the model names.
#'   Each element in the list should be a list of data frames with inference
#'   results, one for each test. In each data frame, the featureID must be in
#'   the first column, and all other columns must be numeric.
#' @inherit shared-add
#'
#' @export
addResults <- function(study, results, reset = FALSE) {
  addElements(study, results, reset)
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
#' @inherit shared-add
#'
#' @export
addEnrichments <- function(study, enrichments, reset = FALSE) {
  addElements(study, enrichments, reset)
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
#' @inherit shared-add
#'
#' @export
addMetaFeatures <- function(study, metaFeatures, reset = FALSE) {
  addElements(study, metaFeatures, reset)
}

#' Add custom plotting functions
#'
#' `addPlots()` adds custom plotting functions and plot metadata to an
#' OmicNavigator study.
#'
#' Custom plotting functions must be constructed to accept as the first argument
#' the value returned from `getPlottingData()`. Custom plotting functions can
#' have additional arguments, but these must be provided with default values.
#' The end-user should call `getPlottingData()` when testing their custom
#' plotting function.  The end-user should consider the nature of the plot, i.e.
#' the `plotType` and (rarely) `models` values (see [getPlottingData()]). For
#' example, a custom plotting function meant to produce a `multiTest` plot
#' should accept the output of a `getPlottingData()` call with multiple
#' `testID`s assigned to the `testID` argument. See the details section of
#' [plotStudy()] for a description of how `plotType` dictates the way a custom
#' plotting function is invoked by the app.
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
#' @param plots A nested list containing custom plotting functions and plot
#'   metadata. The input object is a 3-level nested list. The first, or
#'   top-level list element name(s) must match the study `modelID`(s). The second,
#'   or mid-level list element name(s) must match the names of the plotting
#'   function(s) defined in the current R session (see Details below for
#'   function construction requirements). The third, or bottom-level list
#'   provides metadata to categorize, display, and support each plot. The
#'   accepted fields are `displayName`, `description`, `plotType`, `models`, and
#'   `packages.` `displayName` sets the plot name in the app and the `description`
#'   field will display as a tool tip when hovering over plotting dropdown
#'   menus. The `plotType` field is a character vector that categorizes the plot
#'   by 1) the number of features it supports (“`singleFeature`” or
#'   “`multiFeature`”), 2) the number of test results used by the plotting
#'   function (“`singleTest`”, “`multiTest`”), 3) if data from one or more models is
#'   used (add “`multiModel`” to specify that data from two or more models are
#'   used in the plot; otherwise the plot is assumed to reference only data
#'   within the model specified by the top-level list element name), and 4) if
#'   the plot is interactive (add “`plotly`” to specify interactive plots built
#'   using the plotly package; otherwise the plot is assumed to be static).
#'   e.g., `plotType = c("multiFeature", "multiTest",”plotly”)`. If you do not
#'   specify the `plotType` the plot will be designated as `plotType =
#'   c("singleFeature", "singleTest")`. The `models` field is an optional
#'   character vector that specifies the models that should be used by the app
#'   when invoking your custom plotting function. This field is set to ‘all’ by
#'   default and is only used when `plotType` includes “`multiModel`”. If this field
#'   is not included the app will assume all models in the study should be used
#'   with your plotting function. If the plotting function requires additional
#'   packages beyond those attached by default to a fresh R session, these must
#'   be defined in the element `packages`.
#'
#' @inherit shared-add
#'
#' @seealso \code{\link{getPlottingData}}, \code{\link{plotStudy}}
#'
#' @export
addPlots <- function(study, plots, reset = FALSE) {
  addElements(study, plots, reset)
}

#' Add mapping object
#'
#' @param mapping Feature IDs from models. The input object is a list of named
#' data frames. For each data frame, column names indicate model names while
#' rows indicate featureIDs per model. Features with same index position across
#' columns are treated as mapped across models. For each model, feature IDs must
#' match feature IDs available in the results object of the respective model.
#' 1:N relationships are allowed.
#'
#' Mapping list elements are required to be named as 'default' or after a model
#' name as provided in addModels(). If a single data frame is provided, this
#' list element is recommended to be named 'default'. For multiple list
#' elements, each with its own data frame, list elements should be named after
#' model name(s) (a single element may still be  named 'default'). In that case,
#' when navigating in ON front-end (FE), mapping element related to the selected
#' model in the FE will be used in multimodel plots. If a selected model in FE
#' does not have a corresponding mapping list element, it may still use the
#' mapping list element called 'default' if this is available.
#'
#' E.g., if in a study there are models "transcriptomics" and "proteomics" and
#' the user wants to create a plot based on data from both, a mapping list
#' should be provided with addMapping(). In this case, the mapping list element
#' may be named 'default'. This should contain a data frame with column names
#' 'transcriptomics' and 'proteomics', where feature IDs that map across models
#' are found in the same row.
#' @inherit shared-add
#'
#' @seealso \code{\link{getPlottingData}}, \code{\link{plotStudy}}
#'
#' @export
addMapping <- function(study, mapping, reset = FALSE) {
  addElements(study, mapping, reset)
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
#' @inherit shared-add
#'
#' @export
addBarcodes <- function(study, barcodes, reset = FALSE) {
  addElements(study, barcodes, reset)
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
#' @inherit shared-add
#'
#' @export
addReports <- function(study, reports, reset = FALSE) {
  addElements(study, reports, reset)
}

#' Add linkouts to external resources in the results table
#'
#' You can provide additional information on the features in your study by
#' providing linkouts to external resources. These will be embedded directly in
#' the results table.
#'
#' For each linkout, the URL pattern you provide will be concatenated with the
#' value of that column for each row. As an example, if your features table
#' included a column named \code{"ensembl"} that contained the Ensembl Gene ID
#' for each feature, you could create a linkout to Ensembl using the following
#' pattern:
#'
#' \preformatted{ensembl = "https://ensembl.org/Homo_sapiens/Gene/Summary?g="}
#'
#' As another example, if you had a column named \code{"entrez"} that contained
#' the Entrez Gene ID  for each feature, you could create a linkout to Entrez
#' using the following pattern:
#'
#' \preformatted{entrez = "https://www.ncbi.nlm.nih.gov/gene/"}
#'
#' Note that you can provide more than one linkout per column.
#'
#' @param resultsLinkouts The URL patterns that describe linkouts to external
#'   resources (see Details below). The input object is a nested named list. The
#'   names of the list correspond to the model names. Each element of the list
#'   is a named list of character vectors. The names of this nested list must
#'   correspond to the column names of the matching features table. To share
#'   linkouts across multiple models, use the modelID "default".
#' @inherit shared-add
#'
#' @examples
#'   study <- createStudy("example")
#'   resultsLinkouts <- list(
#'     default = list(
#'       ensembl = c("https://ensembl.org/Homo_sapiens/Gene/Summary?g=",
#'                   "https://www.genome.ucsc.edu/cgi-bin/hgGene?hgg_gene="),
#'       entrez = "https://www.ncbi.nlm.nih.gov/gene/"
#'     )
#'   )
#'   study <- addResultsLinkouts(study, resultsLinkouts)
#'
#' @seealso \code{\link{addFeatures}}
#'
#' @export
addResultsLinkouts <- function(study, resultsLinkouts, reset = FALSE) {
  addElements(study, resultsLinkouts, reset)
}

#' Add linkouts to external resources in the enrichments table
#'
#' You can provide additional information on the annotation terms in your study
#' by providing linkouts to external resources. These will be embedded directly
#' in the enrichments table.
#'
#' For each linkout, the URL pattern you provide will be concatenated with the
#' value of the termID column. As an example, if you used the annotation
#' database \href{https://amigo.geneontology.org/}{AmiGO 2} for your enrichments
#' analysis, you can provide a linkout for each termID using the following
#' pattern:
#'
#' \preformatted{go = "https://amigo.geneontology.org/amigo/term/"}
#'
#' As another example, if you used the annotation database
#' \href{https://reactome.org/}{Reactome} for your enrichments analysis, you can
#' provide a linkout for each termID using the following pattern:
#'
#' \preformatted{reactome = "https://reactome.org/content/detail/"}
#'
#' Note that you can provide more than one linkout per termID.
#'
#' @param enrichmentsLinkouts The URL patterns that describe linkouts to
#'   external resources (see Details below). The input object is a named list.
#'   The names of the list correspond to the annotation names. Each element of
#'   the list is a character vector of linkouts for that annotationID.
#' @inherit shared-add
#'
#' @examples
#'   study <- createStudy("example")
#'   enrichmentsLinkouts <- list(
#'     gobp = c("https://amigo.geneontology.org/amigo/term/",
#'              "https://www.ebi.ac.uk/QuickGO/term/"),
#'     reactome = "https://reactome.org/content/detail/"
#'   )
#'   study <- addEnrichmentsLinkouts(study, enrichmentsLinkouts)
#'
#' @seealso \code{\link{addAnnotations}}, \code{\link{addEnrichments}}
#'
#' @export
addEnrichmentsLinkouts <- function(study, enrichmentsLinkouts, reset = FALSE) {
  addElements(study, enrichmentsLinkouts)
}

#' Add linkouts to external resources in the metaFeatures table
#'
#' You can provide additional information on the metaFeatures in your study by
#' providing linkouts to external resources. These will be embedded directly in
#' the metaFeatures table.
#'
#' For each linkout, the URL pattern you provide will be concatenated with the
#' value of that column for each row. As an example, if your metaFeatures table
#' included a column named \code{"ensembl"} that contained the Ensembl Gene ID
#' for each feature, you could create a linkout to Ensembl using the following
#' pattern:
#'
#' \preformatted{ensembl = "https://ensembl.org/Homo_sapiens/Gene/Summary?g="}
#'
#' As another example, if you had a column named \code{"entrez"} that contained
#' the Entrez Gene ID  for each feature, you could create a linkout to Entrez
#' using the following pattern:
#'
#' \preformatted{entrez = "https://www.ncbi.nlm.nih.gov/gene/"}
#'
#' Note that you can provide more than one linkout per column.
#'
#' @param metaFeaturesLinkouts The URL patterns that describe linkouts to external
#'   resources (see Details below). The input object is a nested named list. The
#'   names of the list correspond to the model names. Each element of the list
#'   is a named list of character vectors. The names of this nested list must
#'   correspond to the column names of the matching metaFeatures table (\code{\link{addMetaFeatures}}). To share
#'   linkouts across multiple models, use the modelID "default".
#' @inherit shared-add
#'
#' @examples
#'   study <- createStudy("example")
#'   metaFeaturesLinkouts <- list(
#'     default = list(
#'       ensembl = c("https://ensembl.org/Homo_sapiens/Gene/Summary?g=",
#'                   "https://www.genome.ucsc.edu/cgi-bin/hgGene?hgg_gene="),
#'       entrez = "https://www.ncbi.nlm.nih.gov/gene/"
#'     )
#'   )
#'   study <- addMetaFeaturesLinkouts(study, metaFeaturesLinkouts)
#'
#' @seealso \code{\link{addMetaFeatures}}
#'
#' @export
addMetaFeaturesLinkouts <- function(study, metaFeaturesLinkouts, reset = FALSE) {
  addElements(study, metaFeaturesLinkouts, reset)
}

addElements <- function(study, elements, reset = FALSE) {
  elementsName <- deparse(substitute(elements))
  checkStudy(study)
  # Note: I really don't like this strategy of obtaining functions dynamically
  # via getFromNamespace(), but I'm not sure what the best strategy would be.
  # I could create S3 generics for the check() and sanitize() functions, and
  # then set the class of `elements` to `elementsName`. That is more idiomatic,
  # but it also seems like a lot simply to internally dispatch these functions
  # here and also in validateStudy().
  checkFunctionName <- paste0("check", capitalize(elementsName))
  checkFunction <- utils::getFromNamespace(checkFunctionName, ns = "OmicNavigator")
  checkFunction(elements)
  sanitizeFunctionName <- paste0("sanitize", capitalize(elementsName))
  sanitizeFunction <- utils::getFromNamespace(sanitizeFunctionName, ns = "OmicNavigator")
  elements <- sanitizeFunction(elements)

  if (reset) {
    study[[elementsName]] <- list()
  }

  study[[elementsName]] <- utils::modifyList(study[[elementsName]], elements)

  return(study)
}
