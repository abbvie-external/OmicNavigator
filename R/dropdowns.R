#' Shared parameters for dropdown functions
#'
#' @name shared-dropdowns
#'
#' @param study An OmicNavigator study. Only accepts name of installed study
#'   package.
#' @param modelID The modelID selected by the user in the app
#'
#' @return A named list. The names are the identifiers to be displayed in the
#'   dropdown menu, and each list element is a single character vector with the
#'   description to be used as a tooltip in the app. If no custom description
#'   was provided by the user, the tooltip text is simply the identifier.
#'
#' @keywords internal
NULL

#' Get installed OmicNavigator studies that have results
#'
#' This is the API endpoint the app uses to populate the dropdown menu in the
#' Differential Analysis tab with the list of available studies with results
#' data.
#'
#' Internally, \code{getResultsStudies} calls \code{\link{getInstalledStudies}}
#' with \code{hasElements = "results"}.
#'
#' @inherit getInstalledStudies params return
#'
#' @seealso
#'   \code{\link{getInstalledStudies}},
#'   \code{\link{getEnrichmentsStudies}}
#'
#' @export
getResultsStudies <- function(libraries = NULL) {
  getInstalledStudies(hasElements = "results", libraries = libraries)
}

#' Get the models for the results of an installed OmicNavigator study
#'
#' This is the API endpoint the app uses to populate the dropdown menu in the
#' Differential Analysis tab with the list of available models for the selected
#' study.
#'
#' The models correspond to those used when adding the results with
#' \code{\link{addResults}}. Any optional tooltips correspond to the
#' descriptions added with \code{\link{addModels}}.
#'
#' @inherit shared-dropdowns params return
#' @inherit getInstalledStudies params
#'
#' @seealso
#'   \code{\link{getResultsStudies}},
#'   \code{\link{getEnrichmentsModels}},
#'   \code{\link{addResults}},
#'   \code{\link{addModels}}
#'
#' @export
getResultsModels <- function(study, libraries = NULL) {
  if (!is.character(study)) {
    stop("Only installed study packages are supported")
  }

  onDirectory <- getDirectory(study, libraries)
  resultsDirectory <- file.path(onDirectory, "results")
  resultsModels <- dir(resultsDirectory)

  if (isEmpty(resultsModels)) return(list())

  dropdown <- vector("list", length = length(resultsModels))
  names(dropdown) <- resultsModels

  # Check for available tooltips added via addModels(). If not available, use
  # the modelID itself for the tooltip
  for (modelID in resultsModels) {
    modelDisplay <- getModels(study, modelID = modelID, quiet = TRUE, libraries = libraries)
    # The tooltip is either added as a single string per modelID, or as a named
    # list with other metadata fields, where the field "description" is the
    # metadata field
    if (is.list(modelDisplay)) modelDisplay <- modelDisplay[["description"]]
    if (isEmpty(modelDisplay)) modelDisplay <- modelID
    dropdown[[modelID]] <- modelDisplay
  }

  return(dropdown)
}

#' Get the tests for the results of an installed OmicNavigator study
#'
#' This is the API endpoint the app uses to populate the dropdown menu in the
#' Differential Analysis tab with the list of available tests for the selected
#' model and study.
#'
#' The tests correspond to those used when adding the results with
#' \code{\link{addResults}}. Any optional tooltips correspond to the
#' descriptions added with \code{\link{addTests}}.
#'
#' @inherit shared-dropdowns params return
#' @inherit getInstalledStudies params
#'
#' @seealso
#'   \code{\link{getResultsStudies}},
#'   \code{\link{getResultsModels}},
#'   \code{\link{addResults}},
#'   \code{\link{addTests}}
#'
#' @export
getResultsTests <- function(study, modelID, libraries = NULL) {
  if (!is.character(study)) {
    stop("Only installed study packages are supported")
  }

  onDirectory <- getDirectory(study, libraries)
  elementsFiles <- getFiles(onDirectory)
  resultsTests <- names(elementsFiles[["results"]][[modelID]])

  if (isEmpty(resultsTests)) return(list())

  dropdown <- vector("list", length = length(resultsTests))
  names(dropdown) <- resultsTests

  # Check for available tooltips added via addTests(). If not available, use the
  # testID itself for the tooltip
  for (testID in resultsTests) {
    testDisplay <- getTests(study, modelID = modelID, testID = testID, quiet = TRUE, libraries = libraries)
    # The tooltip is either added as a single string per testID, or as a named
    # list with other metadata fields, where the field "description" is the
    # metadata field
    if (is.list(testDisplay)) testDisplay <- testDisplay[["description"]]
    if (isEmpty(testDisplay)) testDisplay <- testID
    dropdown[[testID]] <- testDisplay
  }

  return(dropdown)
}

#' Get installed OmicNavigator studies that have enrichments
#'
#' This is the API endpoint the app uses to populate the dropdown menu in the
#' Enrichment Analysis tab with the list of available studies with enrichments
#' data.
#'
#' Internally, \code{getEnrichmentsStudies} calls
#' \code{\link{getInstalledStudies}} with \code{hasElements = "enrichments"}.
#'
#' @inherit getInstalledStudies params return
#'
#' @seealso
#'   \code{\link{getInstalledStudies}},
#'   \code{\link{getResultsStudies}}
#'
#' @export
getEnrichmentsStudies <- function(libraries = NULL) {
  getInstalledStudies(hasElements = "enrichments", libraries = libraries)
}

#' Get the models for the enrichments of an installed OmicNavigator study
#'
#' This is the API endpoint the app uses to populate the dropdown menu in the
#' Enrichment Analysis tab with the list of available models for the selected
#' study.
#'
#' The models correspond to those used when adding the results with
#' \code{\link{addEnrichments}}. Any optional tooltips correspond to the
#' descriptions added with \code{\link{addModels}}.
#'
#' @inherit shared-dropdowns params return
#' @inherit getInstalledStudies params
#'
#' @seealso
#'   \code{\link{getEnrichmentsStudies}},
#'   \code{\link{getResultsModels}},
#'   \code{\link{addEnrichments}},
#'   \code{\link{addModels}}
#'
#' @export
getEnrichmentsModels <- function(study, libraries = NULL) {
  if (!is.character(study)) {
    stop("Only installed study packages are supported")
  }

  onDirectory <- getDirectory(study, libraries)
  enrichmentsDirectory <- file.path(onDirectory, "enrichments")
  enrichmentsModels <- dir(enrichmentsDirectory)

  if (isEmpty(enrichmentsModels)) return(list())

  dropdown <- vector("list", length = length(enrichmentsModels))
  names(dropdown) <- enrichmentsModels

  # Check for available tooltips added via addModels(). If not available, use
  # the modelID itself for the tooltip
  for (modelID in enrichmentsModels) {
    modelDisplay <- getModels(study, modelID = modelID, quiet = TRUE, libraries = libraries)
    # The tooltip is either added as a single string per modelID, or as a named
    # list with other metadata fields, where the field "description" is the
    # metadata field
    if (is.list(modelDisplay)) modelDisplay <- modelDisplay[["description"]]
    if (isEmpty(modelDisplay)) modelDisplay <- modelID
    dropdown[[modelID]] <- modelDisplay
  }

  return(dropdown)
}

#' Get the annotations for the enrichments of an installed OmicNavigator study
#'
#' This is the API endpoint the app uses to populate the dropdown menu in the
#' Enrichment Analysis tab with the list of available annotations for the
#' selected model and study.
#'
#' The annotations correspond to those used when adding the enrichments with
#' \code{\link{addEnrichments}}. Any optional tooltips correspond to the
#' descriptions added with \code{\link{addAnnotations}}.
#'
#' @inherit shared-dropdowns params return
#' @inherit getInstalledStudies params
#'
#' @seealso
#'   \code{\link{getEnrichmentsStudies}},
#'   \code{\link{getEnrichmentsModels}},
#'   \code{\link{addEnrichments}},
#'   \code{\link{addAnnotations}}
#'
#' @export
getEnrichmentsAnnotations <- function(study, modelID, libraries = NULL) {
  if (!is.character(study)) {
    stop("Only installed study packages are supported")
  }

  onDirectory <- getDirectory(study, libraries)
  modelAnnotations <- dir(file.path(onDirectory, "enrichments", modelID))

  if (isEmpty(modelAnnotations )) return(list())

  dropdown <- vector("list", length = length(modelAnnotations ))
  names(dropdown) <- modelAnnotations

  # Check for available tooltips added via addAnnotations(). If not available,
  # use the annotationID itself for the tooltip
  for (annotationID in modelAnnotations) {
    annotationDisplay <- getAnnotations(study, annotationID = annotationID, quiet = TRUE, libraries = libraries)
    annotationDisplay <- annotationDisplay[["description"]]
    if (isEmpty(annotationDisplay)) annotationDisplay <- annotationID
    dropdown[[annotationID]] <- annotationDisplay
  }

  return(dropdown)
}
