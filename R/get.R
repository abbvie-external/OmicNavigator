#' Get installed OmicNavigator studies
#'
#' @param libraries Character vector of library directories to search for study
#'   packages. If \code{NULL}, uses \code{.libPaths}.
#'
#' @return Returns a character vector of the installed OmicNavigator study
#'   packages
#'
#' @export
getInstalledStudies <- function(libraries = NULL) {
  pkgsAll <- rownames(utils::installed.packages(lib.loc = libraries))
  names(pkgsAll) <- NULL
  regex <- getPrefix(regex = TRUE)
  pkgsOa <- grep(regex, pkgsAll, value = TRUE)
  studies <- pkgToStudy(pkgsOa)
  studies <- sort(studies)

  return(studies)
}

#' Get study metadata from the study name
#' 
#' @param name Name of the study (without the OmicNavigator prefix)
#' @return A list containing the study metadata
#' 
#' @export
getStudyMeta <- function(name) {
  stopifnot(is.character(name), length(name) == 1)
  pkg <- paste0(getPrefix(), name)
  if (!requireNamespace(pkg, quietly = TRUE)) {
    stop(sprintf("The study \"%s\" is not installed\n", name),
         "Did you run installStudy()?\n")
  }
  study <- importStudy(name)
  meta <- study[["studyMeta"]]
  meta[["description"]] <- study[["description"]]
  meta[["version"]] <- study[["version"]]
  meta[["maintainer"]] <- study[["maintainer"]]
  meta[["maintainerEmail"]] <- study[["maintainerEmail"]]
  return(meta)
}

#' Shared parameters for get functions
#'
#' @name shared-get
#'
#' @param study An OmicNavigator study. Either an object of class \code{onStudy},
#'   or the name of an installed study package.
#' @param modelID Filter by modelID
#' @param testID Filter by testID
#' @param annotationID Filter by annotationID
#' @param termID Filter by termID
#' @param featureID Filter by featureID
#' @param plotID Filter by plotID
#' @param quiet Suppress messages (default: \code{FALSE})
#'
#' @return The object returned depends on the data available and any filters
#'   (e.g. the argument  \code{modelID}):
#'
#' If no filters are specified, then the object returned is a nested list,
#' similar to the original input object.
#'
#' If one or more filters are applied, then only a subset of the original nested
#' list is returned. Technically, each filter applied is used to subset the
#' original nested list using \code{\link[base:Extract]{[[}}.
#'
#' If no data is available, an empty list is returned (\code{list()}).
#'
#' @keywords internal
NULL

#' Get models from a study
#'
#' @inherit shared-get
#' @inheritParams listStudies
#'
#' @export
getModels <- function(study, modelID = NULL, quiet = FALSE, libraries = NULL) {
  getElements(
    study,
    elements = "models",
    filters = list(modelID = modelID),
    fileType = "json",
    quiet = quiet,
    libraries = libraries
  )
}

#' Get samples from a study
#'
#' @inherit shared-get
#' @inheritParams listStudies
#'
#' @export
getSamples <- function(study, modelID = NULL, quiet = FALSE, libraries = NULL) {
  getElements(
    study,
    elements = "samples",
    filters = list(modelID = modelID),
    default = "default",
    quiet = quiet,
    libraries = libraries
  )
}

#' Get features from a study
#'
#' @inherit shared-get
#' @inheritParams listStudies
#'
#' @return A data frame (if \code{modelID} is specified) or a list of data
#'   frames. All the columns will be character strings, even if the values
#'   appear numeric.
#'
#' @export
getFeatures <- function(study, modelID = NULL, quiet = FALSE, libraries = NULL) {
  getElements(
    study,
    elements = "features",
    filters = list(modelID = modelID),
    default = "default",
    quiet = quiet,
    libraries = libraries,
    colClasses = "character"
  )
}

#' Get assays from a study
#'
#' @inherit shared-get
#' @inheritParams listStudies
#'
#' @export
getAssays <- function(study, modelID = NULL, quiet = FALSE, libraries = NULL) {
  getElements(
    study,
    elements = "assays",
    filters = list(modelID = modelID),
    default = "default",
    quiet = quiet,
    libraries = libraries,
    hasRowNames = TRUE,
    # The featureIDs are returned as row names, but they are initially imported
    # as the first column, and then converted to the row names. This is because
    # data.table does not support row names.
    colClasses = list(character = 1)
  )
}

#' Get tests from a study
#'
#' @inherit shared-get
#' @inheritParams listStudies
#'
#' @export
getTests <- function(study, modelID = NULL, testID = NULL, quiet = FALSE, libraries = NULL) {
  if (is.null(modelID) && !is.null(testID)) {
    stop("Must specify a model in order to specify a test")
  }

  getElements(
    study,
    elements = "tests",
    filters = list(modelID = modelID, testID = testID),
    default = "default",
    fileType = "json",
    quiet = quiet,
    libraries = libraries
  )
}

#' Get annotations from a study
#'
#' @inherit shared-get
#' @inheritParams listStudies
#'
#' @export
getAnnotations <- function(study, annotationID = NULL, quiet = FALSE, libraries = NULL) {
  getElements(
    study,
    elements = "annotations",
    filters = list(annotationID = annotationID),
    fileType = "json",
    quiet = quiet,
    libraries = libraries
  )
}

#' Get overlaps from a study
#'
#' @inherit shared-get
#' @inheritParams listStudies
#'
#' @export
getOverlaps <- function(study, annotationID = NULL, quiet = FALSE, libraries = NULL) {
  getElements(
    study,
    elements = "overlaps",
    filters = list(annotationID = annotationID),
    quiet = quiet,
    libraries = libraries
  )
}

#' Get results from a study
#'
#' @inherit shared-get
#' @inheritParams listStudies
#'
#' @export
getResults <- function(study, modelID = NULL, testID = NULL, quiet = FALSE, libraries = NULL) {
  if (is.null(modelID) && !is.null(testID)) {
    stop("Must specify a model in order to specify a test")
  }

  getElements(
    study,
    elements = "results",
    filters = list(modelID = modelID, testID = testID),
    quiet = quiet,
    libraries = libraries,
    colClasses = list(character = 1)
  )
}

#' Get enrichments from a study
#'
#' @inherit shared-get
#' @inheritParams listStudies
#'
#' @export
getEnrichments <- function(study, modelID = NULL, annotationID = NULL, testID = NULL, quiet = FALSE, libraries = NULL) {
  if (is.null(modelID) && !is.null(annotationID)) {
    stop("Must specify a model in order to specify an annotation")
  }
  if (is.null(modelID) && !is.null(testID)) {
    stop("Must specify a model in order to specify a test")
  }
  if (is.null(annotationID) && !is.null(testID)) {
    stop("Must specify an annotation in order to specify a test")
  }

  getElements(
    study,
    elements = "enrichments",
    filters = list(modelID = modelID, annotationID = annotationID, testID = testID),
    quiet = quiet,
    libraries = libraries
  )
}

#' Get metaFeatures from a study
#'
#' @inherit shared-get
#' @inheritParams listStudies
#'
#' @export
getMetaFeatures <- function(study, modelID = NULL, quiet = FALSE, libraries = NULL) {
  getElements(
    study,
    elements = "metaFeatures",
    filters = list(modelID = modelID),
    default = "default",
    quiet = quiet,
    libraries = libraries,
    colClasses = "character"
  )
}

#' Get plots from a study
#'
#' @inherit shared-get
#' @inheritParams listStudies
#'
#' @export
getPlots <- function(study, modelID = NULL, quiet = FALSE, libraries = NULL) {
  getElements(
    study,
    elements = "plots",
    filters = list(modelID = modelID),
    default = "default",
    fileType = "json",
    quiet = quiet,
    libraries = libraries
  )
}

#' Get mapping object from a study
#'
#' @inherit shared-get
#' @inheritParams listStudies
#'
#' @export
getMapping <- function(study, modelID = NULL, quiet = FALSE, libraries = NULL) {
  getElements(
    study,
    elements = "mapping",
    filters = list(modelID = modelID),
    default = "default",
    quiet = quiet,
    libraries = libraries
  )
}

#' Get barcodes from a study
#'
#' @inherit shared-get
#' @inheritParams listStudies
#'
#' @export
getBarcodes <- function(study, modelID = NULL, quiet = FALSE, libraries = NULL) {
  getElements(
    study,
    elements = "barcodes",
    filters = list(modelID = modelID),
    default = "default",
    fileType = "json",
    quiet = quiet,
    libraries = libraries
  )
}

#' Get reports from a study
#'
#' @inherit shared-get
#' @inheritParams listStudies
#'
#' @export
getReports <- function(study, modelID = NULL, quiet = FALSE, libraries = NULL) {
  getElements(
    study,
    elements = "reports",
    filters = list(modelID = modelID),
    default = "default",
    fileType = "json",
    quiet = quiet,
    libraries = libraries
  )
}

#' Get results table linkouts from a study
#'
#' @inherit shared-get
#' @inheritParams listStudies
#'
#' @export
getResultsLinkouts <- function(study, modelID = NULL, quiet = FALSE, libraries = NULL) {
  getElements(
    study,
    elements = "resultsLinkouts",
    filters = list(modelID = modelID),
    default = "default",
    fileType = "json",
    quiet = quiet,
    libraries = libraries
  )
}

#' Get enrichments table linkouts from a study
#'
#' @inherit shared-get
#' @inheritParams listStudies
#'
#' @export
getEnrichmentsLinkouts <- function(study, annotationID = NULL, quiet = FALSE, libraries = NULL) {
  getElements(
    study,
    elements = "enrichmentsLinkouts",
    filters = list(annotationID = annotationID),
    fileType = "json",
    quiet = quiet,
    libraries = libraries
  )
}

#' Get metaFeatures table linkouts from a study
#'
#' @inherit shared-get
#' @inheritParams listStudies
#'
#' @export
getMetaFeaturesLinkouts <- function(study, modelID = NULL, quiet = FALSE, libraries = NULL) {
  getElements(
    study,
    elements = "metaFeaturesLinkouts",
    filters = list(modelID = modelID),
    default = "default",
    fileType = "json",
    quiet = quiet,
    libraries = libraries
  )
}

#' Get metaAssays from a study
#'
#' @inherit shared-get
#' @inheritParams listStudies
#'
#' @export
getMetaAssays <- function(study, modelID = NULL, quiet = FALSE, libraries = NULL) {
  getElements(
    study,
    elements = "metaAssays",
    filters = list(modelID = modelID),
    default = "default",
    quiet = quiet,
    libraries = libraries,
    hasRowNames = TRUE,
    colClasses = list(character = 1)
  )
}

# ... Arguments passed to either data.table::fread() or jsonlite::read_json()
getElements <- function(
  study,
  elements,
  filters = list(),
  default = NULL,
  fileType = c("txt", "json"),
  quiet = FALSE,
  libraries = NULL,
  ...
)
{
  stopifnot(
    is.character(elements),
    length(elements) == 1,
    is.list(filters),
    if (length(filters) > 0) !is.null(names(filters)) else TRUE
  )
  UseMethod("getElements")
}

#' @export
getElements.default <- function(
  study,
  elements,
  filters = list(),
  default = NULL,
  fileType = c("txt", "json"),
  quiet = FALSE,
  libraries = NULL,
  ...
)
{
  stop(sprintf("No method for object of class \"%s\"", class(study)))
}

#' @export
getElements.onStudy <- function(
  study,
  elements,
  filters = list(),
  default = NULL,
  fileType = c("txt", "json"),
  quiet = FALSE,
  libraries = NULL,
  ...
)
{
  elementsList <- study[[elements]]

  if (isEmpty(elementsList)) {
    if (!quiet) message(sprintf("No %s available for study \"%s\"",
                                elements, study[["name"]]))
    return(list())
  }

  filters <- Filter(function(x) !is.null(x), filters)

  for (i in seq_along(filters)) {
    filterName <- names(filters)[i]
    filterValue <- filters[[i]]
    namesCurrent <- names(elementsList)
    if (filterValue %in% namesCurrent) {
      elementsList <- elementsList[[filterValue]]
    } else if (!is.null(default) && default %in% namesCurrent) {
      if (!quiet) message(sprintf("Returning \"%s\" %s for %s \"%s\"",
                                  default, elements, filterName, filterValue))
      elementsList <- elementsList[[default]]
    } else {
      if (!quiet) message(sprintf("No %s available for %s \"%s\"",
                                  elements, filterName, filterValue))
      return(list())
    }
  }

  return(elementsList)
}

#' @export
getElements.character <- function(
  study,
  elements,
  filters = list(),
  default = NULL,
  fileType = c("txt", "json"),
  quiet = FALSE,
  libraries = NULL,
  ...
)
{
  onDirectory <- getDirectory(study, libraries)
  if (onDirectory == "") {
    stop(sprintf("The study \"%s\" is not installed\n", study),
         "Did you run installStudy()?\n")
  }

  elementsDirectory <- file.path(onDirectory, elements)
  if (!dir.exists(elementsDirectory)) {
    if (!quiet) message(sprintf("Study \"%s\" does not have any elements named \"%s\"",
                                study, elements))
    return(list())
  }
  fileType <- match.arg(fileType, c("txt", "json"))
  elementsFiles <- getFiles(elementsDirectory, fileType = fileType)

  if (isEmpty(elementsFiles)) {
    if (!quiet) message(sprintf("Study \"%s\" does not have any elements named \"%s\"",
                                study, elements))
    return(list())
  }

  filters <- Filter(function(x) !is.null(x), filters)

  for (i in seq_along(filters)) {
    filterName <- names(filters)[i]
    filterValue <- filters[[i]]
    namesCurrent <- names(elementsFiles)
    if (filterValue %in% namesCurrent) {
      elementsFiles <- elementsFiles[[filterValue]]
    } else if (!is.null(default) && default %in% namesCurrent) {
      if (!quiet) message(sprintf("Returning \"%s\" %s for %s \"%s\"",
                                  default, elements, filterName, filterValue))
      elementsFiles <- elementsFiles[[default]]
    } else {
      if (!quiet) message(sprintf("No %s available for %s \"%s\"",
                                  elements, filterName, filterValue))
      return(list())
    }
  }

  readFunction <- if (fileType == "txt") readTable else readJson
  if (is.list(elementsFiles)) {
    object <- rapply(elementsFiles, readFunction, how = "replace", ...)
  } else {
    object <- readFunction(elementsFiles, ...)
  }

  return(object)
}

getDirectory <- function(study, libraries = NULL) {
  system.file("OmicNavigator/",
              package = paste0(getPrefix(), study),
              lib.loc = libraries)
}

getFiles <- function(path, fileType = "txt") {
  if (dir.exists(path)) {
    contents <- list.files(path, full.names = TRUE)
    contentsNames <- basename(contents)
    extensionRegEx <- sprintf("\\.%s$", fileType)
    contentsNames <- sub(extensionRegEx, "", contentsNames)
    stats::setNames(lapply(contents, getFiles, fileType = fileType), contentsNames)
  } else {
    path
  }
}
