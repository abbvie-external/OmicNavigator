#' Get installed OmicNavigator studies
#'
#' @param libraries Character vector of library directories to search for study
#'   packages. If \code{NULL}, uses \code{.libPaths}.
#'
#' @return character vector of OmicNavigator study packages
#'
#' @examples
#'  getInstalledStudies()
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

#' Shared parameters for get functions
#'
#' @name shared-get
#'
#' @param study An OmicNavigator study. Either an object of class \code{oaStudy},
#'   or the name of an installed study package.
#' @param modelID Filter by modelID
#' @param testID Filter by testID
#' @param annotationID Filter by annotation
#' @param termID Filter by termID
#' @param featureID Filter by featureID
#' @param plotID Filter by plotID
#'
#' @keywords internal
NULL

#' Get models from a study
#'
#' @inheritParams shared-get
#' @inheritParams listStudies
#'
#' @export
getModels <- function(study, modelID = NULL, libraries = NULL) {
  getElements(
    study,
    elements = "models",
    filters = list(modelID = modelID),
    fileType = "json",
    libraries = libraries
  )
}

#' Get samples from a study
#'
#' @inheritParams shared-get
#' @inheritParams listStudies
#'
#' @export
getSamples <- function(study, modelID = NULL, libraries = NULL) {
  getElements(
    study,
    elements = "samples",
    filters = list(modelID = modelID),
    default = "default",
    libraries = libraries
  )
}

#' Get features from a study
#'
#' @inheritParams shared-get
#' @inheritParams listStudies
#'
#' @return A data frame (if \code{modelID} is specified) or a list of data
#'   frames. All the columns will be character strings, even if the values
#'   appear numeric.
#'
#' @export
getFeatures <- function(study, modelID = NULL, libraries = NULL) {
  getElements(
    study,
    elements = "features",
    filters = list(modelID = modelID),
    default = "default",
    libraries = libraries,
    colClasses = "character"
  )
}

#' Get assays from a study
#'
#' @inheritParams shared-get
#' @inheritParams listStudies
#'
#' @export
getAssays <- function(study, modelID = NULL, libraries = NULL) {
  getElements(
    study,
    elements = "assays",
    filters = list(modelID = modelID),
    default = "default",
    libraries = libraries,
    hasRowNames = TRUE
  )
}

#' Get tests from a study
#'
#' @inheritParams shared-get
#' @inheritParams listStudies
#'
#' @export
getTests <- function(study, modelID = NULL, testID = NULL, libraries = NULL) {
  if (is.null(modelID) && !is.null(testID)) {
    stop("Must specify a model in order to specify a test")
  }

  getElements(
    study,
    elements = "tests",
    filters = list(modelID = modelID, testID = testID),
    default = "default",
    fileType = "json",
    libraries = libraries
  )
}

#' Get annotations from a study
#'
#' @inheritParams shared-get
#' @inheritParams listStudies
#'
#' @export
getAnnotations <- function(study, annotationID = NULL, libraries = NULL) {
  getElements(
    study,
    elements = "annotations",
    filters = list(annotationID = annotationID),
    fileType = "json",
    libraries = libraries
  )
}

#' Get overlaps from a study
#'
#' @inheritParams shared-get
#' @inheritParams listStudies
#'
#' @export
getOverlaps <- function(study, annotationID = NULL, libraries = NULL) {
  getElements(
    study,
    elements = "overlaps",
    filters = list(annotationID = annotationID),
    libraries = libraries
  )
}

#' Get results from a study
#'
#' @inheritParams shared-get
#' @inheritParams listStudies
#'
#' @export
getResults <- function(study, modelID = NULL, testID = NULL, libraries = NULL) {
  if (is.null(modelID) && !is.null(testID)) {
    stop("Must specify a model in order to specify a test")
  }

  getElements(
    study,
    elements = "results",
    filters = list(modelID = modelID, testID = testID),
    libraries = libraries
  )
}

#' Get results table from a study
#'
#' @inheritParams shared-get
#' @inheritParams listStudies
#'
#' @return A data frame which includes the columns from the features table
#'   followed by the columns from the results table. All the columns from the
#'   features table will be character strings, even if the values appear
#'   numeric.
#'
#' @export
getResultsTable <- function(study, modelID, testID, libraries = NULL) {
  results <- getResults(study, modelID, testID)
  features <- getFeatures(study, modelID)

  # Results must be first argument to preserve input order
  resultsTable <- merge(results, features, by = 1,
                        all.x = TRUE, all.y = FALSE, sort = FALSE)
  # Rearrange columns so that features are listed first
  columnsOrder <- c(colnames(features),
                    setdiff(colnames(results), colnames(features)))
  resultsTable <- resultsTable[, columnsOrder]
  # featureID column must be character
  resultsTable[, 1] <- as.character(resultsTable[, 1])

  return(resultsTable)
}

#' Get enrichments from a study
#'
#' @inheritParams shared-get
#' @inheritParams listStudies
#'
#' @export
getEnrichments <- function(study, modelID = NULL, annotationID = NULL, testID = NULL, libraries = NULL) {
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
    libraries = libraries
  )
}

#' Get enrichments table from a study
#'
#' @inheritParams shared-get
#' @inheritParams shared-upset
#' @inheritParams listStudies
#'
#' @export
getEnrichmentsTable <- function(study, modelID, annotationID, type = "nominal", libraries = NULL) {
  stopifnot(type %in% c("nominal", "adjusted"))

  enrichments <- getEnrichments(
    study,
    modelID = modelID,
    annotationID = annotationID,
    libraries = libraries
  )

  enrichmentsTable <- combineListIntoTable(enrichments, "testID")

  enrichmentsTableWide <- enrichmentsToWide(enrichmentsTable, type = type)

  return(enrichmentsTableWide)
}

#' Get enrichments network from a study
#'
#' @inheritParams shared-get
#' @inheritParams listStudies
#'
#' @export
getEnrichmentsNetwork <- function(study, modelID, annotationID, libraries = NULL) {
  if (inherits(study, "oaStudy")) {
    stop("The Enrichment Network is only available for study packages")
  }

  annotation <- getAnnotations(
    study,
    annotationID = annotationID,
    libraries = libraries
  )

  terms <- lengths(annotation[["terms"]])
  terms <- data.frame(
    termID = names(terms),
    geneSetSize = terms,
    stringsAsFactors = FALSE
  )

  enrichments <- getEnrichments(study, modelID = modelID, annotationID = annotationID)
  enrichmentsTable <- combineListIntoTable(enrichments, "testID")

  nodesLong <- merge(enrichmentsTable, terms, by = "termID",
                     all.x = TRUE, all.y = FALSE, sort = FALSE)
  nodesLong <- nodesLong[order(nodesLong[["testID"]]), ]

  tests <- unique(nodesLong[["testID"]])

  nodes <- stats::aggregate(
    cbind(nominal, adjusted) ~ termID + description + geneSetSize,
    data = nodesLong,
    FUN = list
  )
  nodes <- cbind(id = seq_len(nrow(nodes)), nodes)

  overlaps <- getOverlaps(
    study,
    annotationID = annotationID,
    libraries = libraries
  )

  links <- overlaps
  colnames(links)[1:2] <- c("source", "target")
  links <- links[links[["source"]] %in% nodes[["termID"]] &
                 links[["target"]] %in% nodes[["termID"]], ]
  links <- cbind(id = seq_len(nrow(links)), links)

  # Use node IDs with links
  links[["source"]] <- match(links[["source"]], nodes[["termID"]])
  links[["target"]] <- match(links[["target"]], nodes[["termID"]])

  enrichmentsNetwork <- list(tests = tests, nodes = nodes, links = links)

  return(enrichmentsNetwork)
}

#' Get metaFeatures from a study
#'
#' @inheritParams shared-get
#' @inheritParams listStudies
#'
#' @export
getMetaFeatures <- function(study, modelID = NULL, libraries = NULL) {
  getElements(
    study,
    elements = "metaFeatures",
    filters = list(modelID = modelID),
    default = "default",
    libraries = libraries,
    colClasses = "character"
  )
}

#' Get plots from a study
#'
#' @inheritParams shared-get
#' @inheritParams listStudies
#'
#' @export
getPlots <- function(study, modelID = NULL, libraries = NULL) {
  getElements(
    study,
    elements = "plots",
    filters = list(modelID = modelID),
    default = "default",
    fileType = "json",
    libraries = libraries
  )
}

#' Get barcodes from a study
#'
#' @inheritParams shared-get
#' @inheritParams listStudies
#'
#' @export
getBarcodes <- function(study, modelID = NULL, libraries = NULL) {
  getElements(
    study,
    elements = "barcodes",
    filters = list(modelID = modelID),
    default = "default",
    fileType = "json",
    libraries = libraries
  )
}

#' Get reports from a study
#'
#' @inheritParams shared-get
#' @inheritParams listStudies
#'
#' @export
getReports <- function(study, modelID = NULL, libraries = NULL) {
  getElements(
    study,
    elements = "reports",
    filters = list(modelID = modelID),
    default = "default",
    fileType = "json",
    libraries = libraries
  )
}

# Wrapper around base::split()
splitTableIntoList <- function(dataFrame, columnName) {

  splitVariable <- dataFrame[[columnName]]
  splitData <- dataFrame
  splitData[[columnName]] <- NULL

  result <- split(splitData, splitVariable)
  result <- lapply(result, function(x) {rownames(x) <- NULL; x})

  return(result)
}

combineListIntoTable <- function(listObj, newColumnName = "newColumnName") {
  stopifnot(
    is.list(listObj),
    length(listObj) > 0,
    is.character(newColumnName)
  )

  listNames <- names(listObj)
  newListObj <- listObj
  for (i in seq_along(listObj)) {
    newListObj[[i]][[newColumnName]] <- listNames[i]
  }

  names(newListObj) <- NULL # to avoid row names in output
  newTable <- do.call(rbind, newListObj)
  newColumnIndex <- ncol(newTable)
  newTable <- newTable[, c(newColumnIndex, seq_len(newColumnIndex - 1))]

  return(newTable)
}

# ... Arguments passed to either data.table::fread() or jsonlite::read_json()
getElements <- function(
  study,
  elements,
  filters = list(),
  default = NULL,
  fileType = c("txt", "json"),
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
  libraries = NULL,
  ...
)
{
  stop(sprintf("No method for object of class \"%s\"", class(study)))
}

#' @export
getElements.oaStudy <- function(
  study,
  elements,
  filters = list(),
  default = NULL,
  fileType = c("txt", "json"),
  libraries = NULL,
  ...
)
{
  elementsList <- study[[elements]]

  if (isEmpty(elementsList)) {
    stop(sprintf("No %s available for study \"%s\"", elements, study[["name"]]))
  }

  filters <- Filter(function(x) !is.null(x), filters)

  for (i in seq_along(filters)) {
    filterName <- names(filters)[i]
    filterValue <- filters[[i]]
    namesCurrent <- names(elementsList)
    if (filterValue %in% namesCurrent) {
      elementsList <- elementsList[[filterValue]]
    } else if (!is.null(default) && default %in% namesCurrent) {
      message(sprintf("Returning \"%s\" %s for %s \"%s\"",
                      default, elements, filterName, filterValue))
      elementsList <- elementsList[[default]]
    } else {
      stop(sprintf("No %s available for %s \"%s\"",
                   elements, filterName, filterValue))
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
  libraries = NULL,
  ...
)
{
  oaDirectory <- getDirectory(study, libraries)
  if (oaDirectory == "") {
    stop(sprintf("The study \"%s\" is not installed\n", study),
         "Did you run installStudy()?\n")
  }

  elementsDirectory <- file.path(oaDirectory, elements)
  if (!dir.exists(elementsDirectory)) {
    stop(sprintf("Study \"%s\" does not have any elements named \"%s\"",
                 study, elements), call. = FALSE)
  }
  fileType <- match.arg(fileType, c("txt", "json"))
  elementsFiles <- getFiles(elementsDirectory, fileType = fileType)

  if (isEmpty(elementsFiles)) {
    stop(sprintf("No \"%s\" available for this study", elements), call. = FALSE)
  }

  filters <- Filter(function(x) !is.null(x), filters)

  for (i in seq_along(filters)) {
    filterName <- names(filters)[i]
    filterValue <- filters[[i]]
    namesCurrent <- names(elementsFiles)
    if (filterValue %in% namesCurrent) {
      elementsFiles <- elementsFiles[[filterValue]]
    } else if (!is.null(default) && default %in% namesCurrent) {
      message(sprintf("Returning \"%s\" %s for %s \"%s\"",
                      default, elements, filterName, filterValue))
      elementsFiles <- elementsFiles[[default]]
    } else {
      stop(sprintf("No %s available for %s \"%s\"",
                   elements, filterName, filterValue))
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
