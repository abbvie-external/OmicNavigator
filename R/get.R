#' Get installed OmicAnalyzer studies
#'
#' @param libraries Character vector of library directories to search for study
#'   packages. If \code{NULL}, uses \code{.libPaths}.
#'
#' @return character vector of OmicAnalyzer study packages
#'
#' @examples
#'  getInstalledStudies()
#'
#' @export
getInstalledStudies <- function(libraries = NULL) {
  pkgs_all <- rownames(utils::installed.packages(lib.loc = libraries))
  names(pkgs_all) <- NULL
  pkgs_oa <- grep("^OAstudy", pkgs_all, value = TRUE)
  studies <- sub("^OAstudy", "", pkgs_oa)
  studies <- sort(studies)

  return(studies)
}

#' Get models from a study
#'
#' @inheritParams shared-get
#'
#' @export
getModels <- function(study, modelID = NULL, ...) {
  UseMethod("getModels")
}

#' @rdname getModels
#' @export
getModels.oaStudy <- function(study, modelID = NULL, ...) {
  models <- study[["models"]]

  if (isEmpty(models)) {
    stop(sprintf("No models available for study \"%s\"", study[["name"]]))
  }

  if (!is.null(modelID)) {
    stopifnot(is.character(modelID), length(modelID) == 1)
    if (!modelID %in% names(models)) {
      stop(sprintf("Study \"%s\" has no model \"%s\"", study[["name"]], modelID))
    }
    models <- models[modelID]
  }

  return(models)
}

#' @rdname getModels
#' @inheritParams listStudies
#' @export
getModels.character <- function(study, modelID = NULL, libraries = NULL, ...) {
  oaDirectory <- system.file("OmicAnalyzer/",
                             package = paste0("OAstudy", study),
                             lib.loc = libraries)
  if (oaDirectory == "") {
    stop(sprintf("The study \"%s\" is not installed\n", study),
         "Did you run installStudy()?\n")
  }
  elementsFile <- file.path(oaDirectory, "models.json")
  if (!file.exists(elementsFile)) {
    stop(sprintf("No models for study \"%s\"", study))
  }
  models <- readJson(elementsFile)

  if(is.null(modelID)) return(models)

  if (!is.character(modelID) || length(modelID) != 1) {
    stop("modelID must be a length one character vector")
  }

  modelsAvailable <- names(models)
  if (!modelID %in% modelsAvailable) {
    stop(sprintf("The modelID \"%s\" is not available for study \"%s\"",
                 modelID, study))
  }

  return(models[modelID])
}

#' @export
getModels.default <- function(study, modelID = NULL, ...) {
  stop(sprintf("No method for object of class \"%s\"", class(study)))
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
#' @export
getFeatures <- function(study, modelID = NULL, libraries = NULL) {
  getElements(
    study,
    elements = "features",
    filters = list(modelID = modelID),
    default = "default",
    libraries = libraries
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
    libraries = libraries
  )
}

#' Get tests from a study
#'
#' @inheritParams shared-get
#' @inheritParams listStudies
#'
#' @export
getTests <- function(study, modelID = NULL, libraries = NULL) {
  getElements(
    study,
    elements = "tests",
    filters = list(modelID = modelID),
    default = "default",
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
#' @export
getResultsTable <- function(study, modelID, testID, libraries = NULL) {
  results <- getResults(study, modelID, testID)
  features <- getFeatures(study, modelID)

  resultsTable <- merge(features, results, by = 1,
                        all.x = FALSE, all.y = TRUE, sort = FALSE)

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
#'
#' @export
getEnrichmentsNetwork <- function(study, modelID, annotationID, ...) {
  UseMethod("getEnrichmentsNetwork")
}

#' @rdname getEnrichmentsNetwork
#' @importFrom dplyr "%>%"
#' @importFrom rlang "!!"
#' @importFrom rlang ".data"
#' @export
getEnrichmentsNetwork.SQLiteConnection <- function(study, modelID, annotationID, ...) {

  terms <- dplyr::tbl(study, "terms") %>%
    dplyr::filter(.data$annotationID == !! annotationID) %>%
    dplyr::group_by(.data$termID) %>%
    dplyr::tally(name = "geneSetSize") %>%
    dplyr::collect()

  enrichments <- getEnrichments(study, modelID = modelID, annotationID = annotationID)
  enrichmentsTable <- combineListIntoTable(enrichments, "testID")

  nodesLong <- enrichmentsTable %>%
    dplyr::left_join(terms, by = "termID") %>%
    dplyr::arrange(.data$testID)

  tests <- unique(nodesLong[["testID"]])

  if (nrow(nodesLong) == 0) {
    stop("Invalid filters.\n",
         if (is.null(modelID)) "modelID: No filter applied\n"
         else sprintf("modelID: \"%s\"\n", modelID),
         if (is.null(annotationID)) "annotationID: No filter applied\n"
         else sprintf("annotationID: \"%s\"\n", annotationID)
    )
  }

  nodes <- nodesLong %>%
    dplyr::group_by(.data$termID, .data$description, .data$geneSetSize) %>%
    dplyr::summarize(nominal = list(.data$nominal), adjusted = list(.data$adjusted)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(id = seq_len(dplyr::n())) %>%
    dplyr::select(.data$id, .data$termID, .data$description, .data$geneSetSize,
                  .data$nominal, .data$adjusted) %>%
    as.data.frame()

  links <- dplyr::tbl(study, "overlaps") %>%
    dplyr::filter(.data$annotationID ==  !! annotationID) %>%
    dplyr::select(-.data$annotationID) %>%
    dplyr::arrange(.data$term1, .data$term2) %>%
    dplyr::rename(source = .data$term1, target = .data$term2) %>%
    dplyr::collect() %>%
    dplyr::semi_join(nodes, by = c("source" = "termID")) %>%
    dplyr::semi_join(nodes, by = c("target" = "termID")) %>%
    dplyr::mutate(id = seq_len(dplyr::n())) %>%
    dplyr::select(.data$id, dplyr::everything()) %>%
    as.data.frame()

  # Use node IDs with links
  links[["source"]] <- match(links[["source"]], nodes[["termID"]])
  links[["target"]] <- match(links[["target"]], nodes[["termID"]])

  enrichmentsNetwork <- list(tests = tests, nodes = nodes, links = links)

  return(enrichmentsNetwork)
}

#' @rdname getEnrichmentsNetwork
#' @inheritParams listStudies
#' @export
getEnrichmentsNetwork.character <- function(study, modelID, annotationID, libraries = NULL, ...) {
  con <- connectDatabase(study, libraries = libraries)
  on.exit(disconnectDatabase(con))

  enrichments <- getEnrichmentsNetwork(con, modelID = modelID, annotationID = annotationID, ...)

  return(enrichments)
}

#' @export
getEnrichmentsNetwork.default <- function(study, modelID, annotationID, ...) {
  if (inherits(study, "oaStudy")) {
    stop("The Enrichment Network is only available for study packages or databases")
  }

  stop(sprintf("No method for object of class \"%s\"", class(study)))
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
    libraries = libraries
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
    if (length(filters) > 0) !is.null(names(filters))
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
  oaDirectory <- system.file("OmicAnalyzer/",
                             package = paste0("OAstudy", study),
                             lib.loc = libraries)
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
    object <- rapply(elementsFiles, readFunction, how = "replace")
  } else {
    object <- readFunction(elementsFiles)
  }

  return(object)
}

getFiles <- function(path, fileType = "txt") {
  if (dir.exists(path)) {
    contents <- list.files(path, full.names = TRUE)
    contentsNames <- basename(contents)
    extensionRegEx <- sprintf("\\.%s$", fileType)
    contentsNames <- sub(extensionRegEx, "", contentsNames)
    stats::setNames(lapply(contents, getFiles), contentsNames)
  } else {
    path
  }
}
