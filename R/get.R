#' Get installed OmicAnalyzer studies
#'
#' @param libraries Character vector of library directories to search for study
#'   packages. If \code{NULL}, uses \code{.libPaths}.
#'
#' @return character vector of OmicAnalyzer study packages
#'
#' @examples
#'  getStudies()
#'
#' @export
getStudies <- function(libraries = NULL) {
  pkgs_all <- rownames(utils::installed.packages(lib.loc = libraries))
  pkgs_oa <- grep("^OAstudy", pkgs_all, value = TRUE)
  studies <- sub("^OAstudy", "", pkgs_oa)

  return(studies)
}

#' Get models from a study
#'
#' @export
getModels <- function(study, modelID = NULL, ...) {
  UseMethod("getModels")
}

#' @rdname getModels
#' @export
getModels.oaStudy <- function(study, modelID = NULL, ...) {
  models <- study[["models"]]
  if (is.null(models)) {
    stop(sprintf("No models available for study \"%s\"", study[["name"]]))
  }

  if (!is.null(modelID)) {
    stopifnot(is.character(modelID), length(modelID) == 1)
    models <- models[modelID]
  }

  return(models)
}

#' @rdname getModels
#' @importFrom rlang "!!"
#' @export
getModels.SQLiteConnection <- function(study, modelID = NULL, ...) {

  df_models <- dplyr::tbl(study, "models")
  if (!is.null(modelID)) {
    stopifnot(is.character(modelID), length(modelID) == 1)
    df_models <- dplyr::filter(df_models, modelID == !! modelID)
  }
  df_models <- dplyr::collect(df_models)
  if (nrow(df_models) == 0) {
    stop(sprintf("Invalid modelID: \"%s\"", modelID))
  }

  models <- as.list(df_models[["description"]])
  names(models) <- df_models[["modelID"]]
  return(models)
}

#' @rdname getModels
#' @export
getModels.character <- function(study, modelID = NULL, libraries = NULL, ...) {
  con <- connectDatabase(study, libraries = libraries)
  on.exit(disconnectDatabase(con))

  models <- getModels(con, modelID = modelID)

  return(models)
}

#' @export
getModels.default <- function(study, modelID = NULL, ...) {
  stop(sprintf("No method for object of class \"%s\"", class(study)))
}

#' Get result results from a study
#'
#' @export
getResults <- function(study, modelID = NULL, contrastID = NULL, ...) {
  if (is.null(modelID) && !is.null(contrastID)) {
    stop("Must specify a model in order to specify a contrast")
  }

  UseMethod("getResults")
}

#' @rdname getResults
#' @export
getResults.oaStudy <- function(study, modelID = NULL, contrastID = NULL, ...) {
  results <- study[["results"]]

  if (is.null(results)) {
    stop(sprintf("No results available for study \"%s\"", study[["name"]]))
  }

  if (!is.null(modelID)) {
    stopifnot(is.character(modelID), length(modelID) == 1)
    if (!modelID %in% names(results)) {
      stop(sprintf("No results available for model \"%s\"", modelID))
    }
    results <- results[[modelID]]
  }

  if (!is.null(contrastID)) {
    stopifnot(is.character(contrastID), length(contrastID) == 1)
    if (!contrastID %in% names(results)) {
      stop(sprintf("No results available for contrast \"%s\" for model \"%s\"",
                   contrastID, modelID))
    }
    results <- results[[contrastID]]
  }

  return(results)
}

#' @rdname getResults
#' @importFrom rlang "!!"
#' @export
getResults.SQLiteConnection <- function(study, modelID = NULL, contrastID = NULL, ...) {

  df_results <- dplyr::tbl(study, "results")
  if (!is.null(modelID)) {
    stopifnot(is.character(modelID), length(modelID) == 1)
    df_results <- dplyr::filter(df_results, modelID == !! modelID)
  }
  if (!is.null(contrastID)) {
    stopifnot(is.character(contrastID), length(contrastID) == 1)
    df_results <- dplyr::filter(df_results, contrastID == !! contrastID)
  }
  df_results <- dplyr::collect(df_results)
  if (nrow(df_results) == 0) {
    stop(sprintf("Invalid filters.\nmodelID: \"%s\"\ncontrastID: \"%s\"",
                 modelID, contrastID))
  }

  results <- splitTableIntoList(df_results, "modelID")
  results <- lapply(results, function(x) splitTableIntoList(x, "contrastID"))
  if (!is.null(modelID)) results <- results[[1]]
  if (!is.null(contrastID)) results <- results[[1]]

  return(results)
}

#' @rdname getResults
#' @export
getResults.character <- function(study, modelID = NULL, contrastID = NULL, libraries = NULL, ...) {
  con <- connectDatabase(study, libraries = libraries)
  on.exit(disconnectDatabase(con))

  results <- getResults(con, modelID = modelID, contrastID = contrastID)

  return(results)
}

#' @export
getResults.default <- function(study, modelID = NULL, contrastID = NULL, ...) {
  stop(sprintf("No method for object of class \"%s\"", class(study)))
}

# Wrapper around base::split()
splitTableIntoList <- function(dataFrame, columnName) {

  splitVariable <- dataFrame[[columnName]]
  splitData <- dataFrame
  splitData[[columnName]] <- NULL

  return(split(splitData, splitVariable))
}
