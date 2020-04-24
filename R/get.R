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
  pkgs_oa <- grep("^OAstudy", pkgs_all, value = TRUE)
  studies <- sub("^OAstudy", "", pkgs_oa)
  studies <- sort(studies)

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
getResults <- function(study, modelID = NULL, testID = NULL, ...) {
  if (is.null(modelID) && !is.null(testID)) {
    stop("Must specify a model in order to specify a test")
  }

  UseMethod("getResults")
}

#' @rdname getResults
#' @export
getResults.oaStudy <- function(study, modelID = NULL, testID = NULL, ...) {
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

  if (!is.null(testID)) {
    stopifnot(is.character(testID), length(testID) == 1)
    if (!testID %in% names(results)) {
      stop(sprintf("No results available for test \"%s\" for model \"%s\"",
                   testID, modelID))
    }
    results <- results[[testID]]
  }

  return(results)
}

#' @rdname getResults
#' @importFrom rlang "!!"
#' @export
getResults.SQLiteConnection <- function(study, modelID = NULL, testID = NULL, ...) {

  df_results <- dplyr::tbl(study, "results")
  if (!is.null(modelID)) {
    stopifnot(is.character(modelID), length(modelID) == 1)
    df_results <- dplyr::filter(df_results, modelID == !! modelID)
  }
  if (!is.null(testID)) {
    stopifnot(is.character(testID), length(testID) == 1)
    df_results <- dplyr::filter(df_results, testID == !! testID)
  }
  df_results <- dplyr::collect(df_results)
  if (nrow(df_results) == 0) {
    stop(sprintf("Invalid filters.\nmodelID: \"%s\"\ntestID: \"%s\"",
                 modelID, testID))
  }

  results <- splitTableIntoList(df_results, "modelID")
  results <- lapply(results, function(x) splitTableIntoList(x, "testID"))
  if (!is.null(modelID)) results <- results[[1]]
  if (!is.null(testID)) results <- results[[1]]

  return(results)
}

#' @rdname getResults
#' @export
getResults.character <- function(study, modelID = NULL, testID = NULL, libraries = NULL, ...) {
  con <- connectDatabase(study, libraries = libraries)
  on.exit(disconnectDatabase(con))

  results <- getResults(con, modelID = modelID, testID = testID)

  return(results)
}

#' @export
getResults.default <- function(study, modelID = NULL, testID = NULL, ...) {
  stop(sprintf("No method for object of class \"%s\"", class(study)))
}

# Wrapper around base::split()
splitTableIntoList <- function(dataFrame, columnName) {

  splitVariable <- dataFrame[[columnName]]
  splitData <- dataFrame
  splitData[[columnName]] <- NULL

  return(split(splitData, splitVariable))
}
