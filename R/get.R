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

  models <- df_models[["description"]]
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

#' Get inference results from a study
#'
#' @export
getInferences <- function(study, modelID = NULL, contrastID = NULL, ...) {
  if (is.null(modelID) && !is.null(contrastID)) {
    stop("Must specify a model in order to specify a contrast")
  }

  UseMethod("getInferences")
}

#' @rdname getInferences
#' @export
getInferences.oaStudy <- function(study, modelID = NULL, contrastID = NULL, ...) {
  inferences <- study[["inferences"]]

  if (is.null(inferences)) {
    stop(sprintf("No Inferences available for study \"%s\"", study[["name"]]))
  }

  if (!is.null(modelID)) {
    stopifnot(is.character(modelID), length(modelID) == 1)
    if (!modelID %in% names(inferences)) {
      stop(sprintf("No inference results available for model \"%s\"", modelID))
    }
    inferences <- inferences[[modelID]]
  }

  if (!is.null(contrastID)) {
    stopifnot(is.character(contrastID), length(contrastID) == 1)
    if (!contrastID %in% names(inferences)) {
      stop(sprintf("No inference results available for contrast \"%s\" for model \"%s\"",
                   contrastID, modelID))
    }
    inferences <- inferences[[contrastID]]
  }

  return(inferences)
}

#' @rdname getInferences
#' @export
getInferences.SQLiteConnection <- function(study, modelID = NULL, contrastID = NULL, ...) {

  df_inferences <- dplyr::tbl(study, "inferences")
  if (!is.null(modelID)) {
    stopifnot(is.character(modelID), length(modelID) == 1)
    df_inferences <- dplyr::filter(df_inferences, modelID == !! modelID)
  }
  if (!is.null(contrastID)) {
    stopifnot(is.character(contrastID), length(contrastID) == 1)
    df_inferences <- dplyr::filter(df_inferences, contrastID == !! contrastID)
  }
  df_inferences <- dplyr::collect(df_inferences)
  if (nrow(df_inferences) == 0) {
    stop(sprintf("Invalid filters.\nmodelID: \"%s\"\ncontrastID: \"%s\"",
                 modelID, contrastID))
  }

  inferences <- splitTableIntoList(df_inferences, "modelID")
  inferences <- lapply(inferences, function(x) splitTableIntoList(x, "contrastID"))
  if (!is.null(modelID)) inferences <- inferences[[1]]
  if (!is.null(contrastID)) inferences <- inferences[[1]]

  return(inferences)
}

#' @rdname getInferences
#' @export
getInferences.character <- function(study, modelID = NULL, contrastID = NULL, libraries = NULL, ...) {
  con <- connectDatabase(study, libraries = libraries)
  on.exit(disconnectDatabase(con))

  inferences <- getInferences(con, modelID = modelID, contrastID = contrastID)

  return(inferences)
}

#' @export
getInferences.default <- function(study, modelID = NULL, contrastID = NULL, ...) {
  stop(sprintf("No method for object of class \"%s\"", class(study)))
}

# Wrapper around base::split()
splitTableIntoList <- function(dataFrame, columnName) {

  splitVariable <- dataFrame[[columnName]]
  splitData <- dataFrame
  splitData[[columnName]] <- NULL

  return(split(splitData, splitVariable))
}
