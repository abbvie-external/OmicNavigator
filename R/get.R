
#' @export
getModels <- function(study, model = NULL) {
  UseMethod("getModels")
}

#' @export
getModels.oaStudy <- function(study, model = NULL) {
  if (is.null(study[["models"]])) {
    stop(sprintf("No models available for study \"%s\"", study[["name"]]))
  }

  return(study[["models"]])
}

#' @export
getModels.SQLiteConnection <- function(study, model = NULL) {

  df_models <- study %>%
    dplyr::tbl("models") %>%
    dplyr::collect()

  models <- df_models[["description"]]
  names(models) <- df_models[["modelID"]]
  return(models)
}

#' @export
getModels.character <- function(study, model = NULL, libraries = NULL) {

  pkg <- paste0("OAstudy", study)
  location <- find.package(pkg, lib.loc = libraries, quiet = TRUE)
  if (length(location) == 0) {
    stop(sprintf("Unable to find study package for \"%s\"", study))
  }

  dbName <- file.path("OmicAnalyzer", paste0(study, ".sqlite"))
  db <- system.file(dbName, package = pkg, mustWork = TRUE)

  con <- DBI::dbConnect(RSQLite::SQLite(), db)
  on.exit(DBI::dbDisconnect(con))

  models <- getModels(con, model = model)

  return(models)
}

#' @export
getModels.default <- function(study, model = NULL) {
  stop(sprintf("No method for object of class \"%s\"", class(study)))
}

#' @export
getInferences <- function(study, model = NULL, contrast = NULL) {
  UseMethod("getInferences")
}

#' @export
getInferences.oaStudy <- function(study, model = NULL, contrast = NULL) {
  if (is.null(study[["inferences"]])) {
    stop(sprintf("No Inferences available for study \"%s\"", study[["name"]]))
  }

  return(study[["inferences"]])
}

#' @export
getInferences.SQLiteConnection <- function(study, model = NULL, contrast = NULL) {

  df_inferences <- study %>%
    dplyr::tbl("inferences") %>%
    dplyr::collect()

  inferences <- splitTableIntoList(df_inferences, "modelID")
  inferences <- lapply(inferences, function(x) splitTableIntoList(x, "contrastID"))

  return(inferences)
}

#' @export
getInferences.character <- function(study, model = NULL, contrast = NULL, libraries = NULL) {

  pkg <- paste0("OAstudy", study)
  location <- find.package(pkg, lib.loc = libraries, quiet = TRUE)
  if (length(location) == 0) {
    stop(sprintf("Unable to find study package for \"%s\"", study))
  }

  dbName <- file.path("OmicAnalyzer", paste0(study, ".sqlite"))
  db <- system.file(dbName, package = pkg, mustWork = TRUE)

  con <- DBI::dbConnect(RSQLite::SQLite(), db)
  on.exit(DBI::dbDisconnect(con))

  inferences <- getInferences(con, model = model, contrast = contrast)

  return(inferences)
}

#' @export
getInferences.default <- function(study, model = NULL, contrast = NULL) {
  stop(sprintf("No method for object of class \"%s\"", class(study)))
}

# Wrapper around base::split()
splitTableIntoList <- function(dataFrame, columnName) {

  splitVariable <- dataFrame[[columnName]]
  splitData <- dataFrame
  splitData[[columnName]] <- NULL

  return(split(splitData, splitVariable))
}
