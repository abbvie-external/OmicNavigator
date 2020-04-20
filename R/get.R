
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
