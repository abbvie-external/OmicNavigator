#' OmicAnalyzer
#'
#' @examples
#' \dontrun{
#'
#' oa <- OmicAnalyzer()
#' }
#'
#' @export
OmicAnalyzer <- function(path = NULL, libraries = .libPaths()) {

  if (is.null(path)) {
    path <- rappdirs::user_data_dir("OmicAnalyzer")
  }

  if (!dir.exists(path)) {
    dir.create(path, recursive = TRUE)
  }

  result <- list(path = path, libraries = libraries)
  class(result) <- "OmicAnalyzer"
  return(result)
}

#' @export
print.OmicAnalyzer <- function(x, ...) {

  cat("== OmicAnalyzer ==\n")
  cat(sprintf("Database directory: %s\n", x$path))
  databases <- find_databases(x$path)
  cat(sprintf("Found %d study databases\n", length(databases)))

  return(invisible(x))
}

find_databases <- function(path) {
  stopifnot(dir.exists(path))

  databases <- list.files(path = path, pattern = "sqlite$")

  return(databases)
}

#' addStudy
#'
#' @examples
#' \dontrun{
#'
#' oa <- OmicAnalyzer()
#' addStudy(oa, "nameOfStudy")
#' }
#' @export
addStudy <- function(OmicAnalyzer, studyID) {
  stopifnot(inherits(OmicAnalyzer, "OmicAnalyzer"))
  stopifnot(is.character(studyID), length(studyID) == 1)

  dbFile <- paste0(studyID, ".sqlite")
  databases <- find_databases(OmicAnalyzer$path)
  if (!dbFile %in% databases) {
    message(sprintf("Adding new study \"%s\"", studyID))
  } else {
    message(sprintf("Study \"%s\" already exists", studyID))
  }
  dbPath <- file.path(OmicAnalyzer$path, dbFile)
  con <- DBI::dbConnect(RSQLite::SQLite(), dbPath)
  DBI::dbDisconnect(con)

  return(invisible(dbPath))
}

#' @importFrom dplyr "%>%"
#' @export
addModel <- function(OmicAnalyzer, studyID, modelID, description) {
  stopifnot(inherits(OmicAnalyzer, "OmicAnalyzer"))
  stopifnot(is.character(studyID), length(studyID) == 1)
  stopifnot(is.character(modelID), length(modelID) == 1)
  stopifnot(is.character(description), length(description) == 1)

  dbFile <- paste0(studyID, ".sqlite")
  databases <- find_databases(OmicAnalyzer$path)
  if (!dbFile %in% databases) {
    stop(sprintf("Study \"%s\" does not exist. Did you run addStudy()?", studyID))
  }
  dbPath <- file.path(OmicAnalyzer$path, dbFile)
  con <- DBI::dbConnect(RSQLite::SQLite(), dbPath)
  on.exit(DBI::dbDisconnect(con))

  models <- data.frame(modelID = modelID, description = description,
                       stringsAsFactors = FALSE)

  table_name <- "models"
  fields <- c("modelID" = "varchar(100) PRIMARY KEY",
              "description" = "varchar(200)")
  if (DBI::dbExistsTable(con, table_name)) {
    models_current <- dplyr::tbl(con, table_name) %>%
      dplyr::select(modelID) %>%
      dplyr::collect()
    if (modelID %in% models_current$modelID) {
      stop(sprintf("Model \"%s\" already exists", modelID))
    }
    DBI::dbWriteTable(con, table_name, models,
                      overwrite = FALSE, append = TRUE)
  } else {
    DBI::dbWriteTable(con, table_name, models, field.types = fields)
  }

  return(invisible(modelID))
}

# queryDatabase(oa, "example", "SELECT modelID FROM models")
queryDatabase <- function(OmicAnalyzer, studyID, query) {
  dbFile <- paste0(studyID, ".sqlite")
  databases <- find_databases(OmicAnalyzer$path)
  if (!dbFile %in% databases) {
    stop(sprintf("Study \"%s\" does not exist. Did you run addStudy()?", studyID))
  }
  dbPath <- file.path(OmicAnalyzer$path, dbFile)
  con <- DBI::dbConnect(RSQLite::SQLite(), dbPath)
  on.exit(DBI::dbDisconnect(con))

  DBI::dbGetQuery(con, query)
}
