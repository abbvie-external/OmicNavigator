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

#' @export
addFeatures <- function(OmicAnalyzer, studyID, features, featureID = "featureID") {
  stopifnot(inherits(OmicAnalyzer, "OmicAnalyzer"))
  stopifnot(is.character(studyID), length(studyID) == 1)
  stopifnot(inherits(features, "data.frame"))
  stopifnot(is.character(featureID), length(featureID) == 1)

  if (!featureID %in% colnames(features)) {
    stop(sprintf("features does not have the column \"%s\"", featureID))
  }

  is_unique <- length(features[[featureID]]) ==
               length(unique(features[[featureID]]))
  if (!is_unique) {
    stop(sprintf("The IDs in column \"%s\" are not unique", featureID))
  }

  dbFile <- paste0(studyID, ".sqlite")
  databases <- find_databases(OmicAnalyzer$path)
  if (!dbFile %in% databases) {
    stop(sprintf("Study \"%s\" does not exist. Did you run addStudy()?", studyID))
  }
  dbPath <- file.path(OmicAnalyzer$path, dbFile)
  con <- DBI::dbConnect(RSQLite::SQLite(), dbPath)
  on.exit(DBI::dbDisconnect(con))

  DBI::dbWriteTable(con, "features", features, overwrite = TRUE,
                    field.types = c("featureID" = "varchar(50) PRIMARY KEY"))

  DBI::dbExecute(con,
                 "CREATE UNIQUE INDEX feature_index ON features(featureID)")
  return(invisible(OmicAnalyzer))
}

#' @export
addSamples <- function(OmicAnalyzer, studyID, samples, sampleID = "sampleID") {
  stopifnot(inherits(OmicAnalyzer, "OmicAnalyzer"))
  stopifnot(is.character(studyID), length(studyID) == 1)
  stopifnot(inherits(samples, "data.frame"))
  stopifnot(is.character(sampleID), length(sampleID) == 1)

  if (!sampleID %in% colnames(samples)) {
    stop(sprintf("features does not have the column \"%s\"", sampleID))
  }

  is_unique <- length(samples[[sampleID]]) ==
               length(unique(samples[[sampleID]]))
  if (!is_unique) {
    stop(sprintf("The IDs in column \"%s\" are not unique", sampleID))
  }

  dbFile <- paste0(studyID, ".sqlite")
  databases <- find_databases(OmicAnalyzer$path)
  if (!dbFile %in% databases) {
    stop(sprintf("Study \"%s\" does not exist. Did you run addStudy()?", studyID))
  }
  dbPath <- file.path(OmicAnalyzer$path, dbFile)
  con <- DBI::dbConnect(RSQLite::SQLite(), dbPath)
  on.exit(DBI::dbDisconnect(con))

  DBI::dbWriteTable(con, "samples", samples, overwrite = TRUE,
                    field.types = c("sampleID" = "varchar(50) PRIMARY KEY"))

  DBI::dbExecute(con,
                 "CREATE UNIQUE INDEX sample_indx ON samples(sampleID)")
  return(invisible(OmicAnalyzer))
}

#' @importFrom dplyr "%>%"
#' @export
addAssay <- function(OmicAnalyzer, studyID, modelID, assay) {
  stopifnot(inherits(OmicAnalyzer, "OmicAnalyzer"))
  stopifnot(is.character(studyID), length(studyID) == 1)
  stopifnot(is.character(modelID), length(modelID) == 1)
  stopifnot(inherits(assay, "data.frame") || is.matrix(assay))

  dbFile <- paste0(studyID, ".sqlite")
  databases <- find_databases(OmicAnalyzer$path)
  if (!dbFile %in% databases) {
    stop(sprintf("Study \"%s\" does not exist. Did you run addStudy()?", studyID))
  }
  dbPath <- file.path(OmicAnalyzer$path, dbFile)
  con <- DBI::dbConnect(RSQLite::SQLite(), dbPath)
  on.exit(DBI::dbDisconnect(con))

  sampleID_all <- dplyr::tbl(con, "samples") %>%
    dplyr::select(sampleID) %>%
    dplyr::collect() %>%
    unlist()

  if (!all(colnames(assay) %in% sampleID_all)) {
    stop("The column names contain samples not in the samples table")
  }

  featureID_all <- dplyr::tbl(con, "features") %>%
    dplyr::select(featureID) %>%
    dplyr::collect() %>%
    unlist()

  if (!all(rownames(assay) %in% featureID_all)) {
    stop("The row names contain features not in the features table")
  }

  assay_long <- assay %>%
    as.data.frame %>%
    dplyr::mutate(featureID = rownames(.)) %>%
    tidyr::pivot_longer(cols = -featureID,
                 names_to = "sampleID",
                 values_to = "quantification") %>%
    dplyr::mutate(modelID = modelID) %>%
    dplyr::select(featureID, sampleID, modelID, quantification)

  DBI::dbWriteTable(con, "assays", assay_long,
                    field.types = c(
                      "featureID" = "varchar(50) REFERENCES features (featureID)",
                      "sampleID" = "varchar(50) REFERENCES samples (sampleID)",
                      "modelID" = "varchar(100) REFERENCES models (modelID)"
                    ))

  DBI::dbExecute(con,
                 "CREATE UNIQUE INDEX assays_index ON assays(featureID, sampleID, modelID)")

  return(invisible(OmicAnalyzer))
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
