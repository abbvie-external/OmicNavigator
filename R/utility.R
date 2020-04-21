# For removing shared database columns that aren't relevant to the filtered
# result
is_available <- function(x) !all(is.na(x))

connectDatabase <- function(study, libraries = NULL) {

  pkg <- paste0("OAstudy", study)
  location <- find.package(pkg, lib.loc = libraries, quiet = TRUE)
  if (length(location) == 0) {
    stop(sprintf("Unable to find study package for \"%s\"", study))
  }

  dbName <- file.path("OmicAnalyzer", paste0(study, ".sqlite"))
  db <- system.file(dbName, package = pkg, mustWork = TRUE)

  con <- DBI::dbConnect(RSQLite::SQLite(), db)

  return(con)
}

disconnectDatabase <- function(con) {
  DBI::dbDisconnect(con)
}
