
## I/O -------------------------------------------------------------------------

readTable <- function(x, hasRowNames = FALSE, ...) {
  d <- data.table::fread(file = x, data.table = FALSE, ...)
  if (hasRowNames) {
    row.names(d) <- d[[1]]
    d[[1]] <- NULL
  }
  d
}

readJson <- function(x, ...) {
  jsonlite::read_json(x, simplifyVector = TRUE, ...)
}

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

## Lists -----------------------------------------------------------------------

isEmpty <- function(x) {length(x) == 0}

addToList <- function(listOne, listTwo, overwrite = FALSE) {
  listNew <- listOne

  for (i in seq_along(listTwo)) {
    elementName <- names(listTwo)[i]
    if (elementName %in% names(listOne) && !overwrite) {
      stop(sprintf("Data for \"%s\" already exists.\n", elementName),
           "Set the argument overwrite to TRUE to replace it.",
      call. = FALSE)
    }

    listNew[[elementName]] <- listTwo[[elementName]]
  }

  return(listNew)
}

# Recursively apply function to all data frames in a nested list
dfrapply <- function(object, f, ...) {
  if (inherits(object, "data.frame")) {
    return(f(object, ...))
  }
  if (inherits(object, "list")) {
    return(lapply(object, function(x) dfrapply(x, f, ...)))
  }
  stop("List element must be either a data frame or another list")
}

## Data frames -----------------------------------------------------------------

hasUniqueIdColumn <- function(x) {
  colValues <- x[[1]]
  colName <- colnames(x)[1]
  colIsUnique <- length(unique(colValues)) == length(colValues)
  if (!colIsUnique) {
    stop(sprintf("The first column, \"%s\", must contain unique values", colName),
         call. = FALSE)
  }
}

enrichmentsToWide <- function(x, type) {
  data.table::setDT(x)
  output <- data.table::dcast.data.table(
    x,
    termID + description ~ testID,
    value.var = type
  )
  data.table::setDF(output)
  return(output)
}
