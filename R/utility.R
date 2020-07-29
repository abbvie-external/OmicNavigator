
## I/O -------------------------------------------------------------------------

readTable <- function(x, hasRowNames = FALSE, data.table = FALSE, ...) {
  d <- data.table::fread(file = x, data.table = data.table, ...)
  if (hasRowNames) {
    row.names(d) <- d[[1]]
    d[[1]] <- NULL
  }
  d
}

writeTable <- function(x, file, sep = "\t", ...) {
  data.table::fwrite(x, file = file, sep = sep, ...)
}

readJson <- function(x, simplifyVector = TRUE, ...) {
  jsonlite::read_json(x, simplifyVector = simplifyVector, ...)
}

writeJson <- function(x, file, auto_unbox = TRUE, pretty = TRUE, ...) {
  jsonlite::write_json(x, path = file, auto_unbox = auto_unbox, pretty = pretty, ...)
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
