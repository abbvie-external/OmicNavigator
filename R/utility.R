
## I/O -------------------------------------------------------------------------

readTable <- function(x, hasRowNames = FALSE, sep = "\t", header = TRUE, data.table = FALSE, ...) {
  d <- data.table::fread(
    file = x,
    sep = sep,
    header = header,
    data.table = data.table,
    ...
  )
  if (hasRowNames) {
    row.names(d) <- d[[1]]
    d[[1]] <- NULL
  }
  d
}

writeTable <- function(x, file, sep = "\t", quote = TRUE, ...) {
  data.table::fwrite(
    x,
    file = file,
    sep = sep,
    quote = quote,
    ...
  )
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

coerceColsToCharacter <- function(x) {
  numberOfCols <- ncol(x)
  if (is.null(numberOfCols) || numberOfCols == 0) {
    stop("Invalid input. No columns to coerce.")
  }

  result <- lapply(x, as.character)
  result <- as.data.frame(result, stringsAsFactors = FALSE)
  return(result)
}

# Miscellaneous ----------------------------------------------------------------

# Returns TRUE if matches URL pattern, else FALSE
#
# > isUrl(c("http://somewhere.net", "https://secure.com/", "C:/path/to/file"))
# [1] FALSE  TRUE FALSE
#
isUrl <- function(x) {
  regex <- "^https?://.+"
  grepl(regex, x)
}
