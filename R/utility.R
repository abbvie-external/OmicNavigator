## OmicAnalzyer ----------------------------------------------------------------

# Get the prefix used to label OmicNavigator study packages.
#
# regex - Prepend "^" to convert the prefix to a regular expression
#
# Global internal package variable `OmicNavigatorPrefix` is defined in zzz.R
getPrefix <- function(regex = FALSE) {
  prefix = getOption("OmicNavigator.prefix", default = OmicNavigatorPrefix)
  if (regex) prefix <- paste0("^", prefix)
  return(prefix)
}

studyToPkg <- function(study) {
  pkg <- paste0(getPrefix(), study)
  return(pkg)
}

pkgToStudy <- function(pkg) {
  regex <- getPrefix(regex = TRUE)
  study <- sub(regex, "", pkg)
  return(study)
}

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

combineListIntoTable <- function(listObj, newColumnName = "newColumnName") {
  stopifnot(
    is.list(listObj),
    length(listObj) > 0,
    is.character(newColumnName)
  )

  listNames <- names(listObj)
  newListObj <- listObj
  for (i in seq_along(listObj)) {
    newListObj[[i]][[newColumnName]] <- listNames[i]
  }

  names(newListObj) <- NULL # to avoid row names in output
  newTable <- do.call(rbind, newListObj)
  newColumnIndex <- ncol(newTable)
  newTable <- newTable[, c(newColumnIndex, seq_len(newColumnIndex - 1))]

  return(newTable)
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
  output <- data.table::dcast.data.table(
    data = data.table::as.data.table(x),
    formula = termID + description ~ testID,
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

warnIfNonCharacterCols <- function(x) {
  stopifnot(is.data.frame(x))

  characterColsFilter <- vapply(x, is.character, logical(1))
  nonCharacterCols <- colnames(x)[!characterColsFilter]
  if (!isEmpty(nonCharacterCols)) {
    warning(
      "Detected non-character columns. ",
      "The following columns were automatically coerced to character strings: ",
      paste(nonCharacterCols, collapse = ", "),
      call. = FALSE
    )
  }

  return(NULL)
}

# Strings ----------------------------------------------------------------------

# Capitalizes the first letter in each element of the character vector
#
# capitalize(c("metaFeatures", "results", "abc def"))
# ## [1] "MetaFeatures" "Results"      "Abc def"
#
capitalize <- function(x) {
  first <- substr(x, 1, 1)
  firstCapitalized <- toupper(first)
  final <- paste0(firstCapitalized, substring(x, 2))
  return(final)
}

# Miscellaneous ----------------------------------------------------------------

# Returns TRUE if matches URL pattern, else FALSE
#
# > isUrl(c("http://somewhere.net", "https://secure.com/", "C:/path/to/file"))
# [1] TRUE  TRUE FALSE
#
isUrl <- function(x) {
  if (is.null(x)) return(FALSE)
  regex <- "^https?://.+"
  grepl(regex, x)
}
