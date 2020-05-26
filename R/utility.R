
## Databases -------------------------------------------------------------------

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

#' @importFrom dplyr "%>%"
#' @importFrom rlang "!!"
#' @importFrom rlang ".data"
assaysToLong <- function(x, modelID) {
  x %>%
    dplyr::mutate(featureID = rownames(.)) %>%
    tidyr::pivot_longer(cols = -.data$featureID,
                        names_to = "sampleID",
                        values_to = "quantification") %>%
    dplyr::mutate(modelID = !! modelID) %>%
    dplyr::select(.data$featureID, .data$sampleID, .data$modelID, .data$quantification)
}

assaysToWide <- function(x) {
  wide <- tidyr::pivot_wider(x,
                             names_from = "sampleID",
                             values_from = "quantification")
  wide <- as.data.frame(wide)
  rownames(wide) <- wide[, 1]
  wide <- wide[, -1]
  return(wide)
}
