
#' Validate a study
#'
#' @param study An OmicNavigator study object
#'
#' @export
validateStudy <- function(study) {
  checkStudy(study)

  emptyElements <- vapply(study, isEmpty, logical(1))
  elements <- names(study)[!emptyElements]
  for (e in elements) {
    checkFunctionName <- paste0("check", capitalize(e))
    checkFunction <- utils::getFromNamespace(checkFunctionName, ns = "OmicNavigator")
    checkFunction(study[[e]])
  }

  validateResults(study)
  validateAssays(study)

  return(invisible(TRUE))
}

validateResults <- function(study) {
  results <- study[["results"]]

  if (isEmpty(results)) stop("No results")

  for (i in seq_along(results)) {
    modelID <- names(results)[i]
    features <- try(getFeatures(study, modelID = modelID), silent = TRUE)
    for (j in seq_along(results[[i]])) {
      testID <- names(results[[i]])[j]
      dataFrame <- results[[i]][[j]]
      if (!inherits(features, "try-error")) {
        if (colnames(dataFrame)[1] != colnames(features)[1]) {
          stop("Name of features column doesn't match between results and features tables")
        }
        if (!all(dataFrame[, 1] %in% features[, 1])) {
          stop("Features in results table do not match features table")
        }
      }
    }
  }

  return(invisible(TRUE))
}

validateAssays <- function(study) {
  assays <- study[["assays"]]

  if (isEmpty(assays)) return(invisible(NA))

  for (i in seq_along(assays)) {
    modelID <- names(assays)[i]

    # Confirm that row names are the featureID
    rows <- row.names(assays[[i]])
    features <- try(getFeatures(study, modelID = modelID), silent = TRUE)
    if (!inherits(features, "try-error")) {
      if (!all(rows %in% features[, 1])) {
        stop("Row names of assays do not match featureID in features table\n",
             sprintf("modelID: %s", modelID))
      }
    }

    # Confirm that column names are the sampleID
    cols <- colnames(assays[[i]])
    samples <- try(getSamples(study, modelID = modelID), silent = TRUE)
    if (!inherits(samples, "try-error")) {
      if (!all(cols %in% samples[, 1])) {
        stop("Column names of assays do not match sampleID in samples table\n",
             sprintf("modelID: %s", modelID))
      }
    }
  }

  return(invisible(TRUE))
}
