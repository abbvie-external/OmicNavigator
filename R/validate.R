
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

  validateResultsLinkouts(study)
  validateAssays(study)
  validateResults(study)

  return(invisible(TRUE))
}

# Validate that results table linkouts only include columns in corresponding
# features table.
validateResultsLinkouts <- function(study) {
  resultsLinkouts <- study[["resultsLinkouts"]]

  if (isEmpty(resultsLinkouts)) return(invisible(NA))

  for (i in seq_along(resultsLinkouts)) {
    modelID <- names(resultsLinkouts)[i]
    features <- getFeatures(study, modelID = modelID, quiet = TRUE)
    for (j in seq_along(resultsLinkouts[[i]])) {
      featureColumnName <- names(resultsLinkouts[[i]])[j]
      if (!featureColumnName %in% colnames(features)) {
        stop(sprintf("Invalid results table linkout for modelID \"%s\"\n"),
             sprintf("\"%s\" is not the name of a column in the features table"))
      }
    }
  }

  return(invisible(TRUE))
}

validateResults <- function(study) {
  results <- study[["results"]]

  if (isEmpty(results)) stop("No results")

  for (i in seq_along(results)) {
    modelID <- names(results)[i]
    features <- getFeatures(study, modelID = modelID, quiet = TRUE)
    assays <- getAssays(study, modelID = modelID, quiet = TRUE)

    # Throw warning if no common columns across tests. This will disable UpSet
    # filtering in app.
    upsetCols <- getUpsetCols(study, modelID)
    if (isEmpty(upsetCols)) {
      warning(
        sprintf("The results tables for the tests of modelID \"%s\" do not have any columns in common. ", modelID),
        "You will not be able to perform set analysis in the app. ",
        "If it makes sense for your study, please consider using shared column names."
      )
    }
    for (j in seq_along(results[[i]])) {
      testID <- names(results[[i]])[j]
      dataFrame <- results[[i]][[j]]

      if (!isEmpty(features)) {
        if (colnames(dataFrame)[1] != colnames(features)[1]) {
          stop("Name of featureID column doesn't match between results and features tables")
        }
        resultsInFeatures <- dataFrame[, 1] %in% features[, 1]
        if (sum(resultsInFeatures) == 0) {
          stop("The features in the results table do not match the featureID column in the features table\n",
               sprintf("modelID: %s, testID: %s", modelID, testID))
        }
        if (!all(resultsInFeatures)) {
          stop("Some of the features in the assays table are missing from the featureID column in the features table\n",
               sprintf("modelID: %s, testID: %s", modelID, testID))
        }
      }

      if (!isEmpty(assays)) {
        resultsInAssays <- dataFrame[, 1] %in% rownames(assays)
        if (sum(resultsInAssays) == 0) {
          stop("The features in the results table do not match the row names of the assays table\n",
               sprintf("modelID: %s, testID: %s", modelID, testID))
        }
        if (!all(resultsInAssays)) {
          stop("Some of the features in the results table are missing from the row names of the assays table\n",
               sprintf("modelID: %s, testID: %s", modelID, testID))
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
    features <- getFeatures(study, modelID = modelID, quiet = TRUE)
    if (!isEmpty(features)) {
      rowsInFeatures <- rows %in% features[, 1]
      if (sum(rowsInFeatures) == 0) {
        stop("The row names of the assays table do not match the featureID column in the features table\n",
             sprintf("modelID: %s", modelID))
      }
      if (!all(rowsInFeatures)) {
        stop("Some of the row names of the assays table are missing from the featureID column in the features table\n",
             sprintf("modelID: %s", modelID))
      }
    }

    # Confirm that column names are the sampleID
    cols <- colnames(assays[[i]])
    samples <- getSamples(study, modelID = modelID, quiet = TRUE)
    if (!isEmpty(samples)) {
      colsInSamples <- cols %in% samples[, 1]
      if (sum(colsInSamples) == 0) {
        stop("The column names of the assays table do not match the sampleID column in the samples table\n",
             sprintf("modelID: %s", modelID))
      }
      if (!all(colsInSamples)) {
        stop("Some of the column names of the assays table are missing from the sampleID column in the samples table\n",
             sprintf("modelID: %s", modelID))
      }
    }
  }

  return(invisible(TRUE))
}
