
#' Validate a study
#'
#' @param study An OmicAnalyzer study object
#'
#' @export
validateStudy <- function(study) {
  checkStudy(study)

  elementsFilter <- vapply(study, function(x) is.list(x) && !isEmpty(x), logical(1))
  elements <- names(study)[elementsFilter]
  for (e in elements) {
    checkFunctionName <- paste0("check", capitalize(e))
    checkFunction <- utils::getFromNamespace(checkFunctionName, ns = "OmicAnalyzer")
    checkFunction(study[[e]])
  }

  validateResults(study)

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
