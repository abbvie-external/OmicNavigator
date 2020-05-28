#' @export
print.oaStudy <- function(x, ...) {

  cat("== OmicAnalyzer ==\n")
  cat(sprintf("* name: %s\n", x[["name"]]))
  cat(sprintf("* description: %s\n", x[["description"]]))
  modelsAll <- listAllModels(x)
  if (length(modelsAll) > 0) {
    cat(sprintf("* %d models: %s\n", length(modelsAll),
                paste(modelsAll, collapse = ", ")))
  } else {
    cat("* 0 models\n")
  }
  if (isEmpty(x[["annotations"]])) {
    cat("* 0 annotations\n")
  } else {
    cat(sprintf("* %d annotations: %s\n", length(x[["annotations"]]),
                paste(names(x[["annotations"]]), collapse = ", ")))
  }

  return(invisible(x))
}

listAllModels <- function(study) {
  elementsWithModels <- c("samples", "features", "assays", "models", "tests",
                          "results", "enrichments", "metaFeatures", "plots")
  modelsAll <- lapply(study[elementsWithModels], names)
  modelsAll <- unlist(modelsAll)
  modelsAll <- unique(modelsAll)
  modelsAll <- setdiff(modelsAll, "default")
  return(modelsAll)
}
