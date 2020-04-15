#' Plot a feature using a custom plotting function
#'
#'
#' @export
plotStudy <- function(study, model, featureName, plotName) {
  stopifnot(inherits(study, "oaStudy"), is.character(model),
            is.character(featureName))

  if (is.null(study$models)) {
    stop("No models are avaliable. Use addModels() to register them.")
  }

  if (!model %in% names(study$models)) {
    stop(sprintf("The model \"%s\" is not available. Use addModels() to register it.",
                 model))
  }

  if (is.null(study$features)) {
    stop("No features are avaliable. Use addFeatures() to register them.")
  }

  if (!featureName %in% study$features[[study$featureID]]) {
    stop(sprintf("The feature \"%s\" is not present in the features table",
                 featureName))
  }

  if (is.null(study$plots)) {
    stop("No custom plots are avaliable. Use addPlots() to register them.")
  }

  plots_available <- names(study$plots)
  if(!plotName %in% plots_available) {
    stop(sprintf("The plot \"%s\" is not available.\n", plotName),
         "Plots available:\n",
         sprintf("* \"%s\"\n", plots_available))
  }

  if (is.null(study$samples)) {
    stop("No samples are available. Use addAssays() to register them.")
  }

  if (is.null(study$assays)) {
    stop("No assays are available. Use addAssays() to register them.")
  }

  p <- study$plots[[plotName]]
  f <- p[["definition"]]
  assay_feature <- t(study[["assays"]][[model]][featureName, , drop = FALSE])
  colnames(assay_feature) <- "feature"
  x <- merge(study$samples, assay_feature,
             by.x = study$sampleID, by.y = "row.names")

  # Setup for the plot and ensure everything is properly reset after the
  # function returns.
  original_par_settings <- graphics::par(no.readonly = TRUE)
  on.exit(par(original_par_settings), add = TRUE)
  for (pkg in p[["packages"]]) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      stop(sprintf("Package \"%s\" is not installed", pkg))
    }
    pkg_namespace <- sprintf("package:%s", pkg)
    if (!pkg_namespace %in% search()) {
      suppressPackageStartupMessages(library(pkg, character.only = TRUE))
      on.exit(detach(pkg_namespace, character.only = TRUE), add = TRUE)
    }
  }

  returned <- f(x = x, featureName = featureName)
  if (inherits(returned, "ggplot")) print(returned)

  return(invisible(study))
}
