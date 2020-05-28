#' Plot a feature using a custom plotting function
#'
#'
#' @export
plotStudy <- function(study, modelID, feature, plotName) {
  stopifnot(inherits(study, "oaStudy"), is.character(modelID),
            is.character(feature), is.character(plotName))

  plots <- getPlots(study, modelID = modelID)
  assays <- getAssays(study, modelID = modelID)
  samples <- getSamples(study, modelID = modelID)
  features <- getFeatures(study, modelID = modelID)

  if (!feature %in% features[, 1]) {
    stop(sprintf("The feature \"%s\" is not present in the features table",
                 feature))
  }

  plotsAvailable <- names(plots)
  if(!plotName %in% plotsAvailable) {
    stop(sprintf("The plot \"%s\" is not available.\n", plotName),
         "Plots available:\n",
         sprintf("* \"%s\"\n", plotsAvailable))
  }

  p <- plots[[plotName]]
  f <- p[["definition"]]
  assayFeature <- t(assays[feature, , drop = FALSE])
  colnames(assayFeature) <- "feature"
  x <- merge(samples, assayFeature,
             by.x = 1, by.y = "row.names")

  # Setup for the plot and ensure everything is properly reset after the
  # function returns.
  original_par_settings <- graphics::par(no.readonly = TRUE)
  on.exit(graphics::par(original_par_settings), add = TRUE)
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

  returned <- f(x = x, feature = feature)
  if (inherits(returned, "ggplot")) print(returned)

  return(invisible(study))
}
