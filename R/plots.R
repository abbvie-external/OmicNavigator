#' Plot a feature using a custom plotting function
#'
#' @export
plotStudy <- function(study, modelID, featureID, plotID, ...) {
  stopifnot(
    is.character(modelID),
    is.character(featureID),
    is.character(plotID)
  )

  plots <- getPlots(study, modelID = modelID)
  plotsAvailable <- names(plots)
  if(!plotID %in% plotsAvailable) {
    stop(sprintf("The plot \"%s\" is not available.\n", plotID),
         "Plots available:\n",
         sprintf("* \"%s\"\n", plotsAvailable))
  }
  p <- plots[[plotID]]
  if (inherits(study, "oaStudy")) {
    f <- getPlotFunction(plotID)
  } else {
    f <- getPlotFunction(plotID, study = study)
  }

  plottingData <- getPlottingData(study, modelID, featureID)

  # Setup for the plot and ensure everything is properly reset after the
  # function returns.
  originalParSettings <- graphics::par(no.readonly = TRUE)
  on.exit(resetPar(originalParSettings), add = TRUE)
  pkgNamespacesToDetach <- character()
  on.exit(resetSearch(pkgNamespacesToDetach), add = TRUE)
  for (pkg in p[["packages"]]) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      stop(sprintf("Package \"%s\" is not installed", pkg))
    }
    pkgNamespace <- sprintf("package:%s", pkg)
    if (!pkgNamespace %in% search()) {
      message(sprintf("Temporarily attaching namespace \"%s\" to the search path", pkgNamespace))
      suppressPackageStartupMessages(library(pkg, character.only = TRUE))
      pkgNamespacesToDetach <- c(pkgNamespacesToDetach, pkgNamespace)
    }
  }

  returned <- f(x = plottingData, featureID = featureID)
  if (inherits(returned, "ggplot")) print(returned)

  return(invisible(study))
}

getPlotFunction <- function(plotID, study = NULL) {
  if (is.null(study)) {
    f <- dynGet(plotID, ifnotfound = list(NA), inherits = TRUE)
  } else {
    pkg <- sprintf("OAstudy%s", study)
    f <- utils::getFromNamespace(plotID, ns = pkg)
  }

  stopifnot(length(f) == 1)
  return(f)
}

# Only reset par() if the settings have changed
resetPar <- function(originalParSettings) {
  currentParSettings <- graphics::par(no.readonly = TRUE)
  if (!identical(currentParSettings, originalParSettings)) {
    graphics::par(originalParSettings)
  }
  return(NULL)
}

# Detach packages from search path
resetSearch <- function(pkgNamespaces) {
  searchPath <- search()
  pkgNamespaces <- unique(pkgNamespaces)
  for (namespace in pkgNamespaces) {
    if (namespace %in% searchPath) {
      detach(namespace, character.only = TRUE)
    }
  }
}

#' Get plotting data
#'
#' @export
getPlottingData <- function(study, modelID, featureID) {
  assays <- getAssays(study, modelID = modelID)
  if (!featureID %in% rownames(assays)) {
    stop(sprintf("The feature \"%s\" is not available for model \"%s\"",
                 featureID, modelID))
  }

  assaysFeature <- t(assays[featureID, , drop = FALSE])
  colnames(assaysFeature) <- "feature"

  samples <- getSamples(study, modelID = modelID)

  plottingData <- merge(samples, assaysFeature,
                        by.x = 1, by.y = "row.names")
  return(plottingData)
}
