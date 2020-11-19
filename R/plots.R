#' Plot a feature using a custom plotting function
#'
#' @inheritParams shared-get
#'
#' @export
plotStudy <- function(study, modelID, testID, featureID, plotID, libraries = NULL) {
  stopifnot(
    is.character(modelID),
    is.character(testID),
    is.character(featureID),
    is.character(plotID),
    is.null(libraries) || is.character(libraries)
  )

  plots <- getPlots(study, modelID = modelID, libraries = libraries)
  plotsAvailable <- names(plots)
  if(!plotID %in% plotsAvailable) {
    stop(sprintf("The plot \"%s\" is not available.\n", plotID),
         "Plots available:\n",
         sprintf("* \"%s\"\n", plotsAvailable))
  }
  p <- plots[[plotID]]
  if (inherits(study, "onStudy")) {
    f <- getPlotFunction(plotID)
  } else {
    f <- getPlotFunction(plotID, study = study)
  }

  plottingData <- getPlottingData(study, modelID, testID, featureID,
                                  libraries = libraries)

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

  returned <- f(plottingData)
  if (inherits(returned, "ggplot")) print(returned)

  return(invisible(study))
}

getPlotFunction <- function(plotID, study = NULL) {
  if (is.null(study)) {
    f <- dynGet(plotID, ifnotfound = list(NA), inherits = TRUE)
  } else {
    pkg <- paste0(getPrefix(), study)
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
#' @inheritParams shared-get
#'
#' @export
getPlottingData <- function(study, modelID, testID, featureID, libraries = NULL) {
  assays <- getAssays(study, modelID = modelID, quiet = TRUE,
                      libraries = libraries)
  if (isEmpty(assays)) {
    stop(sprintf("No assays available for modelID \"%s\"\n", modelID),
         "Add assays data with addAssays()")
  }
  if (!featureID %in% rownames(assays)) {
    stop(sprintf("The feature \"%s\" is not available for modelID \"%s\"",
                 featureID, modelID))
  }
  assaysPlotting <- assays[featureID, , drop = FALSE]

  samples <- getSamples(study, modelID = modelID, quiet = TRUE,
                        libraries = libraries)
  if (isEmpty(samples)) {
    samplesPlotting <- samples
  } else {
    samplesPlotting <- samples[match(samples[[1]], colnames(assaysPlotting)), ]
  }

  features <- getFeatures(study, modelID = modelID, quiet = TRUE,
                          libraries = libraries)
  if (isEmpty(features)) {
    featuresPlotting <- features
  } else {
    featuresPlotting <- features[features[[1]] == featureID, , drop = FALSE]
  }

  results <- getResults(study, modelID = modelID, testID = testID, quiet = TRUE,
                        libraries = libraries)
  if (isEmpty(results)) {
    resultsPlotting <- results
  } else {
    resultsPlotting <- results[results[[1]] == featureID, , drop = FALSE]
  }

  plottingData <- list(
    assays = assaysPlotting,
    samples = samplesPlotting,
    features = featuresPlotting,
    results = resultsPlotting
  )
  return(plottingData)
}
