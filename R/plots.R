#' Plot a feature using a custom plotting function
#'
#' @inheritParams shared-get
#' @inheritParams listStudies
#'
#' @seealso \code{\link{addPlots}}, \code{\link{getPlottingData}}
#'
#' @export
plotStudy <- function(study, modelID, featureID, plotID, libraries = NULL) {
  stopifnot(
    is.character(modelID),
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

  # Throw error is mismatch between number of features and plot type
  nFeatures <- length(featureID)
  plotType <- p[["plotType"]]
  if (isEmpty(plotType)) plotType <- "singleFeature"
  if (plotType == "singleFeature" && nFeatures != 1) {
    stop(
      "Plot type \"singleFeature\" requires 1 featureID\n",
      sprintf("Received %d featureID(s)", nFeatures)
    )
  }
  if (plotType == "multiFeature" && nFeatures < 2) {
    stop(
      "Plot type \"multiFeature\" requires at least 2 featureIDs\n",
      sprintf("Received %d featureID(s)", nFeatures)
    )
  }

  plottingData <- getPlottingData(study, modelID, featureID,
                                  libraries = libraries)

  # Setup for the plot and ensure everything is properly reset after the
  # function returns.
  originalParSettings <- graphics::par(no.readonly = TRUE)
  on.exit(resetPar(originalParSettings), add = TRUE)
  pkgNamespacesToDetach <- character()
  on.exit(resetSearch(pkgNamespacesToDetach), add = TRUE)
  originalTheme <- thematic::thematic_on(
    bg = "#ffffff", # white
    fg = "#2e2e2e", # black
    accent = "#ff4400", # orange
    font = thematic::font_spec(
      families = c("Lato", "Open Sans"),
      install = TRUE,
      update = TRUE,
      quiet = FALSE
    ),
    sequential = thematic::sequential_gradient(
      fg_weight = 0,
      bg_weight = 0.9,
      fg_low = FALSE
    ),
    qualitative = c(
      "#ff4400", # orange
      "#ff7e38", # yellow-orange
      "#2c3b78"  # blue
    )
  )
  on.exit(thematic::thematic_set_theme(originalTheme), add = TRUE)
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

  stopifnot(is.function(f))
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
#' This function creates the input data that \code{\link{plotStudy}} passes to
#' custom plotting functions added with \code{\link{addPlots}}. You can use it
#' directly when you are interactively creating your custom plotting functions.
#'
#' @inheritParams shared-get
#' @inheritParams listStudies
#'
#' @return Returns a list of 3 data frames:
#'
#' \item{\code{assays}}{A data frame that contains the assay measurements,
#' filtered to only include the row(s) corresponding to the input featureID(s)
#' (see \code{\link{getAssays}}). If multiple featureIDs are requested, the rows
#' are reordered to match the order of this input. The column order is
#' unchanged.}
#'
#' \item{\code{samples}}{A data frame that contains the sample metadata for the
#' given modelID (see \code{\link{getSamples}}). The rows are reordered to match
#' the columns of the assays data frame.}
#'
#' \item{\code{features}}{A data frame that contains the feature metadata,
#' filtered to only include the row(s) corresponding to the input featureID(s)
#' (see \code{\link{getFeatures}}). If multiple featureIDs are requested, the
#' rows are reordered to match the order of this input (and thus match the order
#' of the assays data frame.}
#'
#' @seealso \code{\link{addPlots}}, \code{\link{plotStudy}}
#'
#' @export
getPlottingData <- function(study, modelID, featureID, libraries = NULL) {
  stopifnot(
    is.character(modelID),
    is.character(featureID),
    is.null(libraries) || is.character(libraries)
  )
  # Deduplicate the featureIDs
  featureID <- unique(featureID)

  assays <- getAssays(study, modelID = modelID, quiet = TRUE,
                      libraries = libraries)
  if (isEmpty(assays)) {
    stop(sprintf("No assays available for modelID \"%s\"\n", modelID),
         "Add assays data with addAssays()")
  }
  featureIDAvailable <- featureID %in% rownames(assays)
  if (any(!featureIDAvailable)) {
    stop(sprintf("The feature \"%s\" is not available for modelID \"%s\"",
                 featureID[!featureIDAvailable][1], modelID))
  }
  assaysPlotting <- assays[featureID, , drop = FALSE]

  samples <- getSamples(study, modelID = modelID, quiet = TRUE,
                        libraries = libraries)
  if (isEmpty(samples)) {
    samplesPlotting <- samples
  } else {
    samplesPlotting <- samples[match(colnames(assaysPlotting), samples[[1]], nomatch = 0), ,
                               drop = FALSE]
    if (!identical(samplesPlotting[[1]], colnames(assaysPlotting))) {
      warning("Not all of the sampleIDs have metadata")
    }
    row.names(samplesPlotting) <- NULL # reset row numbers after filtering
  }

  features <- getFeatures(study, modelID = modelID, quiet = TRUE,
                          libraries = libraries)
  if (isEmpty(features)) {
    featuresPlotting <- features
  } else {
    featuresPlotting <- features[match(featureID, features[[1]], nomatch = 0), , drop = FALSE]
    if (!identical(featuresPlotting[[1]], featureID)) {
      warning("Not all of the featureIDs have metadata")
    }
    row.names(featuresPlotting) <- NULL # reset row numbers after filtering
  }

  plottingData <- list(
    assays = assaysPlotting,
    samples = samplesPlotting,
    features = featuresPlotting
  )
  return(plottingData)
}
