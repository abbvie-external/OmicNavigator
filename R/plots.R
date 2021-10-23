#' Plot a feature using a custom plotting function
#'
#' @inheritParams shared-get
#' @inheritParams listStudies
#'
#' @details The arguments \code{study}, \code{modelID}, \code{featureID}, and
#' \code{testID} are passed to the function \code{\link{getPlottingData}}, and
#' the nested list returned by this function is passed as the first argument to
#' your custom plotting function. By default, the app will pass a single
#' featureID unless the plotType is "multiFeature". Similarly, the app will pass
#' a single testID unless the plotType is "multiTest". You can specify the
#' plotType when you add a plot with \code{\link{addPlots}}.
#'
#' @return This function is called for the side effect of creating a plot.
#'   However, it also invisible returns the original \code{onStudy} object
#'   passed to \code{study}.
#'
#' @seealso \code{\link{addPlots}}, \code{\link{getPlottingData}}
#'
#' @export
plotStudy <- function(study, modelID, featureID, plotID, testID = NULL, libraries = NULL) {
  stopifnot(
    is.character(modelID),
    is.character(featureID),
    is.character(plotID),
    is.null(testID) || is.character(testID),
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

  # Throw error is mismatch between number of features / tests and plot type
  nFeatures <- length(featureID)
  nTests    <- length(testID)
  plotType  <- p[["plotType"]]

  if (isEmpty(plotType)) plotType <- "singleFeature"
  if (length(plotType) == 1 && plotType == "multiTest") plotType <- c("singleFeature", "multiTest")
  nPlotType <- length(plotType)

  for (ind in 1:nPlotType) {
    if (plotType[ind] == "singleFeature") {
      if (nFeatures != 1) {
        stop(
          "Plot type \"singleFeature\" requires 1 featureID\n",
          sprintf("Received %d featureID(s)", nFeatures)
        )
      } else if (nTests > 1 && !any(which(plotType == "multiTest"))) {
        stop(
          "Plot type \"singleFeature\" requires 1 testID or be associated with multiTest, e.g. plot type = c(\"singleFeature\", \"multiTest\")\n",
          sprintf("Received %d testID(s)", nTests)
        )
      }
    }
    if (plotType[ind] == "multiFeature") {
      if (nFeatures < 2) {
        stop(
          "Plot type \"multiFeature\" requires at least 2 featureIDs\n",
          sprintf("Received %d featureID(s)", nFeatures)
        )
      } else if (nTests > 1 && !any(which(plotType == "multiTest"))) {
        stop(
          "Plot type \"multiFeature\" requires 1 testID or be associated with multiTest, e.g. plot type = c(\"multiFeature\", \"multiTest\")\n",
          sprintf("Received %d testID(s)", nTests)
        )
      }
    }
    if (plotType[ind] == "multiTest" && nTests < 2) {
      stop(
        "Plot type \"multiTest\" requires at least 2 testIDs\n",
        sprintf("Received %d testID(s)", nTests)
      )
    }
  }

  plottingData <- getPlottingData(study, modelID, featureID, testID = testID,
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
#' @return Returns a list of 4 data frames:
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
#' of the assays data frame).}
#'
#' \item{\code{results}}{A data frame that contains the test results,
#' filtered to only include the row(s) corresponding to the input featureID(s).
#' If multiple featureIDs are requested, the rows are reordered to match the
#' order of this input. The column order is unchanged. If multiple testIDs are
#' provided, they are stored in a list object.}
#'
#' The data frame \code{results} is only returned if you pass a testID. By
#' default the app will always pass the currently selected testID. To make
#' \code{results} a list of data frames (one for each testID for the currently
#' selected modelID), set the plotType to be "multiTest" when adding the plot
#' with \code{\link{addPlots}}.
#'
#' @seealso \code{\link{addPlots}}, \code{\link{plotStudy}}
#'
#' @export
getPlottingData <- function(study, modelID, featureID, testID = NULL, libraries = NULL) {
  stopifnot(
    is.character(modelID),
    is.character(featureID),
    is.null(testID) || is.character(testID),
    is.null(libraries) || is.character(libraries)
  )
  # Deduplicate the featureIDs
  featureID <- unique(featureID)

  assays <- getAssays(study, modelID = modelID, quiet = TRUE,
                      libraries = libraries)
  # Assays data is only required if no testID is defined. Users may want to only
  # plot data from the results table
  if (isEmpty(assays) && !is.null(testID)) {
    message(sprintf("No assays available for modelID \"%s\"\n", modelID))
    assaysPlotting <- assays
  } else if (isEmpty(assays)) {
    stop(sprintf("No assays available for modelID \"%s\"\n", modelID),
         "Add assays data with addAssays()")
  } else {
    featureIDAvailable <- featureID %in% rownames(assays)
    if (any(!featureIDAvailable)) {
      stop(sprintf("The feature \"%s\" is not available for modelID \"%s\"",
                   featureID[!featureIDAvailable][1], modelID))
    }
    assaysPlotting <- assays[featureID, , drop = FALSE]
  }

  samples <- getSamples(study, modelID = modelID, quiet = TRUE,
                        libraries = libraries)
  if (isEmpty(samples) || isEmpty(assays)) {
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

  if (!isEmpty(testID)) {
    resultsPlotting <- vector("list", length(testID))
    for (i in seq_along(testID)) {
      results <- getResults(study, modelID = modelID, testID = testID[i], quiet = TRUE,
                            libraries = libraries)
      if (isEmpty(results)) {
        stop(sprintf("The test result (testID) \"%s\" is not available for modelID \"%s\" ", testID[i], modelID))
      }
      featureIDAvailable_results <- featureID %in% results[,1]
      if (any(!featureIDAvailable_results)) {
        stop(sprintf("The feature \"%s\" is not available for testID \"%s\"",
                     featureID[!featureIDAvailable][1], testID[i]))
      }
      resultsPlotting[[i]] <- results[match(featureID, results[,1], nomatch = 0), , drop = FALSE]
      names(resultsPlotting)[[i]] <- testID[i]
    }
    if (length(resultsPlotting) == 1) resultsPlotting <- resultsPlotting[[1]]
  }

  plottingData <- list(
    assays = assaysPlotting,
    samples = samplesPlotting,
    features = featuresPlotting
  )
  if (!isEmpty(testID)) plottingData <- c(plottingData, list(results = resultsPlotting))

  return(plottingData)
}
