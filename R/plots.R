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
#' @return This function is called for the side effect of creating a plot. It
#'   invisibly returns the result from the custom plotting function specified by
#'   \code{plotID}. Previously it invisibly returned the study object. It's
#'   unlikely you relied on this behavior. For a ggplot2 plot, the return value
#'   will be the plotting object with class \code{"ggplot"}.
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

  plots <- list()
  for (i in seq_along(modelID)) {
    tempPlots <- getPlots(study, modelID = modelID[i], libraries = libraries)
    plots <- c(plots, tempPlots)
  }
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

  # Throw error if there is mismatch between number of features/tests and plot type
  nFeatures <- length(featureID)
  nTests    <- length(testID)
  nModels   <- length(modelID)
  plotType  <- p[["plotType"]]

  if (isEmpty(plotType)) plotType <- "singleFeature"
  if (length(plotType) == 1) {
    if (plotType == "multiTest") {
      plotType <- c("singleFeature", "multiTest")
    } else if (plotType == "multiModel") {
      plotType <- c("singleFeature", "multiModel")
    }
  }

  nPlotType <- length(plotType)

  for (ind in 1:nPlotType) {
    if (plotType[ind] == "singleFeature") {
      if (nFeatures != 1) {
        stop(
          "Plot type \"singleFeature\" requires 1 featureID\n",
          sprintf("Received %d featureID(s)", nFeatures)
        )
      } else if (nTests > 1 && !any(which(plotType %in% c("multiTest", "multiModel")))) {
        stop(
          "Plot type \"singleFeature\" requires 1 testID or be associated with either multiTest or multiModel\n",
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
      } else if (nTests > 1 && !any(which(plotType %in% c("multiTest", "multiModel")))) {
        stop(
          "Plot type \"multiFeature\" requires 1 testID or be associated with either multiTest or multiModel\n",
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
    # multiModel is checked as a multiTest as it requires at least 2 testIDs, eg.:
    # (1) 1 testID per model and > 1 model
    # (2) > 1 testID and 1 model
    if (plotType[ind] == "multiModel") {
      if (nTests < 2) {
        stop(
          "Plot type \"multiModel\" requires at least 2 testIDs\n",
          sprintf("Received %d testID(s)", nTests)
        )
      }
      if ((!is.null(names(testID)) & any(is.na(names(testID)))) | is.null(names(testID))) {
        stop(
          "Plot type \"multiModel\" requires a vector for testID named after related modelID"
        )
      }
      if (nModels > 1) {
        mapping <- getMapping(study = study, libraries = libraries)
        if (is.list(mapping) & length(mapping) == 0) {
          stop(
            "Plot type \"multiModel\" requires mapping object if > 1 modelID is used\n",
            sprintf("Received %d modelIDs", nModels)
          )
        }
      }
    }
  }

  if (length(modelID) > 1) {
    model_features <- mapping[[modelID[1]]][!is.na(mapping[[modelID[1]]])]
    if (!all(featureID %in% model_features)) {
      stop(
        "features list contains at least one feature not present in the corresponding model from mapping object\n",
        sprintf("ModelID : %s", modelID[1])
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

  # This is required so that the plot is immediately displayed. The final value
  # is returned invisibly to avoid overwhelming the R console with the data some
  # plotting functions return, but this prevents the ggplot object from
  # displaying (it's the same reason you have to print() ggplot plots inside a
  # for loop).
  if (inherits(returned, "ggplot")) print(returned)

  return(invisible(returned))
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
  # Detach packages in reverse order to avoid dependency conflicts
  pkgNamespaces <- pkgNamespaces[rank(match(pkgNamespaces, searchPath))]
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

  if (length(modelID) > 1) {
    mapping <- getMapping(study, libraries = libraries)
    listMaxLength <- max(sapply(mapping, length))
    mapping <- lapply(lapply(mapping, unlist), "length<-", listMaxLength)

    mappingdf <- as.data.frame(mapping, stringsAsFactors = FALSE)
    mapping_features_all <- stats::na.omit(mappingdf)
    column_order <- unique(c(modelID[1], colnames(mapping_features_all)))
    mapping_features_all <- mapping_features_all[, column_order]

    mapping_features <- mapping_features_all[which(mapping_features_all[,1] %in% featureID),]

    testID_all <- testID
  }

  for (model_i in modelID) {

    if (length(modelID) > 1) {
      featureID <- unique(mapping_features[,which(colnames(mapping_features) %in% model_i)])
      testID <- testID_all[which(names(testID_all) == model_i)]
    }

    assays <- getAssays(study, modelID = model_i, quiet = TRUE,
                        libraries = libraries)

    # Assays data is only required if no testID is defined. Users may want to only
    # plot data from the results table
    if (isEmpty(assays) && !is.null(testID)) {
      message(sprintf("No assays available for modelID \"%s\"\n", model_i))
      assaysPlotting <- assays
    } else if (isEmpty(assays)) {
      stop(sprintf("No assays available for modelID \"%s\"\n", model_i),
           "Add assays data with addAssays()")
    } else {
      featureIDAvailable <- featureID %in% rownames(assays)
      if (any(!featureIDAvailable)) {
        stop(sprintf("The feature \"%s\" is not available for modelID \"%s\"",
                     featureID[!featureIDAvailable][1], model_i))
      }
      assaysPlotting <- assays[featureID, , drop = FALSE]
    }

    samples <- getSamples(study, modelID = model_i, quiet = TRUE,
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

    features <- getFeatures(study, modelID = model_i, quiet = TRUE,
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
        results <- getResults(study, modelID = model_i, testID = testID[i], quiet = TRUE,
                              libraries = libraries)
        if (isEmpty(results)) {
          stop(sprintf("The test result (testID) \"%s\" is not available for modelID \"%s\" ", testID[i], model_i))
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

    if (length(modelID) > 1) {
      if (!exists("plottingData")) plottingData <- list()
      temp_model <- list(
        assays = assaysPlotting,
        samples = samplesPlotting,
        features = featuresPlotting,
        results = resultsPlotting
      )
      plottingData <- c(plottingData, stats::setNames(list(temp_model), model_i))
    } else {
    plottingData <- list(
      assays = assaysPlotting,
      samples = samplesPlotting,
      features = featuresPlotting
    )
    if (!isEmpty(testID)) plottingData <- c(plottingData, list(results = resultsPlotting))
    }
  }

  # for multiModel, reorder plottingData to have the same order as study$model
  if (length(modelID) > 1) {
    model_seq <- names(getModels(study, libraries = libraries))
    plottingData <- plottingData[order(match(names(plottingData), model_seq))]
  }

  return(plottingData)
}
