#'Invoke a custom plotting function
#'
#'`plotStudy()` invokes a custom plotting function saved within an OmicNavigator
#'study. This function is called by the app using the study-model-test
#'selection, feature selections, and plotting function metadata (see
#'[addPlots()]) to define arguments.
#'
#'@inheritParams shared-get
#'@inheritParams listStudies
#'
#'@details The arguments \code{study}, \code{modelID}, \code{featureID}, and
#'  `testID` are passed to the function [getPlottingData()]. The list returned
#'  by `getPlottingData()` is passed as the first argument to a custom plotting
#'  function. Some custom `plotTypes` (see [addPlots()]) require care when being
#'  invoked and attention should be paid to how a custom plot will be rendered
#'  by the app. Custom plots with `plotType = c(‘multiModel’, ‘singleTest’)`
#'  accept a `modelID` vector of length n and a vector of `testID`s length n,
#'  where n is the number of models. Custom plots with `plotType =
#'  c(‘multiModel’, ‘multiTest’)` accept `modelID` and `testID` vectors of
#'  length m, where m is the total number of tests considered across all models
#'  (note `testID`s are often repeated across models). Note that the index
#'  positions of these two vectors should correspond. That is, `testID` position
#'  1 should be found in the model specified by `modelID` position 1, etc.
#'
#'  The app will invoke custom plotting functions via `plotStudy()` using the
#'  current menu selections and plot metadata (see [addPlots()]). Plots with
#'  `plotType = ‘multiTest’` will be invoked with all `testID`s found within the
#'  currently selected model. Plots with `plotType =
#'  c(‘multiModel’,‘singleTest’)` will be invoked with all `modelID`s within the
#'  study (unless the plot has specified a list of models via `models`) and the
#'  currently selected `testID` (an error will result if the currently selected
#'  `testID` is not present in all relevant models for the plot). Plots with
#'  `plotType = c(‘multiModel’, ‘multiTest’)` will be invoked with all
#'  `modelID`s within the study (unless the plot has specified a list of models
#'  via `models`) and all identical `testID`s across models (if there are no
#'  matching testIDs across models an error will result).
#'
#'@return
#'
#'  This function is called for the side effect of creating a plot. It invisibly
#'  returns the result from the custom plotting function specified by `plotID`.
#'  Previously it invisibly returned the study object. It's unlikely you relied
#'  on this behavior. For a ggplot2 plot, the return value will be the plotting
#'  object with class `"ggplot"`. For a plotly plot, the return value will be
#'  the json schema used for plotting with class `“json”`.
#'
#'@seealso \code{\link{addPlots}}, \code{\link{getPlottingData}}
#'
#'@export
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
  if (!plotID %in% plotsAvailable) {
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
  dynamic <- FALSE

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
    if (plotType[ind] == "plotly") {
      dynamic <- TRUE
    }
    # multiModel is checked as a multiTest as it requires at least 2 testIDs.
    # E.g.: 1 testID per model and > 1 model
    if (plotType[ind] == "multiModel") {
      if (!is.null(testID) & nTests == 1) {
        stop(
          "Plot type \"multiModel\" requires testID to be either NULL (default) or a vector containing at least 2 testIDs\n",
          sprintf("Received %d testID(s)", nTests)
        )
      }
      if (nModels < 2) {
        stop(
          "Plot type \"multiModel\" requires at least 2 modelIDs\n",
          sprintf("Received %d modelID(s)", nModels)
        )
      }
      if (nModels > 1) {
        mapping <- getMapping(study = study, libraries = libraries)
        if (is.list(mapping) & length(mapping) == 0) {
          stop(
            "Plot type \"multiModel\" requires mapping object\n",
            sprintf("Received %d modelIDs", nModels)
          )
        }
      }
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

  if (dynamic == TRUE) {
    returned <- f(plottingData)
    if (!inherits(returned, "plotly")) {
      stop(sprintf("The plotID \"%s\" has plotType \"plotly\" but did not return an object with class \"plotly\"",
                   plotID))
    }
    returned <- plotly::plotly_json(returned, jsonedit = FALSE)
  } else {
    returned <- f(plottingData)
  }

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

# check mapping data requirements and extract relevant features per featureID
getMappingPlottingData <- function(study = study, modelID = modelID, featureID = featureID, testID = testID, libraries = NULL) {
  mapping <- getMapping(study, modelID = modelID[1], quiet = TRUE, libraries = libraries)
  model_features <- mapping[modelID[1]][!is.na(mapping[modelID[1]])]

  if (!any(featureID %in% model_features)) {
    stop(
      sprintf("The provided features list does not contain any feature present in model '%s' from mapping object.",
              modelID[1]
      )
    )
  }
  if (!all(featureID %in% model_features)) {
    stop(
      sprintf(
        "At least one feature is not present in the first model passed, '%s'.",
        modelID[1]
      )
    )
  }

  secondary_mapping  <- mapping[which(mapping[, modelID[1]] %in% featureID), ]
  secondary_mapping[, modelID[1]]  <- NULL
  secondary_features <- data.frame(secondary_mapping[rowSums(is.na(secondary_mapping)) != ncol(secondary_mapping),])
  if (nrow(secondary_features) == 0) {
    stop(
      sprintf(
        "The features are only present in the first model passed, '%s'.",
        modelID[1]
      )
    )
  }
  if (!is.null(testID) & length(modelID) != length(testID)) {
    stop(
      "For multimodel plots modelID and testID are required to be vectors of the same length or testID to be set to NULL.",
      "\n",
      sprintf("modelID: %s", paste(modelID, collapse = ", ")),
      "\n",
      sprintf("testID: %s", paste(testID, collapse = ", "))
    )
  }
  # Inform user if at least one feature from modelID is not present in secondary models
  incomplete_matches_sec_models = NULL
  for (ii in colnames(mapping)) {
    if (ii == modelID[1]) next
    if (any(is.na(mapping[ii]))) {
      incomplete_matches_sec_models <- trimws(paste(incomplete_matches_sec_models, ii, collapse=' '))
    }
  }
  if (!is.null(incomplete_matches_sec_models)) {
    warning(
      sprintf("At least one feature from model '%s' is not mapped in other model(s). Model(s) impacted: %s",
              modelID[1],
              incomplete_matches_sec_models
              )
    )
  }

  # Structuring data for mapping
  mappingdf <- as.data.frame(mapping, stringsAsFactors = FALSE)
  column_order <- unique(c(modelID[1], colnames(mappingdf)))
  mappingdf <- mappingdf[, column_order]

  mapping_features <- mappingdf[which(mappingdf[,1] %in% featureID),]

  if (!is.null(testID)) testID_all <- testID[order(modelID)] else testID_all <- NULL
  modelID    <- modelID[order(modelID)]

  mappingPlottingData <- list(
    mapping_features = mapping_features,
    testID_all = testID_all,
    modelID = modelID
  )
}

#'Get plotting data from an OmicNavigator study
#'
#'Returns `assay`, `sample`, `feature`, and `result` data that may be used for
#'plotting. This function is called by `plotStudy()` and the output is passed to
#'custom plotting functions. It should be used directly when interactively
#'creating custom plotting functions.
#'
#'The end-user should call this function and populate the first argument of
#'their custom plotting function with the output. When building functions, the
#'end-user should understand the category of plotting function they are creating
#'(e.g. `singleFeature` or `multiFeature`, see [addPlots()]) and call
#'`getPlottingData()` accordingly.
#'
#'Custom plots that accept data from multiple models and a single test
#'(`plotType = c(‘multiModel’, ‘singleTest’)`; see [addPlots()]) should be built
#'to accept output from `getPlottingData()` where `modelID` is vector of length
#'n and `testID` is a vector of length n, where n is the number of models.
#'Custom plots that accept data from multiple models and multiple tests
#'(`plotType = c(‘multiModel’, ‘multiTest’)`) should be built to accept output
#'from `getPlottingData()` where `modelID` and `testID` vectors are length m,
#'where m is the total number of tests considered across all models (note that
#'`testID`s must be repeated across models for the plotting function to work in
#'the app). The index positions of these two vectors should correspond. That is,
#'`testID` position 1 should be found in the model specified by `modelID`
#'position 1, etc. See [addPlots()] for information about the assignment of
#'`plotTypes` for your custom plots.
#'
#'
#'@inheritParams shared-get
#'@inheritParams listStudies
#'
#'@return Returns a list of at least 4 objects:
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
#' \item{\code{mapping}}{A data frame that contains the featureID(s) from each
#' model. This is the filtered mapping object. This data frame is returned when multiple models are passed as arguments}
#'
#'  The data frame \code{results} is only returned if you pass a testID. By
#'  default the app will always pass the currently selected testID.
#'
#'@seealso \code{\link{addPlots}}, \code{\link{plotStudy}}
#'
#'@export
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
    mappingPlottingData <- getMappingPlottingData(study, modelID, featureID, testID, libraries)
    mapping_features <- mappingPlottingData$mapping_features
    testID_all <- mappingPlottingData$testID_all
    modelID <- mappingPlottingData$modelID

    # Error if test passed to testID associated with modelID[1] is not available in study object
    incomplete_test_matches_sec_models = NULL
    incomplete_test_matches = list()
    current_model_tests <- testID[modelID %in% modelID[1]]
    for (i_current_model_tests in current_model_tests) {
      tmp <- getResults(study, modelID = modelID[1], testID = i_current_model_tests)
      if (isEmpty(tmp)) {
        stop(
          sprintf("Test '%s' from testID is not available in selected model '%s'.",
                  i_current_model_tests,
                  modelID[1])
        )
      } else { rm(tmp) }
    }

    # Warning if at least one test from modelID[1] is not available in secondary models
    for (ii in unique(modelID)) {
      if (ii == modelID[1]) next
      secondary_model_tests = testID[modelID %in% ii]
      for (i_secondary_model_tests in secondary_model_tests) {
        tmp <- suppressMessages(getResults(study, modelID = ii, testID = i_secondary_model_tests))
        if (isEmpty(tmp)) {
          secondary_model_tests <- secondary_model_tests[!secondary_model_tests %in% i_secondary_model_tests]
        } else { rm(tmp) }
      }
      if (isEmpty(secondary_model_tests)) secondary_model_tests <- NULL
      incomplete_test_matches_sec_models = !current_model_tests %in% secondary_model_tests

      if (any(incomplete_test_matches_sec_models)) {
        incomplete_test_matches[[ii]] = current_model_tests[incomplete_test_matches_sec_models]
      }
    }


    if (!isEmpty(incomplete_test_matches)) {
      unmatched_tests <- NULL
      for (inames in names(incomplete_test_matches)) {
        unmatched_tests <- c(unmatched_tests, incomplete_test_matches[[inames]])
      }
      unmatched_tests <- paste0(unique(unmatched_tests), collapse=", ")
      warning(
        sprintf("At least one test from model '%s' is not available in other model(s). Test(s) impacted: %s",
                modelID[1],
                unmatched_tests)
      )
    }
  }

  for (ii in 1:length(modelID)) {
    model_i <- modelID[ii]

    if (length(modelID) > 1) {
      featureID <- unique(mapping_features[,which(colnames(mapping_features) %in% model_i)])
      featureID <- featureID[!is.na(featureID)]
      testID <- testID_all[ii]
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
      # only necessary for singleModel plots - for multiModel the check is done at getMappingPlottingData
      if (length(modelID) == 1) {
        if (!is.null(testID)) {
          featID_tmp <- NULL
          for (i_testID in testID) {
            tmp <- getResults(study, modelID = model_i, testID = i_testID, quiet = TRUE,
                              libraries = libraries)
            if (is.data.frame(tmp)) {
              featID_tmp <- unique(c(featID_tmp, tmp[[1]]))
            }
          }
          featureIDAvailable <- featureID %in% featID_tmp
          if (any(!featureIDAvailable)) {
            stop(sprintf("At least one feature is not available in the results object for modelID \"%s\": \"%s\"",
                         model_i, featureID[!featureIDAvailable][1]))
          }
        }
        if (is.null(testID) || (!is.null(testID) && !is.data.frame(tmp))) {
          featureIDAvailable <- featureID %in% rownames(assays)
          if (any(!featureIDAvailable)) {
            stop(sprintf("At least one feature is not available in the assay object for modelID \"%s\": \"%s\"",
                         model_i, featureID[!featureIDAvailable][1]))
          }
        }
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

        if (length(modelID) > 1) {
          if (model_i %in% names(incomplete_test_matches)) {
            if (testID %in% incomplete_test_matches[[model_i]]) {
              resultsPlotting[[i]] <- NULL
              next
            }
          }
        }

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

      if (!model_i %in% names(plottingData)) {
        temp_model <- list(
          assays = assaysPlotting,
          samples = samplesPlotting,
          features = featuresPlotting
        )
        if (!isEmpty(testID) && (!isEmpty(resultsPlotting))) temp_model <- c(temp_model, list(results = stats::setNames(list(resultsPlotting), testID)))
        plottingData <- c(plottingData, stats::setNames(list(temp_model), model_i))

      } else if (sum(modelID %in% model_i) > 1 & exists("resultsPlotting")) {
        if (!isEmpty(resultsPlotting)) {
          resultsPlotting <- list(resultsPlotting)
          names(resultsPlotting) <- testID
          plottingData[[model_i]]$results <- c(plottingData[[model_i]]$results, resultsPlotting)
        }
      }

      if (ii == length(modelID)) {
        plottingData <- c(plottingData, stats::setNames(list(mapping_features), "mapping"))
      }
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
