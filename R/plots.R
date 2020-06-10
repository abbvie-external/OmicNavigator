#' Plot a feature using a custom plotting function
#'
#' @export
plotStudy <- function(study, modelID, featureID, plotID, ...) {
  UseMethod("plotStudy")
}

#' @rdname plotStudy
#' @export
plotStudy.oaStudy <- function(study, modelID, featureID, plotID, ...) {
  stopifnot(is.character(modelID), is.character(featureID), is.character(plotID))

  plots <- getPlots(study, modelID = modelID)
  assays <- getAssays(study, modelID = modelID)
  samples <- getSamples(study, modelID = modelID)
  features <- getFeatures(study, modelID = modelID)

  if (!featureID %in% features[, 1]) {
    stop(sprintf("The featureID \"%s\" is not present in the features table",
                 featureID))
  }

  plotsAvailable <- names(plots)
  if(!plotID %in% plotsAvailable) {
    stop(sprintf("The plot \"%s\" is not available.\n", plotID),
         "Plots available:\n",
         sprintf("* \"%s\"\n", plotsAvailable))
  }

  p <- plots[[plotID]]
  f <- getPlotFunction(plotID)

  assayFeature <- t(assays[featureID, , drop = FALSE])
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

  returned <- f(x = x, featureID = featureID)
  if (inherits(returned, "ggplot")) print(returned)

  return(invisible(study))
}

#' @rdname plotStudy
#' @export
plotStudy.character <- function(study, modelID, featureID, plotID, ...) {
  stopifnot(is.character(modelID), is.character(featureID), is.character(plotID))

  plots <- getPlots(study, modelID = modelID)
  assays <- getAssays(study, modelID = modelID)
  samples <- getSamples(study, modelID = modelID)
  features <- getFeatures(study, modelID = modelID)

  if (!featureID %in% features[, 1]) {
    stop(sprintf("The featureID \"%s\" is not present in the features table",
                 featureID))
  }

  plotsAvailable <- names(plots)
  if(!plotID %in% plotsAvailable) {
    stop(sprintf("The plot \"%s\" is not available.\n", plotID),
         "Plots available:\n",
         sprintf("* \"%s\"\n", plotsAvailable))
  }

  p <- plots[[plotID]]
  f <- getPlotFunction(plotID, study = study)

  assayFeature <- t(assays[featureID, , drop = FALSE])
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

  returned <- f(x = x, featureID = featureID)
  if (inherits(returned, "ggplot")) print(returned)

  return(invisible(study))
}

#' @export
plotStudy.default <- function(study, modelID, featureID, plotID, ...) {
  stop(sprintf("No method for object of class \"%s\"", class(study)))
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
