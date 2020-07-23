
#' List available studies and their metadata
#'
#' @param libraries The directories to search for installed study packages. If
#'   left as \code{NULL} (the default), then
#'   \code{\link[utils]{installed.packages}} will use the result of
#'   \code{\link{.libPaths}}.
#'
#' @importFrom dplyr "%>%"
#' @importFrom rlang ".data"
#' @export
listStudies <- function(libraries = NULL) {
  pkgsAll <- rownames(utils::installed.packages(lib.loc = libraries))
  names(pkgsAll) <- NULL
  pkgsOa <- grep("^OAstudy", pkgsAll, value = TRUE)
  studies <- sub("^OAstudy", "", pkgsOa)
  studies <- sort(studies)

  output <- vector(mode = "list", length = length(studies))
  for (i in seq_along(studies)) {
    output[[i]] <- list()
    studyName <- studies[i]
    output[[i]][["name"]] <- studyName

    # package metadata
    pkgName <- pkgsOa[i]
    pkgDescription <- utils::packageDescription(pkgName)
    output[[i]][["package"]] <- list(
      description = pkgDescription[["Description"]],
      version = pkgDescription[["Version"]],
      buildInfo = pkgDescription[["Built"]],
      OmicAnalyzerVersion = pkgDescription[["OmicAnalyzerVersion"]]
    )

    if (as.package_version(pkgDescription[["OmicAnalyzerVersion"]]) <
        as.package_version(minVersionCompatible)) {
      warning(
        "OmicAnalyzer version incompatibility\n",
        sprintf("Study \"%s\" was created with version %s\n", studyName,
                pkgDescription[["OmicAnalyzerVersion"]]),
        sprintf("OmicAnalyzer version %s is currently installed\n",
                utils::packageVersion("OmicAnalyzer")),
        sprintf("It requires study packages to be created with a minimum OmicAnalyzer version of %s\n",
                minVersionCompatible),
        sprintf("Reinstall the study to avoid any potential issues\n"),
        immediate. = TRUE
      )
    }

    attempt <- try(
      con <- connectDatabase(studyName, libraries = libraries),
      silent = TRUE
    )
    if (inherits(attempt, "try-error")) {
      warning(sprintf("Unable to import package %s", pkgName),
              immediate. = TRUE)
      next
    }

    # Results available
    output[[i]][["results"]] <- list()
    models <- dplyr::tbl(con, "models")
    tests <- dplyr::tbl(con, "tests")
    results <- dplyr::tbl(con, "results") %>%
      dplyr::select(.data$modelID, .data$testID) %>%
      dplyr::distinct() %>%
      dplyr::arrange(.data$modelID, .data$testID) %>%
      dplyr::left_join(models, by = "modelID") %>%
      dplyr::left_join(tests, by = "testID", suffix = c(".model", ".test")) %>%
      dplyr::rename(modelID = .data$modelID.model) %>%
      dplyr::collect() %>%
      as.data.frame(stringsAsFactors = FALSE)

    resultsModels <- unique(results[["modelID"]])
    for (j in seq_along(resultsModels)) {
      modelID <- resultsModels[j]
      modelDisplay <- unique(results[results[["modelID"]] == modelID,
                                     "description.model"])
      output[[i]][["results"]][[j]] <- list(
        modelID = modelID,
        modelDisplay = modelDisplay
      )
      modelTests <- results[results[["modelID"]] == modelID, "testID"]
      modelTestsDescriptions <- results[results[["modelID"]] == modelID,
                                        "description.test"]
      output[[i]][["results"]][[j]][["tests"]] <- vector("list", length(modelTests))
      for (k in seq_along(modelTests)) {
        output[[i]][["results"]][[j]][["tests"]][[k]] <- list(
          testID = modelTests[k],
          testDisplay = modelTestsDescriptions[k]
        )
      }
    }

    # Enrichments available
    output[[i]][["enrichments"]] <- list()
    annotations <- dplyr::tbl(con, "annotations")
    enrichments <- dplyr::tbl(con, "enrichments") %>%
      dplyr::select(.data$modelID, .data$annotationID) %>%
      dplyr::distinct() %>%
      dplyr::arrange(.data$modelID, .data$annotationID) %>%
      dplyr::left_join(models, by = "modelID") %>%
      dplyr::left_join(annotations, by = "annotationID", suffix = c(".model", ".annotation")) %>%
      dplyr::collect() %>%
      as.data.frame(stringsAsFactors = FALSE)

    enrichmentsModels <- unique(enrichments[["modelID"]])
    for (j in seq_along(enrichmentsModels)) {
      modelID <- enrichmentsModels[j]
      modelDisplay <- unique(enrichments[enrichments[["modelID"]] == modelID,
                                         "description.model"])
      output[[i]][["enrichments"]][[j]] <- list(
        modelID = modelID,
        modelDisplay = modelDisplay
      )
      modelAnnotations <- enrichments[enrichments[["modelID"]] == modelID, "annotationID"]
      modelAnnotationsDescriptions <- enrichments[enrichments[["modelID"]] == modelID,
                                                  "description.annotation"]
      output[[i]][["enrichments"]][[j]][["annotations"]] <- vector("list", length(modelAnnotations))
      for (k in seq_along(modelAnnotations)) {
        output[[i]][["enrichments"]][[j]][["annotations"]][[k]] <- list(
          annotationID = modelAnnotations[k],
          annotationDisplay = modelAnnotationsDescriptions[k]
        )
      }
    }

    # Plots available
    plotsModels <- unique(c(resultsModels, enrichmentsModels))
    modelsTable <- dplyr::collect(models)
    plotsModelsDescriptions <- modelsTable[["description"]][
      match(plotsModels, modelsTable[["modelID"]])
    ]
    output[[i]][["plots"]] <- vector("list", length(plotsModels))
    for (j in seq_along(plotsModels)) {
      modelID <- plotsModels[j]
      modelDisplay <- plotsModelsDescriptions[j]
      output[[i]][["plots"]][[j]] <- list(
        modelID = modelID,
        modelDisplay = modelDisplay
      )
      plots <- tryCatch(
        getPlots(con, modelID = modelID),
        error = function(e) list()
      )
      plotsDisplay <- vapply(plots, function(x) x[["displayName"]],
                             character(1), USE.NAMES = FALSE)
      output[[i]][["plots"]][[j]][["plots"]] <- vector("list", length(plots))
      for (k in seq_along(plots)) {
        output[[i]][["plots"]][[j]][["plots"]][[k]] <- list(
          plotID = names(plots)[k],
          plotDisplay = plotsDisplay[k]
        )
      }
    }

    disconnectDatabase(con)
  }

  return(output)
}

#' Get the features in a network node
#'
#' @param study An OmicAnalyzer study. Only accepts name of installed study
#'   package.
#' @inheritParams shared-get
#' @inheritParams listStudies
#'
#' @seealso \code{\link{getLinkFeatures}}
#'
#' @export
getNodeFeatures <- function(study, annotationID, termID, libraries = NULL) {
  if (inherits(study, "oaStudy")) {
    stop("\"study\" must be the name of an installed study package")
  }
  stopifnot(
    is.character(study), length(study) == 1,
    is.character(annotationID), length(annotationID) == 1,
    is.character(termID), length(termID) == 1
  )

  annotation <- getAnnotations(
    study,
    annotationID = annotationID,
    libraries = libraries
  )

  termsAvailable <- names(annotation[["terms"]])
  if (!termID %in% termsAvailable) {
    stop(sprintf("The termID \"%s\" is not available for annotationID \"%s\"",
                 termID, annotationID))
  }

  nodeFeatures <- sort(annotation[["terms"]][[termID]])

  return(nodeFeatures)
}

#' Get the shared features in a network link
#'
#' @param termID1,termID2 Linked terms to find overlapping features
#' @inheritParams getNodeFeatures
#' @inheritParams shared-get
#'
#' @seealso \code{\link{getNodeFeatures}}
#'
#' @export
getLinkFeatures <- function(study, annotationID, termID1, termID2) {

  nodeFeatures1 <- getNodeFeatures(study, annotationID, termID1)
  nodeFeatures2 <- getNodeFeatures(study, annotationID, termID2)

  linkFeatures <- sort(intersect(nodeFeatures1, nodeFeatures2))

  return(linkFeatures)
}

#' Get metaFeatures for a given feature
#'
#' @inheritParams shared-get
#'
#' @export
getMetaFeaturesTable <- function(study, modelID, featureID) {
  metaFeatures <- getMetaFeatures(study, modelID = modelID)

  metaFeaturesTable <- metaFeatures[metaFeatures[, 1] == featureID, ]
  metaFeaturesTable <- metaFeaturesTable[, -1]

  if (nrow(metaFeaturesTable) == 0) {
    warning(sprintf("No metaFeatures found for featureID \"%s\"", featureID))
  }

  return(metaFeaturesTable)
}

#' Get data for barcode and violin plots
#'
#' @inheritParams shared-get
#'
#' @export
getBarcodeData <- function(study, modelID, testID, annotationID, termID) {
  # Adapted from: ***REMOVED***/blob/7560460792780289eb45eb18567d8904a0f0d40d/R/getBarcodeData.R

  resultsTable <- getResultsTable(study, modelID = modelID, testID = testID)
  barcodes <- getBarcodes(study, modelID = modelID)

  # Default barcode settings. See ?addBarcodes
  if (is.null(barcodes[["logFoldChange"]])) {
    barcodes[["logFoldChange"]] <- NA_character_
  }
  if (is.null(barcodes[["absolute"]])) {
    barcodes[["absolute"]] <- TRUE
  }
  if (is.null(barcodes[["labelStat"]])) {
    barcodes[["labelStat"]] <- barcodes[["statistic"]]
  }
  if (is.null(barcodes[["labelLow"]])) {
    barcodes[["labelLow"]] <- "Low"
  }
  if (is.null(barcodes[["labelHigh"]])) {
    barcodes[["labelHigh"]] <- "High"
  }
  if (is.null(barcodes[["featureDisplay"]])) {
    barcodes[["featureDisplay"]] <- NA_character_
  }

  if (!barcodes[["statistic"]] %in% colnames(resultsTable)) {
    stop(sprintf("The statistic \"%s\" is not available in the results table",
         barcodes[["statistic"]]))
  }

  annotations <- getAnnotations(study, annotationID = annotationID)
  if (!termID %in% names(annotations[["terms"]])) {
    stop(sprintf("The term \"%s\" is not available for the annotation \"%s\"",
         termID, annotationID))
  }
  termFeatures <- annotations[["terms"]][[termID]]

  # `featureID` - The unique feature variable used in the inference results table
  # `featureEnrichment` - The feature variable used to perform the enrichment
  #                       analysis with the given annotation database
  # `featureDisplay` - The feature variable to use to label the barcode plot
  #                    on hover
  featureID <- colnames(resultsTable)[1]
  featureEnrichment <- annotations[["featureID"]]
  if (is.na(barcodes[["featureDisplay"]]) ||
      is.null(barcodes[["featureDisplay"]])) {
    featureDisplay <- featureEnrichment
  } else {
    featureDisplay <- barcodes[["featureDisplay"]]
  }

  if (!featureEnrichment %in% colnames(resultsTable)) {
    stop(sprintf("The feature variable \"%s\" used by annotation \"%s\" is not available in the results for the model \"%s\"",
         featureEnrichment, annotationID, modelID))
  }

  if (!featureDisplay %in% colnames(resultsTable)) {
    stop(sprintf("The feature variable \"%s\" for display in the barcode plot is not available in the results for the model \"%s\"",
         featureDisplay, modelID))
  }

  termFeaturesTable <- data.frame(termFeatures, stringsAsFactors = FALSE)
  colnames(termFeaturesTable) <- featureEnrichment

  barcodeDataTableAll <- merge(termFeaturesTable, resultsTable,
                               by = annotations[["featureID"]])

  barcodeDataTable <- data.frame(
    barcodeDataTableAll[[featureID]],
    barcodeDataTableAll[[featureEnrichment]],
    barcodeDataTableAll[[featureDisplay]],
    barcodeDataTableAll[[barcodes[["statistic"]]]],
    stringsAsFactors = FALSE
  )
  colnames(barcodeDataTable) <- c("featureID", "featureEnrichment",
                                  "featureDisplay", "statistic")

  if (is.na(barcodes[["logFoldChange"]]) ||
      is.null(barcodes[["logFoldChange"]])) {
    barcodeDataTable[, "logFoldChange"] <- 0
  } else {
    if (!barcodes[["logFoldChange"]] %in% colnames(resultsTable)) {
      stop(sprintf("The column \"%s\" is not available in the results table",
           barcodes[["logFoldChange"]]))
    }
    barcodeDataTable[, "logFoldChange"] <- barcodeDataTableAll[[barcodes[["logFoldChange"]]]]
  }

  if (barcodes[["absolute"]]) {
    barcodeDataTable[, "statistic"] <- abs(barcodeDataTable[, "statistic"])
  }

  # Sort the barcode results by "statistic"
  rowsOrdered <- order(barcodeDataTable[, "statistic"], decreasing = TRUE)
  barcodeDataTable <- barcodeDataTable[rowsOrdered, ]

  newList <- list(
    data = barcodeDataTable,
    highest = ceiling(max(abs(barcodeDataTable[, "statistic"]))),
    labelStat = barcodes[["labelStat"]],
    labelLow = barcodes[["labelLow"]],
    labelHigh = barcodes[["labelHigh"]]
  )
  return (newList)
}
