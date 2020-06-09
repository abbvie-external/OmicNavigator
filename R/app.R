
#' List available studies and their metadata
#'
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

    con <- connectDatabase(studyName, libraries = libraries)

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
#' @importFrom dplyr "%>%"
#' @importFrom rlang "!!"
#' @importFrom rlang ".data"
#' @export
getNodeFeatures <- function(study, annotationID, termID, libraries = NULL) {
  con <- connectDatabase(study, libraries = libraries)
  on.exit(disconnectDatabase(con))

  terms <- dplyr::tbl(con, "terms") %>%
    dplyr::filter(.data$annotationID == !! annotationID,
                  .data$termID == !! termID) %>%
    dplyr::collect()

  if (nrow(terms) == 0) {
    stop("Invalid filters.\n",
         sprintf("annotationID: \"%s\"\n", annotationID),
         sprintf("termID: \"%s\"\n", termID)
    )
  }

  nodeFeatures <- sort(terms[["featureID"]])

  return(nodeFeatures)
}

#' Get the shared features in a network link
#'
#' @export
getLinkFeatures <- function(study, annotationID, termID1, termID2) {

  nodeFeatures1 <- getNodeFeatures(study, annotationID, termID1)
  nodeFeatures2 <- getNodeFeatures(study, annotationID, termID2)

  linkFeatures <- sort(intersect(nodeFeatures1, nodeFeatures2))

  return(linkFeatures)
}

#' Get data for barcode and violin plots
#'
#' @export
getBarcodeData <- function(study, modelID, testID, annotationID, termID) {
  # Adapted from: ***REMOVED***/blob/7560460792780289eb45eb18567d8904a0f0d40d/R/getBarcodeData.R

  results <- getResults(study, modelID = modelID, testID = testID)
  barcodes <- getBarcodes(study, modelID = modelID)
  termFeatures <- getNodeFeatures(study, annotationID, termID)

  if (!barcodes[["statistic"]] %in% colnames(results)) {
    stop(sprintf("The statistic \"%s\" is not available in the results table"),
         barcodes[["statistic"]])
  }

  termFeaturesTable <- data.frame(
    featureID = termFeatures,
    stringsAsFactors = FALSE
  )

  barcodeDataTable <- merge(termFeaturesTable, results, by = 1)
  statisticCol <- which(colnames(barcodeDataTable) == barcodes[["statistic"]])
  barcodeDataTable <- barcodeDataTable[, c(1, statisticCol)]

  if (barcodes[["absolute"]]) {
    barcodeDataTable[, 2] <- abs(barcodeDataTable[, 2])
  }

  newList <- list(
    data = barcodeDataTable,
    highest = ceiling(max(abs(barcodeDataTable[, 2]))),
    labelStat = barcodes[["labelStat"]],
    labelLow = barcodes[["labelLow"]],
    labelHigh = barcodes[["labelHigh"]]
  )
  return (newList)
}
