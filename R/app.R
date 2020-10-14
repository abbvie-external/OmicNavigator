
#' List available studies and their metadata
#'
#' @param libraries The directories to search for installed study packages. If
#'   left as \code{NULL} (the default), then
#'   \code{\link[utils]{installed.packages}} will use the result of
#'   \code{\link{.libPaths}}.
#'
#' @export
listStudies <- function(libraries = NULL) {
  studies <- getInstalledStudies(libraries = libraries)

  output <- vector(mode = "list", length = length(studies))
  for (i in seq_along(studies)) {
    output[[i]] <- list()
    studyName <- studies[i]
    output[[i]][["name"]] <- studyName

    # package metadata
    pkgName <- studyToPkg(studyName)
    pkgDescription <- utils::packageDescription(pkgName, lib.loc = libraries)
    output[[i]][["package"]] <- list(
      description = pkgDescription[["Description"]],
      version = pkgDescription[["Version"]],
      buildInfo = pkgDescription[["Built"]],
      OmicNavigatorVersion = pkgDescription[["OmicNavigatorVersion"]]
    )

    if (as.package_version(pkgDescription[["OmicNavigatorVersion"]]) <
        as.package_version(minVersionCompatible)) {
      warning(
        "OmicNavigator version incompatibility\n",
        sprintf("Study \"%s\" was created with version %s\n", studyName,
                pkgDescription[["OmicNavigatorVersion"]]),
        sprintf("OmicNavigator version %s is currently installed\n",
                utils::packageVersion("OmicNavigator")),
        sprintf("It requires study packages to be created with a minimum OmicNavigator version of %s\n",
                minVersionCompatible),
        sprintf("Reinstall the study to avoid any potential issues\n"),
        immediate. = TRUE
      )
    }

    studyDirectory <- getDirectory(studyName, libraries)
    studySummaryFile <- file.path(studyDirectory, "summary.json")
    if (!file.exists(studySummaryFile)) {
      warning(sprintf("Unable to import package %s", pkgName),
              immediate. = TRUE)
      next
    }

    studySummary <- readJson(studySummaryFile, simplifyVector = FALSE)
    output[[i]] <- c(output[[i]], studySummary)
  }

  return(output)
}

#' Get the features in a network node
#'
#' @param study An OmicNavigator study. Only accepts name of installed study
#'   package.
#' @inheritParams shared-get
#' @inheritParams listStudies
#'
#' @seealso \code{\link{getLinkFeatures}}
#'
#' @export
getNodeFeatures <- function(study, annotationID, termID, libraries = NULL) {
  if (inherits(study, "onStudy")) {
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
  metaFeaturesTable <- metaFeaturesTable[, -1, drop = FALSE]
  row.names(metaFeaturesTable) <- NULL

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


#' Get link to report
#'
#' @inheritParams shared-get
#'
#' @export
getReportLink <- function(study, modelID) {
  report <- getReports(study, modelID = modelID)

  if (isUrl(report)) return(report)

  pkgName <- studyToPkg(study)
  installationDir <- dirname(find.package(package = pkgName))
  reportFile <- file.path(installationDir, report)
  if (!file.exists(reportFile)) {
    stop(sprintf("The requested report file does not exist: %s", reportFile))
  }

  return(report)
}
