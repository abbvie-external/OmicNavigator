# Functions called directly by the app

#' List available studies and their metadata
#'
#' @param libraries The directories to search for installed study packages. If
#'   left as \code{NULL} (the default), then
#'   \code{\link[utils]{installed.packages}} will use the result of
#'   \code{\link{.libPaths}}.
#'
#' @return Returns a nested list with one element per installed OmicNavigator
#'   study package. Each study package entry has the following sublist components:
#'
#'   \item{name}{(character) Name of the study}
#'   \item{package}{(list) The fields from \code{DESCRIPTION}}
#'   \item{results}{(nested list) The testIDs available for each modelID}
#'   \item{enrichments}{(nested list) The annotationIDs available for each modelID}
#'   \item{plots}{(nested list) The plotIDs available for each modelID}
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
    pkgDescription <- unclass(pkgDescription)
    attr(pkgDescription, "file") <- NULL
    output[[i]][["package"]] <- pkgDescription
    # For temporary backwards compatibility. The app currently reads
    # "description"
    output[[i]][["package"]] <- c(
      output[[i]][["package"]],
      list(description = pkgDescription[["Description"]])
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

#' Get study metadata
#'
#' Get the study description, version, maintainer, maintainer email, and any
#' extra metadata added via the argument \code{studyMeta} of
#' \code{\link{createStudy}}.
#'
#' @inheritParams importStudy
#'
#' @return Returns a list with the following components:
#'
#'   \item{description}{(character) Study description}
#'   \item{version}{(character) Study version}
#'   \item{maintainer}{(character) Study maintainer}
#'   \item{maintainerEmail}{(character) Study maintainer email}
#'   \item{studyMeta}{(list) Additional study metadata added via the argument
#'                    \code{studyMeta} of \code{\link{createStudy})}}
#'
#' @seealso \code{\link{createStudy}}
#'
#' @export
getStudyMeta <- function(study, libraries = NULL) {
  pkg <- studyToPkg(study)
  if (!requireNamespace(pkg, lib.loc = libraries, quietly = TRUE)) {
    stop("The package ", pkg, " is not installed")
  }
  # Need to unload package namespace so that it doesn't interfere with find.package()
  on.exit(unloadNamespace(pkg), add = TRUE)

  # Import info from DESCRIPTION
  description <- utils::packageDescription(pkg, lib.loc = libraries)
  if (is.null(description[["Maintainer"]])) {
    description[["Maintainer"]] <- "Unknown <unknown@unknown>"
    message(
      "This study package didn't have a maintainer listed",
      "\nUsing the placeholder: ", description[["Maintainer"]],
      "\nHighly recommended to update this with your own name and email:",
      "\n<name of study object>$maintainer <- \"<Your Name>\"",
      "\n<name of study object>$maintainer <- \"<youremail@domain.com>\"",
      "\n(replace the text in between the brackets; make sure to delete the brackets)"
    )
  }
  maintainerField <- strsplit(description[["Maintainer"]], "<|>")[[1]]
  maintainer <- sub("[[:space:]]$", "", maintainerField[1])
  maintainerEmail <- maintainerField[2]
  descriptionFieldsReservedFile <- system.file(
    "extdata/description-fields-reserved.txt",
    package = "OmicNavigator",
    mustWork = TRUE
  )
  descriptionFieldsReserved <- scan(
    file = descriptionFieldsReservedFile,
    what = character(),
    quiet = TRUE
  )
  studyMeta <- description[setdiff(names(description), descriptionFieldsReserved)]

  studyMeta <- c(
    description = description[["Description"]],
    version = description[["Version"]],
    maintainer = maintainer,
    maintainerEmail = maintainerEmail,
    studyMeta = list(studyMeta)
  )
  return(studyMeta)
}

#' Get results table from a study
#'
#' @inheritParams shared-get
#' @inheritParams listStudies
#'
#' @return A data frame which includes the columns from the features table
#'   followed by the columns from the results table. All the columns from the
#'   features table will be character strings, even if the values appear
#'   numeric.
#'
#'   If the optional arguments \code{annotationID} and \code{termID} are
#'   provided, the table will be filtered to only include features in that
#'   annotation term.
#'
#' @export
getResultsTable <- function(study, modelID, testID, annotationID = NULL, termID = NULL, libraries = NULL) {
  results <- getResults(study, modelID, testID)
  features <- getFeatures(study, modelID, quiet = TRUE)

  if (isEmpty(results)) return(data.frame())
  if (isEmpty(features)) return(results)

  # Results must be first argument to preserve input order
  resultsTable <- merge(results, features, by = 1,
                        all.x = TRUE, all.y = FALSE, sort = FALSE)
  # Rearrange columns so that features are listed first
  columnsOrder <- c(colnames(features),
                    setdiff(colnames(results), colnames(features)))
  resultsTable <- resultsTable[, columnsOrder]

  if (!is.null(annotationID) && !is.null(termID)) {
    termFeatures <- getNodeFeatures(study, annotationID, termID, libraries = libraries)
    annotationIDfeatureID <- getAnnotations(study, annotationID = annotationID)[["featureID"]]
    featureIDcolumn <- which(colnames(resultsTable) == annotationIDfeatureID)
    resultsTable <- resultsTable[resultsTable[[featureIDcolumn]] %in% termFeatures, ]
  }

  return(resultsTable)
}

#' Get enrichments table from a study
#'
#' @inheritParams shared-get
#' @inheritParams shared-upset
#' @inheritParams listStudies
#'
#' @return A data frame of enrichments with the following columns:
#'
#'   \item{termID}{The unique ID for the annotation term}
#'   \item{description}{The description of the annotation term}
#'   \item{...}{One column for each of the enrichments}
#'
#' @export
getEnrichmentsTable <- function(study, modelID, annotationID, type = "nominal", libraries = NULL) {
  stopifnot(type %in% c("nominal", "adjusted"))

  enrichments <- getEnrichments(
    study,
    modelID = modelID,
    annotationID = annotationID,
    libraries = libraries
  )

  if (isEmpty(enrichments)) return(data.frame())

  enrichmentsTable <- combineListIntoTable(enrichments, "testID")

  enrichmentsTableWide <- enrichmentsToWide(enrichmentsTable, type = type)

  return(enrichmentsTableWide)
}

#' Get enrichments network from a study
#'
#' @inheritParams shared-get
#' @inheritParams listStudies
#'
#' @return Returns a list with the following components:
#'
#'   \item{tests}{(character) Vector of testIDs}
#'   \item{nodes}{(data frame) The description of each annotation term (i.e.
#'   node). The nominal and adjusted p-values are in list-columns.}
#'   \item{links}{(list) The statistics for each pairwise overlap between the
#'   annotation terms (i.e. nodes)}
#'
#' @importFrom data.table ":=" "%chin%" .N
#' @export
getEnrichmentsNetwork <- function(study, modelID, annotationID, libraries = NULL) {

  annotation <- getAnnotations(
    study,
    annotationID = annotationID,
    libraries = libraries
  )
  if (isEmpty(annotation)) return(list())

  termsVec <- annotation[["terms"]]
  if (isEmpty(termsVec)) {
    message(sprintf("No terms available for annotationID \"%s\"", annotationID))
    return(list())
  }
  terms <- data.table::data.table(
    termID = names(termsVec),
    geneSetSize = lengths(termsVec)
  )

  enrichments <- getEnrichments(study, modelID = modelID, annotationID = annotationID)
  if (isEmpty(enrichments)) return(list())
  enrichmentsTable <- combineListIntoTable(enrichments, "testID")
  data.table::setDT(enrichmentsTable)

  nodesLong <- data.table::merge.data.table(
    x = enrichmentsTable,
    y = terms,
    by = "termID",
    all.x = TRUE,
    all.y = FALSE,
    sort = FALSE
  )
  data.table::setorderv(nodesLong, cols = "testID")

  tests <- unique(nodesLong[["testID"]])

  adjusted <- id <- nominal <- NULL # for R CMD check
  nodes <- nodesLong[
    ,
    list(nominal = list(nominal), adjusted = list(adjusted)),
    by = c("termID", "description", "geneSetSize")
  ]
  nodes[, id := seq_len(.N)]
  data.table::setcolorder(nodes, "id")

  overlaps <- getOverlaps(
    study,
    annotationID = annotationID,
    libraries = libraries
  )
  if (isEmpty(overlaps)) return(list())

  links <- data.table::setDT(overlaps)
  data.table::setnames(
    links,
    old = c("term1", "term2"),
    new = c("source", "target")
  )
  links <- links[links[["source"]] %chin% nodes[["termID"]] &
                   links[["target"]] %chin% nodes[["termID"]], ]
  links[, id := seq_len(.N)]
  data.table::setcolorder(links, "id")

  # Use node IDs with links
  links[["source"]] <- data.table::chmatch(links[["source"]], nodes[["termID"]])
  links[["target"]] <- data.table::chmatch(links[["target"]], nodes[["termID"]])

  enrichmentsNetwork <- list(
    tests = tests,
    nodes = data.table::setDF(nodes),
    links = data.table::setDF(links)
  )

  return(enrichmentsNetwork)
}

#' Get the features in a network node
#'
#' @param study An OmicNavigator study. Only accepts name of installed study
#'   package.
#' @inheritParams shared-get
#' @inheritParams listStudies
#'
#' @return Returns a character vector with the features in the termID
#'
#' @seealso \code{\link{getLinkFeatures}}
#'
#' @export
getNodeFeatures <- function(study, annotationID, termID, libraries = NULL) {

  stopifnot(
    is.character(annotationID), length(annotationID) == 1,
    is.character(termID), length(termID) == 1
  )

  annotation <- getAnnotations(
    study,
    annotationID = annotationID,
    libraries = libraries
  )
  if (isEmpty(annotation)) return(character())

  termsAvailable <- names(annotation[["terms"]])
  if (!termID %in% termsAvailable) {
    message(sprintf("The termID \"%s\" is not available for annotationID \"%s\"",
                    termID, annotationID))
    return(character())
  }

  nodeFeatures <- sort(annotation[["terms"]][[termID]])

  return(nodeFeatures)
}

#' Get the shared features in a network link
#'
#' @param termID1,termID2 Linked terms to find overlapping features
#' @inheritParams getNodeFeatures
#' @inheritParams shared-get
#' @inheritParams listStudies
#'
#' @return Returns a character vector with the features included in both termIDs
#'   (i.e. the intersection)
#'
#' @seealso \code{\link{getNodeFeatures}}
#'
#' @export
getLinkFeatures <- function(study, annotationID, termID1, termID2, libraries = NULL) {

  nodeFeatures1 <- getNodeFeatures(study, annotationID, termID1, libraries = libraries)
  nodeFeatures2 <- getNodeFeatures(study, annotationID, termID2, libraries = libraries)

  linkFeatures <- sort(intersect(nodeFeatures1, nodeFeatures2))

  return(linkFeatures)
}

#' Get metaFeatures for a given feature
#'
#' @inheritParams shared-get
#' @inheritParams listStudies
#'
#' @return Returns a data frame with the metaFeatures for the provided
#'   featureID. If the featureID is not found in the metaFeatures table, the
#'   data frame will have zero rows.
#'
#' @seealso \code{\link{addMetaFeatures}}, \code{\link{getMetaFeatures}}
#'
#' @export
getMetaFeaturesTable <- function(study, modelID, featureID, libraries = NULL) {
  metaFeatures <- getMetaFeatures(study, modelID = modelID, libraries = libraries)
  if (isEmpty(metaFeatures)) return(data.frame())

  metaFeaturesTable <- metaFeatures[metaFeatures[, 1] == featureID, ]
  metaFeaturesTable <- metaFeaturesTable[, -1, drop = FALSE]
  row.names(metaFeaturesTable) <- NULL

  if (nrow(metaFeaturesTable) == 0) {
    message(sprintf("No metaFeatures found for featureID \"%s\"", featureID))
  }

  return(metaFeaturesTable)
}

#' Get data for barcode and violin plots
#'
#' @inheritParams shared-get
#' @inheritParams listStudies
#'
#' @return A list with the following components:
#'
#'   \item{data}{Data frame with the differential statistics to plot}
#'   \item{highest}{(numeric) The largest differential statistic, rounded up to
#'   the next integer}
#'   \item{lowest}{(numeric) The lowest differential statistic, rounded down to the next integer}
#'   \item{labelStat}{(character) The x-axis label to describe the differential
#'   statistic}
#'   \item{labelLow}{(character) The vertical axis label on the left to describe
#'   smaller values (default is "Low")}
#'   \item{labelHigh}{(character) The vertical axis label on the right to
#'   describe larger values (default is "High")}
#'
#' @seealso \code{\link{addBarcodes}}, \code{\link{getBarcodes}}
#'
#' @export
getBarcodeData <- function(study, modelID, testID, annotationID, termID, libraries = NULL) {

  resultsTable <- getResultsTable(study, modelID = modelID, testID = testID, libraries = libraries)
  if (isEmpty(resultsTable)) return(list())
  barcodes <- getBarcodes(study, modelID = modelID, libraries = libraries)
  if (isEmpty(barcodes)) return(list())

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

  annotations <- getAnnotations(study, annotationID = annotationID, libraries = libraries)
  if (isEmpty(annotations)) return(list())
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
                               by = annotations[["featureID"]], sort = FALSE)

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
  # No point in keeping the original row names prior to re-ordering
  row.names(barcodeDataTable) <- NULL

  newList <- list(
    data = barcodeDataTable,
    highest = ceiling(max(barcodeDataTable[, "statistic"])),
    lowest = floor(min(barcodeDataTable[, "statistic"])),
    labelStat = barcodes[["labelStat"]],
    labelLow = barcodes[["labelLow"]],
    labelHigh = barcodes[["labelHigh"]]
  )
  return (newList)
}

#' Get link to report
#'
#' @inheritParams shared-get
#' @inheritParams listStudies
#'
#' @return Returns a one-element character vector with either a path to a report
#'   file or a URL to a report web page. If no report is available for the
#'   modelID, an empty character vector is returned.
#'
#' @export
getReportLink <- function(study, modelID, libraries = NULL) {
  report <- getReports(study, modelID = modelID, libraries = libraries)
  if (isEmpty(report)) return(character())

  if (isUrl(report)) return(report)

  pkgName <- studyToPkg(study)
  installationDir <- dirname(find.package(package = pkgName, lib.loc = libraries))
  reportFile <- file.path(installationDir, report)
  if (!file.exists(reportFile)) {
    stop(sprintf("The requested report file does not exist: %s", reportFile))
  }

  return(report)
}

#' Get version of OmicNavigator package
#'
#' This is a convenience function for the app. It is easier to always call the
#' OmicNavigator package functions via OpenCPU than to call the utils package
#' for this one endpoint.
#'
#' @param libraries Directory path(s) to R package library(ies). Passed to
#' the argument \code{lib.loc} of \code{\link[utils]{packageVersion}}.
#'
#' @return Returns a one-element character vector with the version of the
#'   currently installed OmicNavigator R package
#'
#' @seealso \code{\link[utils]{packageVersion}}
#'
#' @export
getPackageVersion <- function(libraries = NULL) {
  as.character(utils::packageVersion("OmicNavigator", lib.loc = libraries))
}

#' Get favicon URLs for table linkouts
#'
#' To enhance the display of the linkouts in the app's tables, it can fetch the
#' favicon URL for each website.
#'
#' @param linkouts Character vector or (potentially nested) list of character
#'   vectors containing the URLs for the table linkouts.
#'
#' @return The URLs to the favicons for each linkout. The output returned will
#'   always be the same class and structure as the input.
#'
#' @seealso \code{\link{getResultsLinkouts}},
#'          \code{\link{getEnrichmentsLinkouts}}
#'
#' @export
getFavicons <- function(linkouts) {
  if (is.list(linkouts)) {
    favicons <- rapply(linkouts, faviconFunction, how = "replace")
  } else {
    favicons <- faviconFunction(linkouts)
  }

  return(favicons)
}

faviconFunction <- function(x) {
  if (!is.character(x)) {
    warning("faviconFunction requires character vector as input")
    return("")
  }

  favicons <- faviconPlease::faviconPlease(x)

  return(favicons)
}
