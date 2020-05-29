
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
      modelName <- resultsModels[j]
      modelDescription <- unique(results[results[["modelID"]] == modelName,
                                         "description.model"])
      output[[i]][["results"]][[modelName]] <- list(
        description = modelDescription)
      modelTests <- results[results[["modelID"]] == modelName, "testID"]
      modelTestsDescriptions <- results[results[["modelID"]] == modelName,
                                        "description.test"]
      modelTestsList <- as.list(modelTestsDescriptions)
      names(modelTestsList) <- modelTests
      output[[i]][["results"]][[modelName]][["tests"]] <- modelTestsList
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
      modelName <- enrichmentsModels[j]
      modelDescription <- unique(enrichments[enrichments[["modelID"]] == modelName,
                                             "description.model"])
      output[[i]][["enrichments"]][[modelName]] <- list(
        description = modelDescription)
      modelAnnotations <- enrichments[enrichments[["modelID"]] == modelName, "annotationID"]
      modelAnnotationsDescriptions <- enrichments[enrichments[["modelID"]] == modelName,
                                                  "description.annotation"]
      modelAnnotationsList <- as.list(modelAnnotationsDescriptions)
      names(modelAnnotationsList) <- modelAnnotations
      output[[i]][["enrichments"]][[modelName]][["annotations"]] <- modelAnnotationsList
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
