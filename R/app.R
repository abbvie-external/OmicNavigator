
#' List available studies and their metadata
#'
#' @export
listStudies <- function(libraries = NULL) {
  pkgsAll <- rownames(utils::installed.packages(lib.loc = libraries))
  names(pkgsAll) <- NULL
  pkgsOa <- grep("^OAstudy", pkgsAll, value = TRUE)
  studies <- sub("^OAstudy", "", pkgsOa)
  studies <- sort(studies)

  output <- list()
  for (i in seq_along(studies)) {
    studyName <- studies[i]
    output[[studyName]] <- list()

    # package metadata
    pkgName <- pkgsOa[i]
    pkgDescription <- utils::packageDescription(pkgName)
    output[[studyName]][["package"]] <- list(
      description = pkgDescription[["Description"]],
      version = pkgDescription[["Version"]],
      buildInfo = pkgDescription[["Built"]]
    )

    con <- connectDatabase(studyName, libraries = libraries)

    # Results available
    output[[studyName]][["results"]] <- list()
    models <- dplyr::tbl(con, "models")
    tests <- dplyr::tbl(con, "tests")
    results <- dplyr::tbl(con, "results") %>%
      dplyr::select(modelID, testID) %>%
      dplyr::distinct() %>%
      dplyr::arrange(modelID, testID) %>%
      dplyr::left_join(models, by = "modelID") %>%
      dplyr::left_join(tests, by = "testID", suffix = c(".model", ".test")) %>%
      dplyr::collect() %>%
      as.data.frame(stringsAsFactors = FALSE)

    resultsModels <- unique(results[["modelID"]])
    for (j in seq_along(resultsModels)) {
      modelName <- resultsModels[j]
      modelDescription <- unique(results[results[["modelID"]] == modelName,
                                         "description.model"])
      output[[studyName]][["results"]][[modelName]] <- list(
        description = modelDescription)
      modelTests <- results[results[["modelID"]] == modelName, "testID"]
      modelTestsDescriptions <- results[results[["modelID"]] == modelName,
                                        "description.test"]
      modelTestsList <- as.list(modelTestsDescriptions)
      names(modelTestsList) <- modelTests
      output[[studyName]][["results"]][[modelName]][["tests"]] <- modelTestsList
    }

    # Enrichments available
    output[[studyName]][["enrichments"]] <- list()
    annotations <- dplyr::tbl(con, "annotations")
    enrichments <- dplyr::tbl(con, "enrichments") %>%
      dplyr::select(modelID, annotationID) %>%
      dplyr::distinct() %>%
      dplyr::arrange(modelID, annotationID) %>%
      dplyr::left_join(models, by = "modelID") %>%
      dplyr::left_join(annotations, by = "annotationID", suffix = c(".model", ".annotation")) %>%
      dplyr::collect() %>%
      as.data.frame(stringsAsFactors = FALSE)

    enrichmentsModels <- unique(enrichments[["modelID"]])
    for (j in seq_along(enrichmentsModels)) {
      modelName <- enrichmentsModels[j]
      modelDescription <- unique(enrichments[enrichments[["modelID"]] == modelName,
                                             "description.model"])
      output[[studyName]][["enrichments"]][[modelName]] <- list(
        description = modelDescription)
      modelAnnotations <- enrichments[enrichments[["modelID"]] == modelName, "annotationID"]
      modelAnnotationsDescriptions <- enrichments[enrichments[["modelID"]] == modelName,
                                                  "description.annotation"]
      modelAnnotationsList <- as.list(modelAnnotationsDescriptions)
      names(modelAnnotationsList) <- modelAnnotations
      output[[studyName]][["enrichments"]][[modelName]][["annotations"]] <- modelAnnotationsList
    }

    disconnectDatabase(con)
  }

  return(output)
}
