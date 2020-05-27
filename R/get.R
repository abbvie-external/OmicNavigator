#' Get installed OmicAnalyzer studies
#'
#' @param libraries Character vector of library directories to search for study
#'   packages. If \code{NULL}, uses \code{.libPaths}.
#'
#' @return character vector of OmicAnalyzer study packages
#'
#' @examples
#'  getInstalledStudies()
#'
#' @export
getInstalledStudies <- function(libraries = NULL) {
  pkgs_all <- rownames(utils::installed.packages(lib.loc = libraries))
  names(pkgs_all) <- NULL
  pkgs_oa <- grep("^OAstudy", pkgs_all, value = TRUE)
  studies <- sub("^OAstudy", "", pkgs_oa)
  studies <- sort(studies)

  return(studies)
}

#' Get models from a study
#'
#' @export
getModels <- function(study, modelID = NULL, ...) {
  UseMethod("getModels")
}

#' @rdname getModels
#' @export
getModels.oaStudy <- function(study, modelID = NULL, ...) {
  models <- study[["models"]]
  if (is.null(models)) {
    stop(sprintf("No models available for study \"%s\"", study[["name"]]))
  }

  if (!is.null(modelID)) {
    stopifnot(is.character(modelID), length(modelID) == 1)
    if (!modelID %in% names(models)) {
      stop(sprintf("Study \"%s\" has no model \"%s\"", study[["name"]], modelID))
    }
    models <- models[modelID]
  }

  return(models)
}

#' @rdname getModels
#' @importFrom rlang "!!"
#' @importFrom rlang ".data"
#' @export
getModels.SQLiteConnection <- function(study, modelID = NULL, ...) {

  df_models <- dplyr::tbl(study, "models")
  if (!is.null(modelID)) {
    stopifnot(is.character(modelID), length(modelID) == 1)
    df_models <- dplyr::filter(df_models, .data$modelID == !! modelID)
  }
  df_models <- dplyr::collect(df_models)
  if (nrow(df_models) == 0) {
    stop(sprintf("Invalid modelID: \"%s\"", modelID))
  }

  models <- as.list(df_models[["description"]])
  names(models) <- df_models[["modelID"]]
  return(models)
}

#' @rdname getModels
#' @export
getModels.character <- function(study, modelID = NULL, libraries = NULL, ...) {
  con <- connectDatabase(study, libraries = libraries)
  on.exit(disconnectDatabase(con))

  models <- getModels(con, modelID = modelID, ...)

  return(models)
}

#' @export
getModels.default <- function(study, modelID = NULL, ...) {
  stop(sprintf("No method for object of class \"%s\"", class(study)))
}

#' Get samples from a study
#'
#' @export
getSamples <- function(study, modelID = NULL, ...) {
  UseMethod("getSamples")
}

#' @rdname getSamples
#' @export
getSamples.oaStudy <- function(study, modelID = NULL, ...) {
  samples <- study[["samples"]]

  if (is.null(samples)) {
    stop(sprintf("No samples available for study \"%s\"", study[["name"]]))
  }

  if (is.null(modelID)) return(samples)

  stopifnot(is.character(modelID), length(modelID) == 1)
  samplesModels <- names(samples)
  if (modelID %in% samplesModels) return(samples[[modelID]])
  if ("default" %in% samplesModels) {
    message(sprintf("Returning \"default\" samples for model \"%s\"", modelID))
    return(samples[["default"]])
  }

  stop(sprintf("No samples available for model \"%s\"", modelID))
}

#' @rdname getSamples
#' @export
getSamples.SQLiteConnection <- function(study, modelID = NULL, ...) {

  dbTables <- DBI::dbListTables(study)
  dbSamples <- grep("^samples-", dbTables, value = TRUE)
  samplesModels <- sub("^samples-", "", dbSamples)

  if (isEmpty(dbSamples)) {
    stop(sprintf("No samples available for study \"%s\"", study[["name"]]))
  }

  if (is.null(modelID)) {
    samples <- lapply(dbSamples, function(x) DBI::dbReadTable(study, x))
    names(samples) <- samplesModels
    return(samples)
  }

  stopifnot(is.character(modelID), length(modelID) == 1)
  if (modelID %in% samplesModels) {
    tableName <- paste("samples", modelID, sep = "-")
    samples <- DBI::dbReadTable(study, tableName)
    return(samples)
  }
  if ("default" %in% samplesModels) {
    message(sprintf("Returning \"default\" samples for model \"%s\"", modelID))
    tableName <- paste("samples", "default", sep = "-")
    samples <- DBI::dbReadTable(study, tableName)
    return(samples)
  }

  stop(sprintf("No samples available for model \"%s\"", modelID))
}

#' @rdname getSamples
#' @export
getSamples.character <- function(study, modelID = NULL, libraries = NULL, ...) {
  con <- connectDatabase(study, libraries = libraries)
  on.exit(disconnectDatabase(con))

  samples <- getSamples(con, modelID = modelID, ...)

  return(samples)
}

#' @export
getSamples.default <- function(study, modelID = NULL, ...) {
  stop(sprintf("No method for object of class \"%s\"", class(study)))
}

#' Get features from a study
#'
#' @export
getFeatures <- function(study, modelID = NULL, ...) {
  UseMethod("getFeatures")
}

#' @rdname getFeatures
#' @export
getFeatures.oaStudy <- function(study, modelID = NULL, ...) {
  features <- study[["features"]]

  if (is.null(features)) {
    stop(sprintf("No features available for study \"%s\"", study[["name"]]))
  }

  if (is.null(modelID)) return(features)

  stopifnot(is.character(modelID), length(modelID) == 1)
  featuresModels <- names(features)
  if (modelID %in% featuresModels) return(features[[modelID]])
  if ("default" %in% featuresModels) {
    message(sprintf("Returning \"default\" features for model \"%s\"", modelID))
    return(features[["default"]])
  }

  stop(sprintf("No features available for model \"%s\"", modelID))
}

#' @rdname getFeatures
#' @export
getFeatures.SQLiteConnection <- function(study, modelID = NULL, ...) {

  dbTables <- DBI::dbListTables(study)
  dbFeatures <- grep("^features-", dbTables, value = TRUE)
  featuresModels <- sub("^features-", "", dbFeatures)

  if (isEmpty(dbFeatures)) {
    stop(sprintf("No features available for study \"%s\"", study[["name"]]))
  }

  if (is.null(modelID)) {
    features <- lapply(dbFeatures, function(x) DBI::dbReadTable(study, x))
    names(features) <- featuresModels
    return(features)
  }

  stopifnot(is.character(modelID), length(modelID) == 1)
  if (modelID %in% featuresModels) {
    tableName <- paste("features", modelID, sep = "-")
    features <- DBI::dbReadTable(study, tableName)
    return(features)
  }
  if ("default" %in% featuresModels) {
    message(sprintf("Returning \"default\" features for model \"%s\"", modelID))
    tableName <- paste("features", "default", sep = "-")
    features <- DBI::dbReadTable(study, tableName)
    return(features)
  }

  stop(sprintf("No features available for model \"%s\"", modelID))
}

#' @rdname getFeatures
#' @export
getFeatures.character <- function(study, modelID = NULL, libraries = NULL, ...) {
  con <- connectDatabase(study, libraries = libraries)
  on.exit(disconnectDatabase(con))

  features <- getFeatures(con, modelID = modelID, ...)

  return(features)
}

#' @export
getFeatures.default <- function(study, modelID = NULL, ...) {
  stop(sprintf("No method for object of class \"%s\"", class(study)))
}

#' Get assays from a study
#'
#' @export
getAssays <- function(study, modelID = NULL, ...) {
  UseMethod("getAssays")
}

#' @rdname getAssays
#' @export
getAssays.oaStudy <- function(study, modelID = NULL, ...) {
  assays <- study[["assays"]]

  if (is.null(assays)) {
    stop(sprintf("No assays available for study \"%s\"", study[["name"]]))
  }

  if (is.null(modelID)) return(assays)

  stopifnot(is.character(modelID), length(modelID) == 1)
  assaysModels <- names(assays)
  if (modelID %in% assaysModels) return(assays[[modelID]])
  if ("default" %in% assaysModels) {
    message(sprintf("Returning \"default\" assays for model \"%s\"", modelID))
    return(assays[["default"]])
  }

  stop(sprintf("No assays available for model \"%s\"", modelID))
}

#' @rdname getAssays
#' @export
getAssays.SQLiteConnection <- function(study, modelID = NULL, ...) {
  assaysTable <- dplyr::tbl(study, "assays")

  if (!is.null(modelID)) {
    stopifnot(is.character(modelID), length(modelID) == 1)
    assaysTable <- dplyr::filter(assaysTable, .data$modelID == !! modelID)
  }
  assaysTable <- dplyr::collect(assaysTable)
  assaysTable <- as.data.frame(assaysTable)

  if (nrow(assaysTable) == 0) {
    stop("Invalid filters.\n",
         if (is.null(modelID)) "modelID: No filter applied\n"
         else sprintf("modelID: \"%s\"\n", modelID),
    )
  }

  assays <- splitTableIntoList(assaysTable, "modelID")
  assays <- lapply(assays, assaysToWide)
  if (!is.null(modelID)) assays <- assays[[1]]

  return(assays)
}

#' @rdname getAssays
#' @export
getAssays.character <- function(study, modelID = NULL, libraries = NULL, ...) {
  con <- connectDatabase(study, libraries = libraries)
  on.exit(disconnectDatabase(con))

  assays <- getAssays(con, modelID = modelID, ...)

  return(assays)
}

#' @export
getAssays.default <- function(study, modelID = NULL, ...) {
  stop(sprintf("No method for object of class \"%s\"", class(study)))
}

#' Get tests from a study
#'
#' @export
getTests <- function(study, modelID = NULL, ...) {
  UseMethod("getTests")
}

#' @rdname getTests
#' @export
getTests.oaStudy <- function(study, modelID = NULL, ...) {
  tests <- study[["tests"]]

  if (is.null(tests)) {
    stop(sprintf("No tests available for study \"%s\"", study[["name"]]))
  }

  if (is.null(modelID)) return(tests)

  stopifnot(is.character(modelID), length(modelID) == 1)
  testsModels <- names(tests)
  if (modelID %in% testsModels) return(tests[[modelID]])
  if ("default" %in% testsModels) {
    message(sprintf("Returning \"default\" tests for model \"%s\"", modelID))
    return(tests[["default"]])
  }

  stop(sprintf("No tests available for model \"%s\"", modelID))
}

#' @rdname getTests
#' @export
getTests.SQLiteConnection <- function(study, modelID = NULL, ...) {

  testsTable <- dplyr::tbl(study, "tests")
  testsTable <- dplyr::collect(testsTable)
  testsTable <- as.data.frame(testsTable)

  if (is.null(modelID)) {
    tests <- splitTableIntoList(testsTable, "modelID")
    return(tests)
  }

  stopifnot(is.character(modelID), length(modelID) == 1)
  testsModels <- testsTable[["modelID"]]
  if (modelID %in% testsModels) {
    tests <- testsTable[testsTable[["modelID"]] == modelID,
                        c("testID", "description")]
    return(tests)
  }
  if ("default" %in% testsModels) {
    message(sprintf("Returning \"default\" tests for model \"%s\"", modelID))
    tests <- testsTable[testsTable[["modelID"]] == "default",
                        c("testID", "description")]
    return(tests)
  }

  stop(sprintf("No tests available for model \"%s\"", modelID))
}

#' @rdname getTests
#' @export
getTests.character <- function(study, modelID = NULL, libraries = NULL, ...) {
  con <- connectDatabase(study, libraries = libraries)
  on.exit(disconnectDatabase(con))

  tests <- getTests(con, modelID = modelID, ...)

  return(tests)
}

#' @export
getTests.default <- function(study, modelID = NULL, ...) {
  stop(sprintf("No method for object of class \"%s\"", class(study)))
}

#' Get results from a study
#'
#' @export
getResults <- function(study, modelID = NULL, testID = NULL, ...) {
  if (is.null(modelID) && !is.null(testID)) {
    stop("Must specify a model in order to specify a test")
  }

  UseMethod("getResults")
}

#' @rdname getResults
#' @export
getResults.oaStudy <- function(study, modelID = NULL, testID = NULL, ...) {
  results <- study[["results"]]

  if (is.null(results)) {
    stop(sprintf("No results available for study \"%s\"", study[["name"]]))
  }

  if (is.null(modelID)) return(results)

  stopifnot(is.character(modelID), length(modelID) == 1)
  resultsModels <- names(results)
  if (modelID %in% resultsModels) {
    resultsPerModel <- results[[modelID]]
  } else {
    stop(sprintf("No results available for model \"%s\"", modelID))
  }

  if (is.null(testID)) return(resultsPerModel)

  stopifnot(is.character(testID), length(testID) == 1)
  resultsTests <- names(resultsPerModel)
  if (testID %in% resultsTests) {
    resultsPerTest <- resultsPerModel[[testID]]
  } else {
    stop(sprintf("No results available for test \"%s\" for model \"%s\"",
                 testID, modelID))
  }

  return(resultsPerTest)
}

#' @rdname getResults
#' @importFrom rlang "!!"
#' @importFrom rlang ".data"
#' @export
getResults.SQLiteConnection <- function(study, modelID = NULL, testID = NULL, ...) {

  resultsTable <- dplyr::tbl(study, "results")
  if (!is.null(modelID)) {
    stopifnot(is.character(modelID), length(modelID) == 1)
    resultsTable <- dplyr::filter(resultsTable, .data$modelID == !! modelID)
  }
  if (!is.null(testID)) {
    stopifnot(is.character(testID), length(testID) == 1)
    resultsTable <- dplyr::filter(resultsTable, .data$testID == !! testID)
  }
  resultsTable <- dplyr::collect(resultsTable)
  resultsTable <- as.data.frame(resultsTable)

  if (nrow(resultsTable) == 0) {
    stop("Invalid filters.\n",
         if (is.null(modelID)) "modelID: No filter applied\n"
         else sprintf("modelID: \"%s\"\n", modelID),
         if (is.null(testID)) "testID: No filter applied\n"
         else sprintf("testID: \"%s\"\n", testID)
    )
  }

  resultsTable <- tidyr::pivot_wider(resultsTable,
                                     names_from = "resultsVariable",
                                     values_from = "resultsValue")
  resultsTable <- as.data.frame(resultsTable)

  results <- splitTableIntoList(resultsTable, "modelID")
  results <- lapply(results, function(x) splitTableIntoList(x, "testID"))
  if (!is.null(modelID)) results <- results[[1]]
  if (!is.null(testID)) results <- results[[1]]

  return(results)
}

#' @rdname getResults
#' @export
getResults.character <- function(study, modelID = NULL, testID = NULL, libraries = NULL, ...) {
  con <- connectDatabase(study, libraries = libraries)
  on.exit(disconnectDatabase(con))

  results <- getResults(con, modelID = modelID, testID = testID, ...)

  return(results)
}

#' @export
getResults.default <- function(study, modelID = NULL, testID = NULL, ...) {
  stop(sprintf("No method for object of class \"%s\"", class(study)))
}

#' Get results table from a study
#'
#' @export
getResultsTable <- function(study, modelID, testID, ...) {
  UseMethod("getResultsTable")
}

#' @rdname getResultsTable
#' @export
getResultsTable.oaStudy <- function(study, modelID, testID, ...) {
  results <- getResults(study, modelID, testID, ...)
  features <- getFeatures(study, modelID, ...)

  resultsTable <- merge(features, results, by = 1,
                        all.x = FALSE, all.y = TRUE, sort = FALSE)

  return(resultsTable)
}

#' @rdname getResultsTable
#' @export
getResultsTable.SQLiteConnection <- function(study, modelID , testID, ...) {
  results <- getResults(study, modelID, testID, ...)
  features <- getFeatures(study, modelID, ...)

  resultsTable <- merge(features, results, by = 1,
                        all.x = FALSE, all.y = TRUE, sort = FALSE)

  return(resultsTable)
}

#' @rdname getResultsTable
#' @export
getResultsTable.character <- function(study, modelID, testID, libraries = NULL, ...) {
  con <- connectDatabase(study, libraries = libraries)
  on.exit(disconnectDatabase(con))

  results <- getResultsTable(con, modelID = modelID, testID = testID, ...)

  return(results)
}

#' @export
getResultsTable.default <- function(study, modelID, testID, ...) {
  stop(sprintf("No method for object of class \"%s\"", class(study)))
}

#' Get enrichments from a study
#'
#' @export
getEnrichments <- function(study, modelID = NULL, annotationID = NULL, testID = NULL, ...) {
  if (is.null(modelID) && !is.null(annotationID)) {
    stop("Must specify a model in order to specify an annotation")
  }
  if (is.null(modelID) && !is.null(testID)) {
    stop("Must specify a model in order to specify a test")
  }
  if (is.null(annotationID) && !is.null(testID)) {
    stop("Must specify an annotation in order to specify a test")
  }

  UseMethod("getEnrichments")
}

#' @rdname getEnrichments
#' @export
getEnrichments.oaStudy <- function(study, modelID = NULL, annotationID = NULL, testID = NULL, ...) {
  enrichments <- study[["enrichments"]]

  if (is.null(enrichments)) {
    stop(sprintf("No enrichments available for study \"%s\"", study[["name"]]))
  }

  if (is.null(modelID)) return(enrichments)

  stopifnot(is.character(modelID), length(modelID) == 1)
  enrichmentsModels <- names(enrichments)
  if (modelID %in% enrichmentsModels) {
    enrichmentsPerModel <- enrichments[[modelID]]
  } else {
    stop(sprintf("No enrichments available for model \"%s\"", modelID))
  }

  if (is.null(annotationID)) return(enrichmentsPerModel)

  stopifnot(is.character(annotationID), length(annotationID) == 1)
  enrichmentsAnnotations <- names(enrichmentsPerModel)
  if (annotationID %in% enrichmentsAnnotations) {
    enrichmentsPerAnnotation <- enrichmentsPerModel[[annotationID]]
  } else {
    stop(sprintf("No enrichments available for annotation \"%s\" for model \"%s\"",
                 annotationID, modelID))
  }

  if (is.null(testID)) return(enrichmentsPerAnnotation)

  stopifnot(is.character(testID), length(testID) == 1)
  enrichmentsTests <- names(enrichmentsPerAnnotation)
  if (testID %in% enrichmentsTests) {
    enrichmentsPerTest <- enrichmentsPerAnnotation[[testID]]
  } else {
    stop(sprintf("No enrichments available for test \"%s\" for annotation \"%s\" for model \"%s\"",
                 testID, annotationID, modelID))
  }

  return(enrichmentsPerTest)
}

#' @rdname getEnrichments
#' @importFrom rlang "!!"
#' @importFrom rlang ".data"
#' @export
getEnrichments.SQLiteConnection <- function(study, modelID = NULL, annotationID = NULL, testID = NULL, ...) {

  enrichmentsTable <- dplyr::tbl(study, "enrichments")
  if (!is.null(modelID)) {
    stopifnot(is.character(modelID), length(modelID) == 1)
    enrichmentsTable <- dplyr::filter(enrichmentsTable, .data$modelID == !! modelID)
  }
  if (!is.null(annotationID)) {
    stopifnot(is.character(annotationID), length(annotationID) == 1)
    enrichmentsTable <- dplyr::filter(enrichmentsTable, .data$annotationID == !! annotationID)
  }
  if (!is.null(testID)) {
    stopifnot(is.character(testID), length(testID) == 1)
    enrichmentsTable <- dplyr::filter(enrichmentsTable, .data$testID == !! testID)
  }

  enrichmentsTable <- dplyr::collect(enrichmentsTable)
  enrichmentsTable <- as.data.frame(enrichmentsTable)

  if (nrow(enrichmentsTable) == 0) {
    stop("Invalid filters.\n",
         if (is.null(modelID)) "modelID: No filter applied\n"
         else sprintf("modelID: \"%s\"\n", modelID),
         if (is.null(annotationID)) "annotationID: No filter applied\n"
         else sprintf("annotationID: \"%s\"\n", annotationID),
         if (is.null(testID)) "testID: No filter applied\n"
         else sprintf("testID: \"%s\"\n", testID)
    )
  }

  enrichments <- splitTableIntoList(enrichmentsTable, "modelID")
  enrichments <- lapply(enrichments, function(x) splitTableIntoList(x, "annotationID"))
  enrichments <- lapply(enrichments,
                        function(x) lapply(x,
                                           function(y) splitTableIntoList(y, "testID")))
  if (!is.null(modelID)) enrichments <- enrichments[[1]]
  if (!is.null(annotationID)) enrichments <- enrichments[[1]]
  if (!is.null(testID)) enrichments <- enrichments[[1]]

  return(enrichments)
}

#' @rdname getEnrichments
#' @export
getEnrichments.character <- function(study, modelID = NULL, annotationID = NULL, testID = NULL, libraries = NULL, ...) {
  con <- connectDatabase(study, libraries = libraries)
  on.exit(disconnectDatabase(con))

  enrichments <- getEnrichments(con, modelID = modelID, annotationID = annotationID, testID = testID, ...)

  return(enrichments)
}

#' @export
getEnrichments.default <- function(study, modelID = NULL, annotationID = NULL, testID = NULL, ...) {
  stop(sprintf("No method for object of class \"%s\"", class(study)))
}

#' Get enrichments table from a study
#'
#' @export
getEnrichmentsTable <- function(study, modelID, annotationID, type = "nominal", ...) {
  stopifnot(type %in% c("nominal", "adjusted"))

  UseMethod("getEnrichmentsTable")
}

#' @rdname getEnrichmentsTable
#' @importFrom rlang ".data"
#' @export
getEnrichmentsTable.oaStudy <- function(study, modelID, annotationID, type = "nominal", ...) {
  enrichments <- getEnrichments(study, modelID = modelID, annotationID = annotationID)

  enrichmentsTable <- combineListIntoTable(enrichmentsTable, "testID")

  enrichmentsTableWide <- enrichmentsToWide(enrichmentsTable, type = type)

  return(enrichmentsTableWide)
}

#' @rdname getEnrichmentsTable
#' @importFrom dplyr "%>%"
#' @importFrom rlang "!!"
#' @importFrom rlang ".data"
#' @export
getEnrichmentsTable.SQLiteConnection <- function(study, modelID, annotationID, type = "nominal", ...) {

  enrichments <- getEnrichments(study, modelID = modelID, annotationID = annotationID)

  enrichmentsTable <- combineListIntoTable(enrichments, "testID")

  enrichmentsTableWide <- enrichmentsToWide(enrichmentsTable, type = type)

  return(enrichmentsTableWide)
}

#' @rdname getEnrichmentsTable
#' @export
getEnrichmentsTable.character <- function(study, modelID, annotationID, type = "nominal", libraries = NULL, ...) {
  con <- connectDatabase(study, libraries = libraries)
  on.exit(disconnectDatabase(con))

  enrichments <- getEnrichmentsTable(con, modelID = modelID,
                                     annotationID = annotationID, type = type,
                                     ...)

  return(enrichments)
}

#' @export
getEnrichmentsTable.default <- function(study, modelID, annotationID, type = "nominal", ...) {
  stop(sprintf("No method for object of class \"%s\"", class(study)))
}

#' Get enrichments network from a study
#'
#' @export
getEnrichmentsNetwork <- function(study, modelID, annotationID, ...) {
  UseMethod("getEnrichmentsNetwork")
}

#' @rdname getEnrichmentsNetwork
#' @importFrom dplyr "%>%"
#' @importFrom rlang "!!"
#' @importFrom rlang ".data"
#' @export
getEnrichmentsNetwork.SQLiteConnection <- function(study, modelID, annotationID, ...) {

  terms <- dplyr::tbl(study, "terms") %>%
    dplyr::filter(.data$annotationID == !! annotationID) %>%
    dplyr::group_by(.data$termID) %>%
    dplyr::tally(name = "geneSetSize") %>%
    dplyr::collect()

  enrichments <- getEnrichments(study, modelID = modelID, annotationID = annotationID)
  enrichmentsTable <- combineListIntoTable(enrichments, "testID")

  nodesLong <- enrichmentsTable %>%
    dplyr::left_join(terms, by = "termID") %>%
    dplyr::arrange(.data$testID)

  tests <- unique(nodesLong[["testID"]])

  if (nrow(nodesLong) == 0) {
    stop("Invalid filters.\n",
         if (is.null(modelID)) "modelID: No filter applied\n"
         else sprintf("modelID: \"%s\"\n", modelID),
         if (is.null(annotationID)) "annotationID: No filter applied\n"
         else sprintf("annotationID: \"%s\"\n", annotationID)
    )
  }

  nodes <- nodesLong %>%
    dplyr::group_by(.data$termID, .data$description, .data$geneSetSize) %>%
    dplyr::summarize(nominal = list(.data$nominal), adjusted = list(.data$adjusted)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(id = seq_len(dplyr::n())) %>%
    dplyr::select(.data$id, .data$termID, .data$description, .data$geneSetSize,
                  .data$nominal, .data$adjusted) %>%
    as.data.frame()

  links <- dplyr::tbl(study, "overlaps") %>%
    dplyr::filter(.data$annotationID ==  !! annotationID) %>%
    dplyr::select(-.data$annotationID) %>%
    dplyr::arrange(.data$term1, .data$term2) %>%
    dplyr::rename(source = .data$term1, target = .data$term2) %>%
    dplyr::collect() %>%
    dplyr::semi_join(nodes, by = c("source" = "termID")) %>%
    dplyr::semi_join(nodes, by = c("target" = "termID")) %>%
    dplyr::mutate(id = seq_len(dplyr::n())) %>%
    dplyr::select(.data$id, dplyr::everything()) %>%
    as.data.frame()

  # Use node IDs with links
  links[["source"]] <- match(links[["source"]], nodes[["termID"]])
  links[["target"]] <- match(links[["target"]], nodes[["termID"]])

  enrichmentsNetwork <- list(tests = tests, nodes = nodes, links = links)

  return(enrichmentsNetwork)
}

#' @rdname getEnrichmentsNetwork
#' @export
getEnrichmentsNetwork.character <- function(study, modelID, annotationID, libraries = NULL, ...) {
  con <- connectDatabase(study, libraries = libraries)
  on.exit(disconnectDatabase(con))

  enrichments <- getEnrichmentsNetwork(con, modelID = modelID, annotationID = annotationID, ...)

  return(enrichments)
}

#' @export
getEnrichmentsNetwork.default <- function(study, modelID, annotationID, ...) {
  if (inherits(study, "oaStudy")) {
    stop("The Enrichment Network is only available for study packages or databases")
  }

  stop(sprintf("No method for object of class \"%s\"", class(study)))
}

# Wrapper around base::split()
splitTableIntoList <- function(dataFrame, columnName) {

  splitVariable <- dataFrame[[columnName]]
  splitData <- dataFrame
  splitData[[columnName]] <- NULL

  result <- split(splitData, splitVariable)
  result <- lapply(result, function(x) {rownames(x) <- NULL; x})

  return(result)
}

combineListIntoTable <- function(listObj, newColumnName = "newColumnName") {
  stopifnot(
    is.list(listObj),
    length(listObj) > 0,
    is.character(newColumnName)
  )

  listNames <- names(listObj)
  newListObj <- listObj
  for (i in seq_along(listObj)) {
    newListObj[[i]][[newColumnName]] <- listNames[i]
  }

  names(newListObj) <- NULL # to avoid row names in output
  newTable <- do.call(rbind, newListObj)
  newColumnIndex <- ncol(newTable)
  newTable <- newTable[, c(newColumnIndex, seq_len(newColumnIndex - 1))]

  return(newTable)
}
