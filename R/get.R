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
#' @inheritParams shared-get
#'
#' @export
getModels <- function(study, modelID = NULL, ...) {
  UseMethod("getModels")
}

#' @rdname getModels
#' @export
getModels.oaStudy <- function(study, modelID = NULL, ...) {
  models <- study[["models"]]

  if (isEmpty(models)) {
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
#' @inheritParams listStudies
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
#' @inheritParams shared-get
#'
#' @export
getSamples <- function(study, modelID = NULL, ...) {
  UseMethod("getSamples")
}

#' @rdname getSamples
#' @export
getSamples.oaStudy <- function(study, modelID = NULL, ...) {
  samples <- study[["samples"]]

  if (isEmpty(samples)) {
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
    stop("No samples available for this study")
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
#' @inheritParams listStudies
#' @export
getSamples.character <- function(study, modelID = NULL, libraries = NULL, ...) {
  getElements(
    study,
    elements = "samples",
    filters = list(modelID = modelID),
    default = "default",
    libraries = libraries,
    ...
  )
}

#' @export
getSamples.default <- function(study, modelID = NULL, ...) {
  stop(sprintf("No method for object of class \"%s\"", class(study)))
}

#' Get features from a study
#'
#' @inheritParams shared-get
#'
#' @export
getFeatures <- function(study, modelID = NULL, ...) {
  UseMethod("getFeatures")
}

#' @rdname getFeatures
#' @export
getFeatures.oaStudy <- function(study, modelID = NULL, ...) {
  features <- study[["features"]]

  if (isEmpty(features)) {
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
    stop("No features available for this study")
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
#' @inheritParams listStudies
#' @export
getFeatures.character <- function(study, modelID = NULL, libraries = NULL, ...) {
  getElements(
    study,
    elements = "features",
    filters = list(modelID = modelID),
    default = "default",
    libraries = libraries,
    ...
  )
}

#' @export
getFeatures.default <- function(study, modelID = NULL, ...) {
  stop(sprintf("No method for object of class \"%s\"", class(study)))
}

#' Get assays from a study
#'
#' @inheritParams shared-get
#'
#' @export
getAssays <- function(study, modelID = NULL, ...) {
  UseMethod("getAssays")
}

#' @rdname getAssays
#' @export
getAssays.oaStudy <- function(study, modelID = NULL, ...) {
  assays <- study[["assays"]]

  if (isEmpty(assays)) {
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
         else sprintf("modelID: \"%s\"\n", modelID)
    )
  }

  assays <- splitTableIntoList(assaysTable, "modelID")
  assays <- lapply(assays, assaysToWide)
  if (!is.null(modelID)) assays <- assays[[1]]

  return(assays)
}

#' @rdname getAssays
#' @inheritParams listStudies
#' @export
getAssays.character <- function(study, modelID = NULL, libraries = NULL, ...) {
  getElements(
    study,
    elements = "assays",
    filters = list(modelID = modelID),
    default = "default",
    libraries = libraries,
    ...
  )
}

#' @export
getAssays.default <- function(study, modelID = NULL, ...) {
  stop(sprintf("No method for object of class \"%s\"", class(study)))
}

#' Get tests from a study
#'
#' @inheritParams shared-get
#'
#' @export
getTests <- function(study, modelID = NULL, ...) {
  UseMethod("getTests")
}

#' @rdname getTests
#' @export
getTests.oaStudy <- function(study, modelID = NULL, ...) {
  tests <- study[["tests"]]

  if (isEmpty(tests)) {
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
#' @inheritParams listStudies
#' @export
getTests.character <- function(study, modelID = NULL, libraries = NULL, ...) {
  getElements(
    study,
    elements = "tests",
    filters = list(modelID = modelID),
    default = "default",
    libraries = libraries,
    ...
  )
}

#' @export
getTests.default <- function(study, modelID = NULL, ...) {
  stop(sprintf("No method for object of class \"%s\"", class(study)))
}

#' Get annotations from a study
#'
#' @inheritParams shared-get
#'
#' @export
getAnnotations <- function(study, annotationID = NULL, ...) {
  UseMethod("getAnnotations")
}

#' @rdname getAnnotations
#' @export
getAnnotations.oaStudy <- function(study, annotationID = NULL, ...) {
  annotations <- study[["annotations"]]

  if (isEmpty(annotations)) {
    stop(sprintf("No annotations available for study \"%s\"", study[["name"]]))
  }

  if (is.null(annotationID)) return(annotations)

  stopifnot(is.character(annotationID), length(annotationID) == 1)
  annotationsAvailable <- names(annotations)
  if (annotationID %in% annotationsAvailable) {
    annotationsEntry <- annotations[[annotationID]]
  } else {
    stop(sprintf("The annotation \"%s\" is not available ", annotationID))
  }

  return(annotationsEntry)
}

#' @rdname getAnnotations
#' @importFrom rlang "!!"
#' @importFrom rlang ".data"
#' @export
getAnnotations.SQLiteConnection <- function(study, annotationID = NULL, ...) {

  annotationsTable <- dplyr::tbl(study, "annotations")
  if (!is.null(annotationID)) {
    stopifnot(is.character(annotationID), length(annotationID) == 1)
    annotationsTable <- dplyr::filter(annotationsTable, .data$annotationID == !! annotationID)
  }

  annotationsTable <- dplyr::collect(annotationsTable)
  annotationsTable <- as.data.frame(annotationsTable)

  if (nrow(annotationsTable) == 0) {
    stop("Invalid filters.\n",
         if (is.null(annotationID)) "annotationID: No filter applied\n"
         else sprintf("annotationID: \"%s\"\n", annotationID),
    )
  }

  annotations <- splitTableIntoList(annotationsTable, "annotationID")
  annotations <- lapply(annotations, as.list)

  terms <- dplyr::tbl(study, "terms")
  for (i in seq_along(annotations)) {
    tmpAnnotationID <- names(annotations)[i]
    annotationTerms <- dplyr::filter(terms, .data$annotationID == !! tmpAnnotationID)
    annotationTerms <- dplyr::collect(annotationTerms)
    annotationTerms[["annotationID"]] <- NULL
    annotationTermsList <- splitTableIntoList(annotationTerms, "termID")
    annotationTermsList <- lapply(annotationTermsList, unlist, use.names = FALSE)
    annotations[[i]][["terms"]] <- annotationTermsList
    annotations[[i]] <- annotations[[i]][c("terms", "description", "featureID")]
  }

  if (!is.null(annotationID)) annotations <- annotations[[1]]

  return(annotations)
}

#' @rdname getAnnotations
#' @inheritParams listStudies
#' @export
getAnnotations.character <- function(study, annotationID = NULL, libraries = NULL, ...) {

  oaDirectory <- system.file("OmicAnalyzer/",
                             package = paste0("OAstudy", study),
                             lib.loc = libraries)
  if (oaDirectory == "") {
    stop(sprintf("The study \"%s\" is not installed\n", study),
         "Did you run installStudy()?\n")
  }

  annotationsDirectory <- file.path(oaDirectory, "annotations")
  annotationsFiles <- list.files(annotationsDirectory, full.names = TRUE)

  if (isEmpty(annotationsFiles)) {
    stop("No annotations available for this study")
  }

  annotationsNames <- basename(annotationsFiles)
  annotationsNames <- sub("\\.json$", "", annotationsNames)
  names(annotationsFiles) <- annotationsNames

  if (is.null(annotationID)) {
    annotations <- lapply(annotationsFiles,
                          function(x) jsonlite::read_json(x))
    names(annotations) <- annotationsNames
    return(annotations)
  }

  stopifnot(is.character(annotationID), length(annotationID) == 1)
  if (annotationID %in% annotationsNames) {
    annotations <- jsonlite::read_json(annotationsFiles[annotationID])
    return(annotations)
  }

  stop(sprintf("The annotationID \"%s\" is not available for this study", annotationID))
}

#' @export
getAnnotations.default <- function(study, annotationID = NULL, ...) {
  stop(sprintf("No method for object of class \"%s\"", class(study)))
}

#' Get results from a study
#'
#' @inheritParams shared-get
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

  if (isEmpty(results)) {
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
  results <- dfrapply(results, removeNaColumns)
  if (!is.null(modelID)) results <- results[[1]]
  if (!is.null(testID)) results <- results[[1]]

  return(results)
}

#' @rdname getResults
#' @inheritParams listStudies
#' @export
getResults.character <- function(study, modelID = NULL, testID = NULL, libraries = NULL, ...) {
  getElements(
    study,
    elements = "results",
    filters = list(modelID = modelID, testID = testID),
    libraries = libraries,
    ...
  )
}

#' @export
getResults.default <- function(study, modelID = NULL, testID = NULL, ...) {
  stop(sprintf("No method for object of class \"%s\"", class(study)))
}

#' Get results table from a study
#'
#' @inheritParams shared-get
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
#' @inheritParams listStudies
#' @export
getResultsTable.character <- function(study, modelID, testID, libraries = NULL, ...) {
  results <- getResults(study, modelID, testID, ...)
  features <- getFeatures(study, modelID, ...)

  resultsTable <- merge(features, results, by = 1,
                        all.x = FALSE, all.y = TRUE, sort = FALSE)

  return(resultsTable)
}

#' @export
getResultsTable.default <- function(study, modelID, testID, ...) {
  stop(sprintf("No method for object of class \"%s\"", class(study)))
}

#' Get enrichments from a study
#'
#' @inheritParams shared-get
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

  if (isEmpty(enrichments)) {
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
  enrichments <- dfrapply(enrichments, splitTableIntoList, columnName = "testID")
  if (!is.null(modelID)) enrichments <- enrichments[[1]]
  if (!is.null(annotationID)) enrichments <- enrichments[[1]]
  if (!is.null(testID)) enrichments <- enrichments[[1]]

  return(enrichments)
}

#' @rdname getEnrichments
#' @inheritParams listStudies
#' @export
getEnrichments.character <- function(study, modelID = NULL, annotationID = NULL, testID = NULL, libraries = NULL, ...) {
  getElements(
    study,
    elements = "enrichments",
    filters = list(modelID = modelID, annotationID = annotationID, testID = testID),
    libraries = libraries,
    ...
  )
}

#' @export
getEnrichments.default <- function(study, modelID = NULL, annotationID = NULL, testID = NULL, ...) {
  stop(sprintf("No method for object of class \"%s\"", class(study)))
}

#' Get enrichments table from a study
#'
#' @inheritParams shared-get
#' @inheritParams shared-upset
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

  enrichmentsTable <- combineListIntoTable(enrichments, "testID")

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
#' @inheritParams listStudies
#' @export
getEnrichmentsTable.character <- function(study, modelID, annotationID, type = "nominal", libraries = NULL, ...) {
  enrichments <- getEnrichments(study, modelID = modelID, annotationID = annotationID)

  enrichmentsTable <- combineListIntoTable(enrichments, "testID")

  enrichmentsTableWide <- enrichmentsToWide(enrichmentsTable, type = type)

  return(enrichmentsTableWide)
}

#' @export
getEnrichmentsTable.default <- function(study, modelID, annotationID, type = "nominal", ...) {
  stop(sprintf("No method for object of class \"%s\"", class(study)))
}

#' Get enrichments network from a study
#'
#' @inheritParams shared-get
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
#' @inheritParams listStudies
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

#' Get metaFeatures from a study
#'
#' @inheritParams shared-get
#'
#' @export
getMetaFeatures <- function(study, modelID = NULL, ...) {
  UseMethod("getMetaFeatures")
}

#' @rdname getMetaFeatures
#' @export
getMetaFeatures.oaStudy <- function(study, modelID = NULL, ...) {
  metaFeatures <- study[["metaFeatures"]]

  if (isEmpty(metaFeatures)) {
    stop(sprintf("No metaFeatures available for study \"%s\"", study[["name"]]))
  }

  if (is.null(modelID)) return(metaFeatures)

  stopifnot(is.character(modelID), length(modelID) == 1)
  metaFeaturesModels <- names(metaFeatures)
  if (modelID %in% metaFeaturesModels) return(metaFeatures[[modelID]])
  if ("default" %in% metaFeaturesModels) {
    message(sprintf("Returning \"default\" metaFeatures for model \"%s\"", modelID))
    return(metaFeatures[["default"]])
  }

  stop(sprintf("No metaFeatures available for model \"%s\"", modelID))
}

#' @rdname getMetaFeatures
#' @export
getMetaFeatures.SQLiteConnection <- function(study, modelID = NULL, ...) {

  dbTables <- DBI::dbListTables(study)
  dbMetaFeatures <- grep("^metaFeatures-", dbTables, value = TRUE)
  metaFeaturesModels <- sub("^metaFeatures-", "", dbMetaFeatures)

  if (isEmpty(dbMetaFeatures)) {
    stop("No metaFeatures available for this study")
  }

  if (is.null(modelID)) {
    metaFeatures <- lapply(dbMetaFeatures, function(x) DBI::dbReadTable(study, x))
    names(metaFeatures) <- metaFeaturesModels
    return(metaFeatures)
  }

  stopifnot(is.character(modelID), length(modelID) == 1)
  if (modelID %in% metaFeaturesModels) {
    tableName <- paste("metaFeatures", modelID, sep = "-")
    metaFeatures <- DBI::dbReadTable(study, tableName)
    return(metaFeatures)
  }
  if ("default" %in% metaFeaturesModels) {
    message(sprintf("Returning \"default\" metaFeatures for model \"%s\"", modelID))
    tableName <- paste("metaFeatures", "default", sep = "-")
    metaFeatures <- DBI::dbReadTable(study, tableName)
    return(metaFeatures)
  }

  stop(sprintf("No metaFeatures available for model \"%s\"", modelID))
}

#' @rdname getMetaFeatures
#' @inheritParams listStudies
#' @export
getMetaFeatures.character <- function(study, modelID = NULL, libraries = NULL, ...) {
  getElements(
    study,
    elements = "metaFeatures",
    filters = list(modelID = modelID),
    default = "default",
    libraries = libraries,
    ...
  )
}

#' @export
getMetaFeatures.default <- function(study, modelID = NULL, ...) {
  stop(sprintf("No method for object of class \"%s\"", class(study)))
}

#' Get plots from a study
#'
#' @inheritParams shared-get
#'
#' @export
getPlots <- function(study, modelID = NULL, ...) {
  UseMethod("getPlots")
}

#' @rdname getPlots
#' @export
getPlots.oaStudy <- function(study, modelID = NULL, ...) {
  plots <- study[["plots"]]

  if (isEmpty(plots)) {
    stop(sprintf("No plots available for study \"%s\"", study[["name"]]))
  }

  if (is.null(modelID)) return(plots)

  stopifnot(is.character(modelID), length(modelID) == 1)
  plotsModels <- names(plots)
  if (modelID %in% plotsModels) {
    return(plots[[modelID]])
  }
  if ("default" %in% plotsModels) {
    message(sprintf("Returning \"default\" plots for model \"%s\"", modelID))
    return(plots[["default"]])
  }

  stop(sprintf("No plots available for model \"%s\"", modelID))
}

#' @rdname getPlots
#' @export
getPlots.SQLiteConnection <- function(study, modelID = NULL, ...) {

  dbTables <- DBI::dbListTables(study)
  dbPlots <- "plots"

  if (!dbPlots %in% dbTables) {
    stop("No plots available for this study")
  }

  plots <- DBI::dbReadTable(study, dbPlots)
  plots <- splitTableIntoList(plots, "modelID")
  plots <- lapply(plots, splitTableIntoList, "plotID")
  plots <- lapply(plots, function(x) lapply(x, as.list))
  plots <- lapply(plots, function(x) lapply(x, function(x) {
    x[["packages"]] <- strsplit(x[["packages"]], split = ";")[[1]]
    if (length(x[["packages"]]) == 0) {
      x[["packages"]] <- NULL
    }
    x
  }))

  if (is.null(modelID)) return(plots)

  stopifnot(is.character(modelID), length(modelID) == 1)
  plotsModels <- names(plots)
  if (modelID %in% plotsModels) {
    return(plots[[modelID]])
  }
  if ("default" %in% plotsModels) {
    message(sprintf("Returning \"default\" plots for model \"%s\"", modelID))
    return(plots[["default"]])
  }

  stop(sprintf("No plots available for model \"%s\"", modelID))
}

#' @rdname getPlots
#' @inheritParams listStudies
#' @export
getPlots.character <- function(study, modelID = NULL, libraries = NULL, ...) {
  con <- connectDatabase(study, libraries = libraries)
  on.exit(disconnectDatabase(con))

  plots <- getPlots(con, modelID = modelID, ...)

  return(plots)
}

#' @export
getPlots.default <- function(study, modelID = NULL, ...) {
  stop(sprintf("No method for object of class \"%s\"", class(study)))
}

#' Get barcodes from a study
#'
#' @inheritParams shared-get
#'
#' @export
getBarcodes <- function(study, modelID = NULL, ...) {
  UseMethod("getBarcodes")
}

#' @rdname getBarcodes
#' @export
getBarcodes.oaStudy <- function(study, modelID = NULL, ...) {
  barcodes <- study[["barcodes"]]

  if (isEmpty(barcodes)) {
    stop(sprintf("No barcodes available for study \"%s\"", study[["name"]]))
  }

  if (is.null(modelID)) return(barcodes)

  stopifnot(is.character(modelID), length(modelID) == 1)
  barcodesModels <- names(barcodes)
  if (modelID %in% barcodesModels) return(barcodes[[modelID]])
  if ("default" %in% barcodesModels) {
    message(sprintf("Returning \"default\" barcodes for model \"%s\"", modelID))
    return(barcodes[["default"]])
  }

  stop(sprintf("No barcodes available for model \"%s\"", modelID))
}

#' @rdname getBarcodes
#' @export
getBarcodes.SQLiteConnection <- function(study, modelID = NULL, ...) {

  dbTables <- DBI::dbListTables(study)
  dbBarcodes <- "barcodes"

  if (!dbBarcodes %in% dbTables) {
    stop("No barcodes available for this study")
  }

  barcodes <- DBI::dbReadTable(study, dbBarcodes)
  barcodes <- splitTableIntoList(barcodes, "modelID")
  barcodes <- lapply(barcodes, as.list)

  if (is.null(modelID)) return(barcodes)

  stopifnot(is.character(modelID), length(modelID) == 1)
  barcodesModels <- names(barcodes)
  if (modelID %in% barcodesModels) {
    return(barcodes[[modelID]])
  }
  if ("default" %in% barcodesModels) {
    message(sprintf("Returning \"default\" barcodes for model \"%s\"", modelID))
    return(barcodes[["default"]])
  }

  stop(sprintf("No barcodes available for model \"%s\"", modelID))
}

#' @rdname getBarcodes
#' @inheritParams listStudies
#' @export
getBarcodes.character <- function(study, modelID = NULL, libraries = NULL, ...) {
  con <- connectDatabase(study, libraries = libraries)
  on.exit(disconnectDatabase(con))

  barcodes <- getBarcodes(con, modelID = modelID, ...)

  return(barcodes)
}

#' @export
getBarcodes.default <- function(study, modelID = NULL, ...) {
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

getElements <- function(study, elements, filters = list(), default = NULL, libraries = NULL, ...) {

  oaDirectory <- system.file("OmicAnalyzer/",
                             package = paste0("OAstudy", study),
                             lib.loc = libraries)
  if (oaDirectory == "") {
    stop(sprintf("The study \"%s\" is not installed\n", study),
         "Did you run installStudy()?\n")
  }

  elementsDirectory <- file.path(oaDirectory, elements)
  if (!dir.exists(elementsDirectory)) {
    stop(sprintf("Study \"%s\" does not have an elements named \"%s\"",
                 study, elements), call. = FALSE)
  }
  elementsFiles <- getFiles(elementsDirectory)

  if (isEmpty(elementsFiles)) {
    stop("No results available for this study")
  }

  filters <- Filter(function(x) !is.null(x), filters)

  for (i in seq_along(filters)) {
    filterName <- names(filters)[i]
    filterValue <- filters[[i]]
    namesCurrent <- names(elementsFiles)
    if (filterValue %in% namesCurrent) {
      elementsFiles <- elementsFiles[[filterValue]]
    } else if (!is.null(default) && default %in% namesCurrent) {
      message(sprintf("Returning \"%s\" %s for %s \"%s\"",
                      default, elements, filterName, filterValue))
      elementsFiles <- elementsFiles[[default]]
    } else {
      stop(sprintf("No %s available for %s \"%s\"",
                   elements, filterName, filterValue))
    }
  }

  if (is.list(elementsFiles)) {
    object <- rapply(elementsFiles, function(x)
      data.table::fread(file = x, data.table = FALSE),
      how = "replace")
  } else {
    object <- data.table::fread(file = elementsFiles, data.table = FALSE)
  }

  return(object)
}

getFiles <- function(path) {
  if (dir.exists(path)) {
    contents <- list.files(path, full.names = TRUE)
    contentsNames <- basename(contents)
    contentsNames <- tools::file_path_sans_ext(contentsNames)
    stats::setNames(lapply(contents, getFiles), contentsNames)
  } else {
    path
  }
}
