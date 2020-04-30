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
#' @export
getModels.SQLiteConnection <- function(study, modelID = NULL, ...) {

  df_models <- dplyr::tbl(study, "models")
  if (!is.null(modelID)) {
    stopifnot(is.character(modelID), length(modelID) == 1)
    df_models <- dplyr::filter(df_models, modelID == !! modelID)
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

#' Get tests from a study
#'
#' @export
getTests <- function(study, modelID = NULL, testID = NULL, ...) {
  if (!is.null(modelID) && !is.null(testID)) {
    stop("Can only filter by modelID or testID, not both")
  }

  UseMethod("getTests")
}

#' @rdname getTests
#' @export
getTests.oaStudy <- function(study, modelID = NULL, testID = NULL, ...) {
  tests <- study[["tests"]]
  if (is.null(tests)) {
    stop(sprintf("No tests available for study \"%s\"", study[["name"]]))
  }

  if (!is.null(modelID)) {
    stopifnot(is.character(modelID), length(modelID) == 1)
    models <- study[["models"]]
    if (!modelID %in% names(models)) {
      stop(sprintf("Study \"%s\" has no model \"%s\"", study[["name"]], modelID))
    }
    # Get the tests per model from results
    results <- study[["results"]]
    if (is.null(results)) {
      stop(sprintf("Study \"%s\" has no results\n", study[["name"]]),
           "Unable to determine tests per model without them\n",
           "Use addResults() to add results to the study")
    }
    tests_per_model <- names(study[["results"]][[modelID]])
    tests <- tests[tests_per_model]
  }

  if (!is.null(testID)) {
    stopifnot(is.character(testID), length(testID) == 1)
    if (!testID %in% names(tests)) {
      stop(sprintf("Study \"%s\" has no test \"%s\"", study[["name"]], testID))
    }
    tests <- tests[testID]
  }

  return(tests)
}

#' @rdname getTests
#' @importFrom rlang "!!"
#' @export
getTests.SQLiteConnection <- function(study, modelID = NULL, testID = NULL, ...) {

  df_tests <- dplyr::tbl(study, "tests")
  if (!is.null(modelID)) {
    stopifnot(is.character(modelID), length(modelID) == 1)
    df_results <- dplyr::tbl(study, "results") %>%
      dplyr::filter(modelID == !! modelID)
    df_tests <- df_tests %>%
      dplyr::semi_join(df_results, by = "testID")
  }
  if (!is.null(testID)) {
    stopifnot(is.character(testID), length(testID) == 1)
    df_tests <- dplyr::filter(df_tests, testID == !! testID)
  }
  df_tests <- dplyr::collect(df_tests)

  if (nrow(df_tests) == 0) {
    if (!is.null(modelID)) {
      stop(sprintf("Invalid modelID: \"%s\"", modelID))
    } else {
      stop(sprintf("Invalid testID: \"%s\"", testID))
    }
  }

  tests <- as.list(df_tests[["description"]])
  names(tests) <- df_tests[["testID"]]
  return(tests)
}

#' @rdname getTests
#' @export
getTests.character <- function(study, modelID = NULL, testID = NULL, libraries = NULL, ...) {
  con <- connectDatabase(study, libraries = libraries)
  on.exit(disconnectDatabase(con))

  tests <- getTests(con, modelID = modelID, testID = testID, ...)

  return(tests)
}

#' @export
getTests.default <- function(study, modelID = NULL, testID = NULL, ...) {
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

  if (!is.null(modelID)) {
    stopifnot(is.character(modelID), length(modelID) == 1)
    if (!modelID %in% names(results)) {
      stop(sprintf("No results available for model \"%s\"", modelID))
    }
    results <- results[[modelID]]
  }

  if (!is.null(testID)) {
    stopifnot(is.character(testID), length(testID) == 1)
    if (!testID %in% names(results)) {
      stop(sprintf("No results available for test \"%s\" for model \"%s\"",
                   testID, modelID))
    }
    results <- results[[testID]]
  }

  return(results)
}

#' @rdname getResults
#' @importFrom rlang "!!"
#' @export
getResults.SQLiteConnection <- function(study, modelID = NULL, testID = NULL, ...) {

  df_results <- dplyr::tbl(study, "results")
  if (!is.null(modelID)) {
    stopifnot(is.character(modelID), length(modelID) == 1)
    df_results <- dplyr::filter(df_results, modelID == !! modelID)
  }
  if (!is.null(testID)) {
    stopifnot(is.character(testID), length(testID) == 1)
    df_results <- dplyr::filter(df_results, testID == !! testID)
  }
  df_results <- dplyr::collect(df_results) %>%
    as.data.frame()

  if (nrow(df_results) == 0) {
    stop("Invalid filters.\n",
         if (is.null(modelID)) "modelID: No filter applied\n"
         else sprintf("modelID: \"%s\"\n", modelID),
         if (is.null(testID)) "testID: No filter applied\n"
         else sprintf("testID: \"%s\"\n", testID)
    )
  }

  results <- splitTableIntoList(df_results, "modelID")
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
  results <- study[["results"]]

  if (is.null(results)) {
    stop(sprintf("No results available for study \"%s\"", study[["name"]]))
  }

  stopifnot(is.character(modelID), length(modelID) == 1)
  if (!modelID %in% names(results)) {
    stop(sprintf("No results available for model \"%s\"", modelID))
  }
  results <- results[[modelID]]

  stopifnot(is.character(testID), length(testID) == 1)
  if (!testID %in% names(results)) {
    stop(sprintf("No results available for test \"%s\" for model \"%s\"",
                 testID, modelID))
  }
  results <- results[[testID]]

  features <- study[["features"]] # to do: replace with getFeatures()
  resultsTable <- merge(results, features, by = study[["featureID"]],
                        all.x = TRUE, all.y = FALSE)
  colOrder <- c(study[["featureID"]],
                setdiff(colnames(features), study[["featureID"]]),
                setdiff(colnames(results), study[["featureID"]]))
  resultsTable <- resultsTable[, colOrder]

  return(resultsTable)
}

#' @rdname getResultsTable
#' @importFrom rlang "!!"
#' @export
getResultsTable.SQLiteConnection <- function(study, modelID , testID, ...) {

  df_results <- dplyr::tbl(study, "results")
  stopifnot(is.character(modelID), length(modelID) == 1)
  df_results <- dplyr::filter(df_results, modelID == !! modelID)

  stopifnot(is.character(testID), length(testID) == 1)
  df_results <- dplyr::filter(df_results, testID == !! testID)

  df_results <- dplyr::collect(df_results) %>%
    as.data.frame()

  if (nrow(df_results) == 0) {
    stop("Invalid filters.\n",
         if (is.null(modelID)) "modelID: No filter applied\n"
         else sprintf("modelID: \"%s\"\n", modelID),
         if (is.null(testID)) "testID: No filter applied\n"
         else sprintf("testID: \"%s\"\n", testID)
    )
  }

  features <- dplyr::tbl(study, "features") %>% dplyr::collect()
  featureCols <- colnames(features)
  featureID <- intersect(featureCols, colnames(df_results))
  stopifnot(length(featureID) == 1)

  resultsTable <- df_results %>%
    dplyr::select(-modelID, -testID) %>%
    dplyr::left_join(features, by = featureID) %>%
    dplyr::select(!! featureID, !! featureCols, dplyr::everything())

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
getEnrichments <- function(study, modelID = NULL, testID = NULL, annotationID = NULL, ...) {
  if (is.null(modelID) && !is.null(testID)) {
    stop("Must specify a model in order to specify a test")
  }
  if (is.null(testID) && !is.null(annotationID)) {
    stop("Must specify a test in order to specify an annotation")
  }

  UseMethod("getEnrichments")
}

#' @rdname getEnrichments
#' @export
getEnrichments.oaStudy <- function(study, modelID = NULL, testID = NULL, annotationID = NULL, ...) {
  enrichments <- study[["enrichments"]]

  if (is.null(enrichments)) {
    stop(sprintf("No enrichments available for study \"%s\"", study[["name"]]))
  }

  if (!is.null(modelID)) {
    stopifnot(is.character(modelID), length(modelID) == 1)
    if (!modelID %in% names(enrichments)) {
      stop(sprintf("No enrichments available for model \"%s\"", modelID))
    }
    enrichments <- enrichments[[modelID]]
  }

  if (!is.null(testID)) {
    stopifnot(is.character(testID), length(testID) == 1)
    if (!testID %in% names(enrichments)) {
      stop(sprintf("No enrichments available for test \"%s\"", testID))
    }
    enrichments <- enrichments[[testID]]
  }

  if (!is.null(annotationID)) {
    stopifnot(is.character(annotationID), length(annotationID) == 1)
    if (!annotationID %in% names(enrichments)) {
      stop(sprintf("No enrichments available for annotation \"%s\" for model \"%s\"",
                   annotationID, modelID))
    }
    enrichments <- enrichments[[annotationID]]
  }

  return(enrichments)
}

#' @rdname getEnrichments
#' @importFrom rlang "!!"
#' @export
getEnrichments.SQLiteConnection <- function(study, modelID = NULL, testID = NULL, annotationID = NULL, ...) {

  df_enrichments <- dplyr::tbl(study, "enrichments")
  if (!is.null(modelID)) {
    stopifnot(is.character(modelID), length(modelID) == 1)
    df_enrichments <- dplyr::filter(df_enrichments, modelID == !! modelID)
  }
  if (!is.null(testID)) {
    stopifnot(is.character(testID), length(testID) == 1)
    df_enrichments <- dplyr::filter(df_enrichments, testID == !! testID)
  }
  if (!is.null(annotationID)) {
    stopifnot(is.character(annotationID), length(annotationID) == 1)
    df_enrichments <- dplyr::filter(df_enrichments, annotationID == !! annotationID)
  }
  df_enrichments <- dplyr::collect(df_enrichments) %>%
    as.data.frame()

  if (nrow(df_enrichments) == 0) {
    stop("Invalid filters.\n",
         if (is.null(modelID)) "modelID: No filter applied\n"
         else sprintf("modelID: \"%s\"\n", modelID),
         if (is.null(testID)) "testID: No filter applied\n"
         else sprintf("testID: \"%s\"\n", testID),
         if (is.null(annotationID)) "annotationID: No filter applied\n"
         else sprintf("annotationID: \"%s\"\n", annotationID)
    )
  }

  enrichments <- splitTableIntoList(df_enrichments, "modelID")
  enrichments <- lapply(enrichments, function(x) splitTableIntoList(x, "testID"))
  enrichments <- lapply(enrichments,
                        function(x) lapply(x,
                                           function(y) splitTableIntoList(y, "annotationID")))
  if (!is.null(modelID)) enrichments <- enrichments[[1]]
  if (!is.null(testID)) enrichments <- enrichments[[1]]
  if (!is.null(annotationID)) enrichments <- enrichments[[1]]

  return(enrichments)
}

#' @rdname getEnrichments
#' @export
getEnrichments.character <- function(study, modelID = NULL, testID = NULL, annotationID = NULL, libraries = NULL, ...) {
  con <- connectDatabase(study, libraries = libraries)
  on.exit(disconnectDatabase(con))

  enrichments <- getEnrichments(con, modelID = modelID, testID = testID, annotationID = annotationID, ...)

  return(enrichments)
}

#' @export
getEnrichments.default <- function(study, modelID = NULL, testID = NULL, annotationID = NULL, ...) {
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
#' @export
getEnrichmentsTable.oaStudy <- function(study, modelID, annotationID, type = "nominal", ...) {
  enrichments <- study[["enrichments"]]

  if (is.null(enrichments)) {
    stop(sprintf("No enrichments available for study \"%s\"", study[["name"]]))
  }

  stopifnot(is.character(modelID), length(modelID) == 1)
  if (!modelID %in% names(enrichments)) {
    stop(sprintf("No enrichments available for model \"%s\"", modelID))
  }
  enrichments <- enrichments[[modelID]]

  enrichmentsTable <- lapply(enrichments, combineListIntoTable, "annotationID")
  enrichmentsTable <- combineListIntoTable(enrichmentsTable, "testID")


  stopifnot(is.character(annotationID), length(annotationID) == 1)
  if (!annotationID %in% enrichmentsTable[["annotationID"]]) {
    stop(sprintf("No enrichments available for annotation \"%s\" for model \"%s\"",
                 annotationID, modelID))
  }
  enrichmentsTable <- enrichmentsTable[enrichmentsTable[["annotationID"]] == annotationID, ]
  enrichmentsTable[["annotationID"]] <- NULL
  if (type == "nominal") {
    enrichmentsTable[["adjusted"]] <- NULL
    enrichmentsTable <- tidyr::pivot_wider(
      enrichmentsTable,
      names_from = testID,
      values_from = nominal
    )
  } else {
    enrichmentsTable[["nominal"]] <- NULL
    enrichmentsTable <- tidyr::pivot_wider(
      enrichmentsTable,
      names_from = testID,
      values_from = adjusted
    )
  }

  enrichmentsTable <- as.data.frame(enrichmentsTable)

  return(enrichmentsTable)
}

#' @rdname getEnrichmentsTable
#' @importFrom rlang "!!"
#' @export
getEnrichmentsTable.SQLiteConnection <- function(study, modelID, annotationID, type = "nominal", ...) {

  df_enrichments <- dplyr::tbl(study, "enrichments")
  stopifnot(is.character(modelID), length(modelID) == 1)
  df_enrichments <- dplyr::filter(df_enrichments, modelID == !! modelID)

  stopifnot(is.character(annotationID), length(annotationID) == 1)
  df_enrichments <- dplyr::filter(df_enrichments, annotationID == !! annotationID)

  df_enrichments <- dplyr::collect(df_enrichments) %>%
    as.data.frame()

  if (nrow(df_enrichments) == 0) {
    stop("Invalid filters.\n",
         if (is.null(modelID)) "modelID: No filter applied\n"
         else sprintf("modelID: \"%s\"\n", modelID),
         if (is.null(annotationID)) "annotationID: No filter applied\n"
         else sprintf("annotationID: \"%s\"\n", annotationID)
    )
  }

  enrichmentsTable <- df_enrichments
  enrichmentsTable[["annotationID"]] <- NULL
  enrichmentsTable[["modelID"]] <- NULL
  if (type == "nominal") {
    enrichmentsTable[["adjusted"]] <- NULL
    enrichmentsTable <- tidyr::pivot_wider(
      enrichmentsTable,
      names_from = testID,
      values_from = nominal
    )
  } else {
    enrichmentsTable[["nominal"]] <- NULL
    enrichmentsTable <- tidyr::pivot_wider(
      enrichmentsTable,
      names_from = testID,
      values_from = adjusted
    )
  }

  enrichmentsTable <- as.data.frame(enrichmentsTable)

  return(enrichmentsTable)
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
#' @importFrom rlang "!!"
#' @export
getEnrichmentsNetwork.SQLiteConnection <- function(study, modelID, annotationID, ...) {

  terms <- dplyr::tbl(study, "terms") %>%
    dplyr::filter(annotationID == !! annotationID) %>%
    dplyr::group_by(termID) %>%
    dplyr::tally(name = "geneSetSize")

  df_enrichments <- dplyr::tbl(study, "enrichments")
  stopifnot(is.character(modelID), length(modelID) == 1)
  df_enrichments <- dplyr::filter(df_enrichments, modelID == !! modelID)

  stopifnot(is.character(annotationID), length(annotationID) == 1)
  df_enrichments <- dplyr::filter(df_enrichments, annotationID == !! annotationID)

  nodes_long <- df_enrichments %>%
    dplyr::select(-modelID, -annotationID) %>%
    dplyr::left_join(terms, by = "termID") %>%
    dplyr::arrange(testID) %>%
    dplyr::collect()

  tests <- unique(nodes_long[["testID"]])

  if (nrow(nodes_long) == 0) {
    stop("Invalid filters.\n",
         if (is.null(modelID)) "modelID: No filter applied\n"
         else sprintf("modelID: \"%s\"\n", modelID),
         if (is.null(annotationID)) "annotationID: No filter applied\n"
         else sprintf("annotationID: \"%s\"\n", annotationID)
    )
  }

  nodes <- nodes_long %>%
    dplyr::group_by(termID, description) %>%
    dplyr::summarize(nominal = list(nominal), adjusted = list(adjusted)) %>%
    dplyr::mutate(id = seq_len(dplyr::n())) %>%
    dplyr::select(id, termID, description, nominal, adjusted) %>%
    as.data.frame()

  links <- dplyr::tbl(study, "overlaps") %>%
    dplyr::filter(annotationID ==  !! annotationID) %>%
    dplyr::select(-annotationID) %>%
    dplyr::arrange(term1, term2) %>%
    dplyr::rename(source = term1, target = term2) %>%
    dplyr::collect() %>%
    dplyr::semi_join(nodes, by = c("source" = "termID")) %>%
    dplyr::semi_join(nodes, by = c("target" = "termID")) %>%
    dplyr::mutate(id = seq_len(dplyr::n())) %>%
    dplyr::select(id, dplyr::everything()) %>%
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
