#' Export a study
#'
#' @param study An OmicAnalyzer study
#' @param type Export to a RDS file ("rds"), SQLite database ("sqlite"), or an
#'   R package ("package")
#' @param path Optional file path to save the object
#'
#' @export
exportStudy <- function(study, type = c("rds", "sqlite", "package"), path = NULL) {
  checkStudy(study)

  type <- match.arg(type)

  if (type == "rds") {
    message(sprintf("Exporting study \"%s\" to an RDS file", study[["name"]]))
    filename <- paste0(study[["name"]], ".rds")
    if (!is.null(path)) filename <- file.path(path, filename)
    saveRDS(object = study, file = filename)
    message(sprintf("Exported study to %s", filename))
    return(invisible(filename))
  } else if (type == "sqlite") {
    message(sprintf("Exporting study \"%s\" to an SQLite database", study[["name"]]))
    filename <- paste0(study[["name"]], ".sqlite")
    if (!is.null(path)) filename <- file.path(path, filename)
    createDatabase(study, filename)
    message(sprintf("Exported study to %s", filename))
    return(invisible(filename))
  } else if (type == "package") {
    message(sprintf("Exporting study \"%s\" to an R package", study[["name"]]))
    directoryname <- paste0("OAstudy", study[["name"]])
    if (!is.null(path)) directoryname <- file.path(path, directoryname)
    createPackage(study, directoryname)
    message(sprintf("Exported study to %s", directoryname))
    return(invisible(directoryname))
  }
}

#' @importFrom dplyr "%>%"
#' @importFrom rlang "!!"
#' @importFrom rlang ".data"
createDatabase <- function(study, filename) {

  tmpdb <- tempfile(fileext = ".sqlite")
  con <- DBI::dbConnect(RSQLite::SQLite(), tmpdb)

  # samples --------------------------------------------------------------------

  message("* Adding samples")
  for (i in seq_along(study[["samples"]])) {
    tableName <- paste("samples", names(study[["samples"]])[i], sep = "-")
    DBI::dbWriteTable(con, tableName, study[["samples"]][[i]])
  }

  # features -------------------------------------------------------------------

  message("* Adding features")
  for (i in seq_along(study[["features"]])) {
    tableName <- paste("features", names(study[["features"]])[i], sep = "-")
    DBI::dbWriteTable(con, tableName, study[["features"]][[i]])
  }

  # models ---------------------------------------------------------------------

  message("* Adding models")
  models <- data.frame(modelID = names(study[["models"]]),
                       description = unlist(study[["models"]]),
                       stringsAsFactors = FALSE)
  DBI::dbWriteTable(con, "models", models,
                    field.types = c("modelID" = "varchar(100) PRIMARY KEY",
                                    "description" = "varchar(200)"))

  # assays ---------------------------------------------------------------------

  message("* Adding assays")
  assays_long <- vector(mode = "list", length = length(study[["assays"]]))
  for (i in seq_along(study[["assays"]])) {
    assays_long[[i]] <- study[["assays"]][[i]] %>%
      dplyr::mutate(featureID = rownames(.)) %>%
      tidyr::pivot_longer(cols = -.data$featureID,
                          names_to = "sampleID",
                          values_to = "quantification") %>%
      dplyr::mutate(modelID = names(study[["assays"]])[i]) %>%
      dplyr::select(.data$featureID, .data$sampleID, .data$modelID, .data$quantification)
  }
  assays_final <- dplyr::bind_rows(assays_long)
  DBI::dbWriteTable(con, "assays", assays_final)

  # tests ------------------------------------------------------------------

  message("* Adding tests")
  testsTable <- combineListIntoTable(study[["tests"]], newColumnName = "modelID")
  DBI::dbWriteTable(con, "tests", testsTable)

  # annotations ----------------------------------------------------------------

  message("* Adding annotations")
  annotations <- data.frame(annotationID = names(study[["annotations"]]),
                            description = vapply(study[["annotations"]], function(x) x[["description"]], character(1)),
                            featureID = vapply(study[["annotations"]], function(x) x[["featureID"]], character(1)),
                            stringsAsFactors = FALSE)

  DBI::dbWriteTable(con, "annotations", annotations,
                    field.types = c("annotationID" = "varchar(50) PRIMARY KEY"))

  terms_list <- list()
  for (i in seq_along(study[["annotations"]])) {
    tmp_annotation <- study[["annotations"]][[i]]
    tmp_annotationID <- names(study[["annotations"]])[i]
    tmp_annotation_terms <- tmp_annotation[["terms"]]
    for (j in seq_along(tmp_annotation_terms)) {
      tmp_term <- tmp_annotation_terms[[j]]
      tmp_termID <- names(tmp_annotation_terms)[j]
      tmp <- data.frame(annotationID = tmp_annotationID,
                        termID = tmp_termID,
                        featureID = tmp_term,
                        stringsAsFactors = FALSE)
      terms_list <- c(terms_list, list(tmp))
    }
  }

  terms <- dplyr::bind_rows(terms_list)
  DBI::dbWriteTable(con, "terms", terms,
                    field.types = c("annotationID" = "varchar(50) REFERENCES annotations (annotationID)"))

  # results --------------------------------------------------------------------

  message("* Adding results")
  resultsList <- list()
  for (modelID in names(study[["results"]])) {
    for (testID in names(study[["results"]][[modelID]])) {
      tmp <- study[["results"]][[modelID]][[testID]]
      tmp[["modelID"]] <- modelID
      tmp[["testID"]] <- testID
      featureIDcolumnName <- colnames(tmp)[1]
      pivotCols <- setdiff(colnames(tmp), c(featureIDcolumnName, "modelID", "testID"))
      tmp <- tidyr::pivot_longer(tmp,
                                 cols = tidyselect::all_of(pivotCols),
                                 names_to = "resultsVariable",
                                 values_to = "resultsValue")
      resultsList <- c(resultsList, list(tmp))
    }
  }

  resultsTable <- Reduce(function(x, y) merge(x, y, all = TRUE), resultsList)
  DBI::dbWriteTable(con, "results", resultsTable)

  # enrichments ----------------------------------------------------------------

  message("* Adding enrichments")
  enrichmentsList <- list()
  for (modelID in names(study[["enrichments"]])) {
    for (testID in names(study[["enrichments"]][[modelID]])) {
      for (annotationID in names(study[["enrichments"]][[modelID]][[testID]])) {
        tmp <- study[["enrichments"]][[modelID]][[testID]][[annotationID]]
        tmp[["modelID"]] <- modelID
        tmp[["testID"]] <- testID
        tmp[["annotationID"]] <- annotationID
        enrichmentsList <- c(enrichmentsList, list(tmp))
      }
    }
  }

  enrichmentsTable <- Reduce(function(x, y) merge(x, y, all = TRUE), enrichmentsList)
  DBI::dbWriteTable(con, "enrichments", enrichmentsTable,
                    field.types = c(
                      "modelID" = "varchar(100) REFERENCES models (modelID)",
                      "testID" = "varchar(50) REFERENCES tests (testID)",
                      "annotationID" = "varchar(50) REFERENCES annotations (annotationID)",
                      "termID" = "varchar(50) REFERENCES terms (termID)"
                    ))

  # metaFeatures ---------------------------------------------------------------

  if (!is.null(study[["metaFeatures"]])) {
    message("* Adding meta-features")
    for (i in seq_along(study[["metaFeatures"]])) {
      tableName <- paste("features", names(study[["metaFeatures"]])[i], sep = "-")
      DBI::dbWriteTable(con, tableName, study[["metaFeatures"]][[i]])
    }
  }

  # Plots ----------------------------------------------------------------------

  if (!is.null(study[["plots"]])) {
    message("* Adding plots")
    plotsTable <- matrix(character(), ncol = 2)
    colnames(plotsTable) <- c("modelID", "plotID")
    for (i in seq_along(study[["plots"]])) {
      modelID <- names(study[["plots"]])[i]
      for (j in seq_along(study[["plots"]][[i]])) {
        plotID <- names(study[["plots"]][[i]])[j]
        tmpPlots <- matrix(c(modelID, plotID), ncol = 2)
        plotsTable <- rbind(plotsTable, tmpPlots)
      }
    }
    plotsTable <- as.data.frame(plotsTable, stringsAsFactors = FALSE)
    DBI::dbWriteTable(con, "plots", plotsTable)
  }

  # overlaps -------------------------------------------------------------------

  message("* Calculating overlaps between annotation terms")
  overlaps_list <- list()
  for (annotationID in names(study[["annotations"]])) {
    terms_tmp <- study[["annotations"]][[annotationID]][["terms"]]
    terms_enrichments <- dplyr::tbl(con, "enrichments") %>%
      dplyr::filter(.data$annotationID == !! annotationID) %>%
      dplyr::pull(.data$termID) %>%
      unique()
    terms_tmp <- terms_tmp[names(terms_tmp) %in% terms_enrichments]
    overlaps_tmp <- calc_pairwise_overlaps(terms_tmp) %>%
      dplyr::filter(.data$overlapSize > 0)
    overlaps_tmp[["annotationID"]] <- annotationID
    overlaps_list <- c(overlaps_list, list(overlaps_tmp))
  }
  overlaps <- dplyr::bind_rows(overlaps_list)

  DBI::dbWriteTable(con, "overlaps", overlaps,
                    field.types = c(
                      "annotationID" = "varchar(50) REFERENCES annotations (annotationID)",
                      "term1" = "varchar(50) REFERENCES terms (termID)",
                      "term2" = "varchar(50) REFERENCES terms (termID)",
                      "overlapSize" = "INTEGER",
                      "overlap" = "DOUBLE",
                      "jaccard" = "DOUBLE"
                    ))

  DBI::dbDisconnect(con)
  file.rename(tmpdb, filename)

  return(invisible(filename))
}

createPackage <- function(study, directoryname) {

  dir.create(directoryname, showWarnings = FALSE, recursive = TRUE)

  # DESCRIPTION
  description_file <- file.path(directoryname, "DESCRIPTION")
  pkgname <- basename(directoryname)
  pkgversion <- if (is.null(study[["version"]])) "0.0.0.9000" else study[["version"]]
  description <- data.frame(
    Package = pkgname,
    Title = sprintf("OmicAnalyzer study %s", study[["name"]]),
    Version = pkgversion,
    Description = sprintf("The OmicAnalyzer data package for the study \"%s\"",
                          study[["name"]]),
    stringsAsFactors = FALSE
  )
  write.dcf(description, file = description_file)

  # Database
  sqldir <- file.path(directoryname, "inst", "OmicAnalyzer")
  dir.create(sqldir, showWarnings = FALSE, recursive = TRUE)
  sqlfile <- file.path(sqldir, paste0(study[["name"]], ".sqlite"))
  createDatabase(study, sqlfile)

  # Plots
  if (!is.null(study[["plots"]])) {
    # Can't have duplicate plots in different models
    plotsAll <- lapply(study[["plots"]], function(x) names(x))
    if (length(plotsAll) != length(unique(plotsAll))) {
      stop("Cannot have duplicate plots in different studies")
    }
    namespace_file <- file.path(directoryname, "NAMESPACE")
    r_dir <- file.path(directoryname, "R")
    dir.create(r_dir, showWarnings = FALSE)
    r_file <- file.path(r_dir, "plots.R")
    dependencies <- character()
    exports <- character()
    code <- character()
    for (i in seq_along(study[["plots"]])) {
      for (j in seq_along(study[["plots"]][[i]])) {
        plot_name <- names(study[["plots"]][[i]])[j]
        dependencies <- c(dependencies, study[["plots"]][[i]][[j]][["packages"]])
        exports <- c(exports, sprintf("export(%s)", plot_name))
        plot_code <- deparse(study[["plots"]][[i]][[j]][["definition"]])
        plot_code[1] <- paste(plot_name, "<-", plot_code[1])
        code <- c(code, plot_code)
      }
    }
    if ("ggplot2" %in% dependencies) {
      dependencies <- c(dependencies, "rlang")
      exports <- c(exports, "importFrom(rlang,\".data\")")
    }
    dependencies <- sort(unique(dependencies))
    imports <- paste(dependencies, collapse = ", ")
    imports <- paste("Imports:", imports)
    cat(imports, file = description_file, sep = "\n", append = TRUE)
    exports <- sort(exports)
    writeLines(exports, namespace_file)
    writeLines(code, r_file)
  }

  return(invisible(directoryname))
}

#' Install a study as an R package
#'
#' @export
installStudy <- function(study, library = .libPaths()[1]) {
  stopifnot(inherits(study, "oaStudy"), dir.exists(library))

  tmpPath <- if (getRversion() >= "3.5.0") tempdir(check = TRUE) else tempdir()
  tmpPkgDir <- exportStudy(study, type = "package", path = tmpPath)
  on.exit(unlink(tmpPkgDir, recursive = TRUE, force = TRUE), add = TRUE)
  buildPkg(tmpPkgDir)
  tarball <- Sys.glob(sprintf("OAstudy%s_*.tar.gz", study[["name"]]))
  stopifnot(length(tarball) == 1)
  on.exit(file.remove(tarball), add = TRUE)
  utils::install.packages(tarball, lib = library, repos = NULL, quiet = TRUE)

  return(invisible(study))
}

buildPkg <- function(pkgDir) {
  r <- file.path(R.home("bin"), "R")
  system2(r, args = c("CMD", "build", pkgDir), stdout = NULL, stderr = NULL)
  return(invisible(pkgDir))
}
