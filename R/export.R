#' Export a study
#'
#' @param study An OmicAnalyzer study
#' @param type Export to a RDS file ("rds"), SQLite database ("sqlite"), or an
#'   R package ("package")
#' @param path Optional file path to save the object
#'
#' @export
exportStudy <- function(study, type = c("rds", "sqlite", "package"), path = NULL) {
  stopifnot(inherits(study, "oaStudy"))

  type <- match.arg(type)

  if (type == "rds") {
    message(sprintf("Exporting study \"%s\" to an RDS file", study$name))
    filename <- paste0(study$name, ".rds")
    if (!is.null(path)) filename <- file.path(path, filename)
    saveRDS(object = study, file = filename)
    message(sprintf("Exported study to %s", filename))
    return(invisible(filename))
  } else if (type == "sqlite") {
    message(sprintf("Exporting study \"%s\" to an SQLite database", study$name))
    filename <- paste0(study$name, ".sqlite")
    if (!is.null(path)) filename <- file.path(path, filename)
    createDatabase(study, filename)
    message(sprintf("Exported study to %s", filename))
    return(invisible(filename))
  } else if (type == "package") {
    message(sprintf("Exporting study \"%s\" to an R package", study$name))
    directoryname <- paste0("OAstudy", study$name)
    if (!is.null(path)) directoryname <- file.path(path, directoryname)
    createPackage(study, directoryname)
    message(sprintf("Exported study to %s", directoryname))
    return(invisible(directoryname))
  }
}

createDatabase <- function(study, filename) {

  tmpdb <- tempfile(fileext = ".sqlite")
  con <- DBI::dbConnect(RSQLite::SQLite(), tmpdb)

  # samples --------------------------------------------------------------------

  message("* Adding samples")
  fields_samples <- c("varchar(50) PRIMARY KEY")
  names(fields_samples) <- study$sampleID
  DBI::dbWriteTable(con, "samples", samples, field.types = fields_samples)
  DBI::dbExecute(con,
                 sprintf("CREATE UNIQUE INDEX samples_index ON samples(%s)",
                         study$sampleID))

  # features -------------------------------------------------------------------

  message("* Adding features")
  fields_features <- c("varchar(50) PRIMARY KEY")
  names(fields_features) <- study$featureID
  DBI::dbWriteTable(con, "features", study$features, field.types = fields_features)
  DBI::dbExecute(con,
                 sprintf("CREATE UNIQUE INDEX feature_index ON features(%s)",
                         study$featureID))

  # models ---------------------------------------------------------------------

  message("* Adding models")
  models <- data.frame(modelID = names(study$models),
                       description = study$models,
                       stringsAsFactors = FALSE)
  DBI::dbWriteTable(con, "models", models,
                    field.types = c("modelID" = "varchar(100) PRIMARY KEY",
                                    "description" = "varchar(200)"))

  # assays ---------------------------------------------------------------------

  message("* Adding assays")
  assays_long <- vector(mode = "list", length = length(study$assays))
  for (i in seq_along(study$assays)) {
    assays_long[[i]] <- study$assays[[i]] %>%
      as.data.frame %>%
      dplyr::mutate(featureID = rownames(.)) %>%
      tidyr::pivot_longer(cols = -featureID,
                          names_to = "sampleID",
                          values_to = "quantification") %>%
      dplyr::mutate(modelID = names(study$assays)[i]) %>%
      dplyr::select(featureID, sampleID, modelID, quantification)
  }
  assays_final <- dplyr::bind_rows(assays_long)
  colnames(assays_final)[1:2] <- c(study$featureID, study$sampleID)
  fields_assays <- c(
    sprintf("varchar(50) REFERENCES features (%s)", study$featureID),
    sprintf("varchar(50) REFERENCES samples (%s)", study$sampleID),
    "varchar(100) REFERENCES models (modelID)"
  )
  names(fields_assays) <- c(study$featureID, study$sampleID, "modelID")
  DBI::dbWriteTable(con, "assays", assays_final,
                    field.types = fields_assays)
  DBI::dbExecute(con,
                 sprintf("CREATE UNIQUE INDEX assays_index ON assays(%s, %s, modelID)",
                         study$featureID, study$sampleID))

  # contrasts ------------------------------------------------------------------

  message("* Adding contrasts")
  contrasts <- data.frame(contrastID = names(study$contrasts),
                          description = study$contrasts,
                          stringsAsFactors = FALSE)
  DBI::dbWriteTable(con, "contrasts", contrasts,
                    field.types = c("contrastID" = "varchar(50) PRIMARY KEY"))

  # annotations ----------------------------------------------------------------

  message("* Adding annotations")
  annotations <- data.frame(annotationID = names(study$annotations),
                            description = vapply(study$annotations, function(x) x[["description"]], character(1)),
                            featureID = vapply(study$annotations, function(x) x[["featureID"]], character(1)),
                            stringsAsFactors = FALSE)

  DBI::dbWriteTable(con, "annotations", annotations,
                    field.types = c("annotationID" = "varchar(50) PRIMARY KEY"))

  terms_list <- list()
  for (annotationID in names(study$annotations)) {
    tmp <- data.frame(annotationID = annotationID,
                      termID = names(study$annotations[[annotationID]][["terms"]]),
                      stringsAsFactors = FALSE)
    terms_list <- c(terms_list, list(tmp))
  }

  terms <- dplyr::bind_rows(terms_list)
  DBI::dbWriteTable(con, "terms", terms,
                    field.types = c("annotationID" = "varchar(50) REFERENCES annotations (annotationID)"))

  # inferences -----------------------------------------------------------------

  message("* Adding inferences")
  inferences_list <- list()
  for (modelID in names(study$inferences)) {
    for (contrastID in names(study$inferences[[modelID]])) {
      tmp <- study$inferences[[modelID]][[contrastID]]
      tmp$modelID <- modelID
      tmp$contrastID <- contrastID
      inferences_list <- c(inferences_list, list(tmp))
    }
  }

  inferences <- Reduce(function(x, y) merge(x, y, all = TRUE), inferences_list)
  fields_inferences <- c(
    sprintf("varchar(50) REFERENCES features (%s)", study$featureID),
    "varchar(50) REFERENCES contrasts (contrastID)",
    "varchar(100) REFERENCES models (modelID)"
  )
  names(fields_inferences) <- c(study$featureID, "contrastID", "modelID")
  DBI::dbWriteTable(con, "inferences", inferences,
                    field.types = fields_inferences)

  # enrichments ----------------------------------------------------------------

  message("* Adding enrichments")
  enrichments_list <- list()
  for (modelID in names(study$enrichments)) {
    for (contrastID in names(study$enrichments[[modelID]])) {
      for (annotationID in names(study$enrichments[[modelID]][[contrastID]])) {
        tmp <- study$enrichments[[modelID]][[contrastID]][[annotationID]]
        tmp$modelID <- modelID
        tmp$contrastID <- contrastID
        tmp$annotationID <- annotationID
        enrichments_list <- c(enrichments_list, list(tmp))
      }
    }
  }

  enrichments <- Reduce(function(x, y) merge(x, y, all = TRUE), enrichments_list)
  DBI::dbWriteTable(con, "enrichments", enrichments,
                    field.types = c(
                      "modelID" = "varchar(100) REFERENCES models (modelID)",
                      "contrastID" = "varchar(50) REFERENCES contrasts (contrastID)",
                      "annotationID" = "varchar(50) REFERENCES annotations (annotationID)",
                      "termID" = "varchar(50) REFERENCES terms (termID)"
                    ))

  # metaFeatures ---------------------------------------------------------------

  if (!is.null(study$metaFeatures)) {
    message("* Adding meta-features")
    fields_metaFeatures <- c(
      sprintf("varchar(50) REFERENCES features (%s)", study$featureID)
    )
    names(fields_metaFeatures) <- study$featureID
    DBI::dbWriteTable(con, "metaFeatures", study$metaFeatures,
                      field.types = fields_metaFeatures)
  }

  # Overlaps -------------------------------------------------------------------

  message("* Calculating overlaps between annotation terms")
  overlaps_list <- list()
  for (annotationID in names(study$annotations)) {
    terms_tmp <- study$annotations[[annotationID]][["terms"]]
    terms_enriched <- dplyr::tbl(con, "enrichments") %>%
      dplyr::filter(annotationID == !! annotationID) %>%
      dplyr::pull(termID) %>%
      unique()
    terms_tmp <- terms_tmp[names(terms_tmp) %in% terms_enriched]
    overlaps_tmp <- calc_pairwise_overlaps(terms_tmp)
    overlaps_tmp$annotationID <- annotationID
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
  description <- data.frame(
    Package = pkgname,
    Title = sprintf("OmicAnalyzer study %s", study$name),
    Version = "0.0.0.9000",
    Description = sprintf("The OmicAnalyzer data package for the study \"%s\"",
                          study$name),
    stringsAsFactors = FALSE
  )
  write.dcf(description, file = description_file)

  # Database
  sqldir <- file.path(directoryname, "inst", "OmicAnalyzer")
  dir.create(sqldir, showWarnings = FALSE, recursive = TRUE)
  sqlfile <- file.path(sqldir, paste0(study$name, ".sqlite"))
  createDatabase(study, sqlfile)

  # Plots
  if (!is.null(study$plots)) {
    namespace_file <- file.path(directoryname, "NAMESPACE")
    r_dir <- file.path(directoryname, "R")
    dir.create(r_dir, showWarnings = FALSE)
    r_file <- file.path(r_dir, "plots.R")
    dependencies <- character()
    exports <- character()
    code <- character()
    for (i in seq_along(study$plots)) {
      plot_name <- names(study$plots)[i]
      dependencies <- c(dependencies, study$plots[[i]][["packages"]])
      exports <- c(exports, sprintf("export(%s)", plot_name))
      plot_code <- deparse(study$plots[[i]][["definition"]])
      plot_code[1] <- paste(plot_name, "<-", plot_code[1])
      code <- c(code, plot_code)
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

  tmpPkgDir <- exportStudy(study, type = "package", path = tempdir(check = TRUE))
  on.exit(unlink(tmpPkgDir, recursive = TRUE, force = TRUE), add = TRUE)
  buildPkg(tmpPkgDir)
  tarball <- Sys.glob(sprintf("OAstudy%s_*.tar.gz", study$name))
  stopifnot(length(tarball) == 1)
  on.exit(file.remove(tarball), add = TRUE)
  install.packages(tarball, lib = library, repos = NULL)

  return(invisible(study))
}

buildPkg <- function(pkgDir) {
  r <- file.path(R.home("bin"), "R")
  system2(r, args = c("CMD", "build", pkgDir))
  return(invisible(pkgDir))
}
