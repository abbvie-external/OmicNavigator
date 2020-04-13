#' Create a study
#'
#' @param name Name of the study
#' @param description Description of the study
#' @param featureID The column name that contains the unique identifiers for the
#'   features measured in the study
#' @param sampleID The column name that contains the unique identifiers for the
#'   samples measured in the study
#' @inheritParams addSamples
#' @inheritParams addFeatures
#' @inheritParams addModels
#' @inheritParams addAssays
#' @inheritParams addContrasts
#' @inheritParams addAnnotations
#' @inheritParams addInferences
#' @inheritParams addEnrichments
#' @inheritParams addMetaFeatures
#'
#' @examples
#'
#' study <- createStudy(name = "ABC",
#'                      description = "An analysis of ABC")
#'
#' @export
createStudy <- function(name,
                        description = name,
                        samples = NULL,
                        features = NULL,
                        models = NULL,
                        assays = NULL,
                        contrasts = NULL,
                        annotations = NULL,
                        inferences = NULL,
                        enrichments = NULL,
                        metaFeatures = NULL,
                        featureID = "featureID",
                        sampleID = "sampleID")
{
  stopifnot(is.character(name), is.character(description))

  study <- list(name = name,
                description = description,
                samples = NULL,
                features = NULL,
                models = NULL,
                assays = NULL,
                contrasts = NULL,
                annotations = NULL,
                inferences = NULL,
                enrichments = NULL,
                metaFeatures = NULL,
                featureID = featureID,
                sampleID = sampleID)
  class(study) <- "oaStudy"

  if (!is.null(samples)) study <- addSamples(study, samples = samples)
  if (!is.null(features)) study <- addFeatures(study, features = features)
  if (!is.null(models)) study <- addModels(study, models = models)
  if (!is.null(assays)) study <- addAssays(study, assays = assays)
  if (!is.null(contrasts)) study <- addContrasts(study, contrasts = contrasts)
  if (!is.null(annotations)) study <- addAnnotations(study, annotations = annotations)
  if (!is.null(inferences)) study <- addInferences(study, inferences = inferences)
  if (!is.null(enrichments)) study <- addEnrichments(study, enrichments = enrichments)
  if (!is.null(metaFeatures)) study <- addMetaFeatures(study, metaFeatures = metaFeatures)

  return(study)
}

#' @export
print.oaStudy <- function(x, ...) {

  cat("== OmicAnalyzer ==\n")
  cat(sprintf("* Study name: %s\n", x$name))
  cat(sprintf("* Feature ID column name: %s\n", x$featureID))
  cat(sprintf("* Sample ID column name: %s\n", x$sampleID))

  if (!is.null(x$samples)) {
    cat(sprintf("* Samples: %d\n", nrow(x$samples)))
    cat(sprintf("* Sample metadata variables: %d\n", ncol(x$samples)))
  }

  if (!is.null(x$features)) {
    cat(sprintf("* Features: %d\n", nrow(x$features)))
    cat(sprintf("* Feature metadata variables: %d\n", ncol(x$features)))
  }

  if (!is.null(x$metaFeatures)) {
    cat(sprintf("* Meta-feature metadata variables: %d\n", ncol(x$metaFeatures) - 1))
  }

  if (!is.null(x$models)) {
    cat(sprintf("* Models: %d\n", length(x$models)))
    for (i in seq_along(x$models)) {
      cat(sprintf("  * \"%s\"\n", names(x$models)[i]))
    }
  }

  if (!is.null(x$assays)) {
    cat(sprintf("* Assays: %d\n", length(x$assays)))
    for (i in seq_along(x$assays)) {
      cat(sprintf("  * \"%s\": %d x %d\n", names(x$assays)[i], nrow(x$assays[[i]]),
          ncol(x$assays[[i]])))
    }
  }

  if (!is.null(x$contrasts)) {
    cat(sprintf("* Contrasts: %d\n", length(x$contrasts)))
    for (i in seq_along(x$contrasts)) {
      cat(sprintf("  * \"%s\"\n", names(x$contrasts)[i]))
    }
  }

  if (!is.null(x$annotations)) {
    cat(sprintf("* Annotations: %d\n", length(x$annotations)))
    for (i in seq_along(x$annotations)) {
      cat(sprintf("  * \"%s\"\n", names(x$annotations)[i]))
    }
  }

  if (!is.null(x$inferences)) {
    cat("* Inferences:\n")
    for (i in seq_along(x$inferences)) {
      cat(sprintf("  * \"%s\":\n", names(x$inferences)[i]))
      for (j in seq_along(x$inferences[[i]])) {
        cat(sprintf("    * \"%s\": %d results\n", names(x$inferences[[i]])[j],
                    nrow(x$inferences[[i]][[j]])))
      }
    }
  }

  if (!is.null(x$enrichments)) {
    cat("* Enrichments:\n")
    for (i in seq_along(x$enrichments)) {
      cat(sprintf("  * \"%s\":\n", names(x$enrichments)[i]))
      for (j in seq_along(x$enrichments[[i]])) {
        cat(sprintf("    * \"%s\":\n", names(x$enrichments[[i]])[j]))
        for (k in seq_along(x$enrichments[[i]][[j]])) {
          cat(sprintf("      * \"%s\": %d results\n",
                      names(x$enrichments[[i]][[j]])[k],
                      nrow(x$enrichments[[i]][[j]][[k]])))
        }
      }
    }
  }

  return(invisible(x))
}

#' Add sample metadata
#'
#' @param samples A table of metadata variables that describe the samples in the
#'   study. The table must contain the unique sampleID used for the study. Also,
#'   the object must inherit from the class data.frame.
#'
#' @export
addSamples <- function(study, samples, overwrite = FALSE) {
  stopifnot(inherits(study, "oaStudy"), inherits(samples, "data.frame"))

  if (!study$sampleID %in% colnames(samples)) {
    stop(
      sprintf("The samples table doesn't contain the sampleID column named \"%s\"",
              study$sampleID)
    )
  }

  if (overwrite || is.null(study$samples)) {
    study$samples <- samples
  } else {
    stop("Sample metadata already exists. Set overwrite=TRUE to overwrite.")
  }

  return(study)
}

#' Add feature metadata
#'
#' @param features A table of metadata variables that describe the features in the
#'   study. The table must contain the unique featureID used for the study. Also,
#'   the object must inherit from the class data.frame.
#'
#' @export
addFeatures <- function(study, features, overwrite = FALSE) {
  stopifnot(inherits(study, "oaStudy"), inherits(features, "data.frame"))

  if (!study$featureID %in% colnames(features)) {
    stop(
      sprintf("The features table doesn't contain the featureID column named \"%s\"",
              study$featureID)
    )
  }

  key_column <- features[[study$featureID]]
  if (length(key_column) != length(unique(key_column))) {
    stop(sprintf("The key column \"%s\" contains duplicates", study$featureID))
  }

  if (overwrite || is.null(study$features)) {
    study$features <- features
  } else {
    stop("Feature metadata already exists. Set overwrite=TRUE to overwrite.")
  }

  return(study)
}

#' Add models
#'
#' @param models The models analyzed in the study. The input is a named
#'   character vector. The names correspond to the names of the models. The
#'   elements correspond to the descriptions of the models.
#'
#' @export
addModels <- function(study, models, overwrite = FALSE) {
  stopifnot(inherits(study, "oaStudy"), is.character(models))

  if (overwrite || is.null(study$models)) {
    study$models <- models
  } else {
    stop("Models metadata already exists. Set overwrite=TRUE to overwrite.")
  }

  return(study)
}

#' Add assays
#'
#' @param assays The assays from the study. The input is a named list. The names
#'   of the list correspond to the model names. Each element in the list should
#'   be a matrix of quantifications for the assays. The column names should be
#'   the sample IDs and the rows should be the feature IDs.
#'
#' @export
addAssays <- function(study, assays, overwrite = FALSE) {
  stopifnot(inherits(study, "oaStudy"), inherits(assays, "list"))

  if (!all(names(study$models) %in% names(assays))) {
    stop(sprintf("The names of the list do not include all of the model names"))
  }

  for (assay in assays) {
    stopifnot(inherits(assay, "matrix"))
    stopifnot(all(colnames(assay) %in% study$samples[[study$sampleID]]))
    stopifnot(all(rownames(assay) %in% study$features[[study$featureID]]))
  }

  if (overwrite || is.null(study$assays)) {
    study$assays <- assays
  } else {
    stop("assays metadata already exists. Set overwrite=TRUE to overwrite.")
  }

  return(study)
}

#' Add contrasts
#'
#' @param contrasts The contrasts tested by the model(s). A named character
#'   vector. The names correspond to the name of each contrast. The elements
#'   correspond to the description of each contrast.
#'
#' @export
addContrasts <- function(study, contrasts, overwrite = FALSE) {
  stopifnot(inherits(study, "oaStudy"), is.character(contrasts))

  if (overwrite || is.null(study$contrasts)) {
    study$contrasts <- contrasts
  } else {
    stop("The contrasts already exist. Set overwrite=TRUE to overwrite.")
  }

  return(study)
}

#' Add annotations
#'
#' @param annotations The annotations used for the enrichment analyses. The
#'   input is a nested list. The top-level list contains one entry per
#'   annotation database, e.g. reactome. The names correspond to the name of
#'   each annotation database. Each of these elements should be list of that
#'   contains more information about each annotation database. Specifically the
#'   sublist should contain 1) \code{description}, a character vector that
#'   describes the resource, 2) \code{featureID}, the name of the column in the
#'   features table that was used for the enrichment analysis (if omitted, it is
#'   assumed to be the main featureID used for the study), and 3) \code{terms},
#'   a list of annotation terms. The names of \code{terms} sublist correspond to
#'   the name of the annotation terms. Each of the annotation terms should be a
#'   character vector of feature IDs.
#'
#' @export
addAnnotations <- function(study, annotations, overwrite = FALSE) {
  stopifnot(inherits(study, "oaStudy"), inherits(annotations, "list"),
            length(annotations) > 0)

  for (i in seq_along(annotations)) {
    annotationID <- names(annotations)[i]
    if (is.null(annotationID)) {
      stop("The annotation list needs to be named")
    }
    if (is.null(annotations[[i]][["description"]])) {
      annotations[[i]][["description"]] <- sprintf("Annotation terms from %s",
                                                   annotationID)
    }
    if (is.null(annotations[[i]][["featureID"]])) {
      annotations[[i]][["featureID"]] <- study$featureID
    }
    if (!annotations[[i]][["featureID"]] %in% colnames(study$features)) {
      stop(sprintf("The ID \"%s\" for \"%s\" is not a column in the features table",
           annotations[[i]][["featureID"]], annotationID))
    }
    if (is.null(annotations[[i]][["terms"]])) {
      stop(sprintf("Missing the list of terms for \"%s\"", annotationID))
    }
    universe <- unique(unlist(annotations[[i]][["terms"]]))
    if (!any(study$features[[study$featureID]] %in% universe)) {
      stop(sprintf("None of the terms in \"%s\" contain feature IDs from \"%s\"\n",
                   annotationID, annotations[[i]][["featureID"]]),
           "Do you need specify the features column that was used for this enrichment analysis?")
    }
  }

  if (overwrite || is.null(study$annotations)) {
    study$annotations <- annotations
  } else {
    stop("The annotations already exist. Set overwrite=TRUE to overwrite.")
  }

  return(study)
}

#' Add inference results
#'
#' @param inferences The inference results from each model. The input is a
#'   nested named list. The names of the list correspond to the model names.
#'   Each element in the list should be a list of data frames with inference
#'   results, one for each contrast. The featureID column needs to be included
#'   in each table.
#'
#' @export
addInferences <- function(study, inferences, overwrite = FALSE) {
  stopifnot(inherits(study, "oaStudy"), inherits(inferences, "list"))

  if (!all(names(study$models) %in% names(inferences))) {
    stop(sprintf("The names of the list do not include all of the model names"))
  }

  for (i in seq_along(inferences)) {
    model <- inferences[[i]]
    model_name <- names(inferences)[i]
    stopifnot(inherits(model, "list"))
    stopifnot(model_name %in% names(study$models))
    for (j in seq_along(model)) {
      contrast <- model[[j]]
      contrast_name <- names(model)[j]
      stopifnot(inherits(contrast, "data.frame"))
      stopifnot(contrast_name %in% names(study$contrasts))
      stopifnot(study$featureID %in% colnames(contrast))
    }
  }

  if (overwrite || is.null(study$inferences)) {
    study$inferences <- inferences
  } else {
    stop("The inference results already exist. Set overwrite=TRUE to overwrite.")
  }

  return(study)
}

#' Add enrichment results
#'
#' To reduce storage and compute time, it is highly recommended to first filter
#' the enrichment results for statistical signficance prior to adding them to
#' the study.
#'
#' @param enrichments The enrichment results from each model. The input is a
#'   nested named list. The names of the list correspond to the model names.
#'   Each list element should be a list of the contrasts tested. The names
#'   correspond to the contrast names. Each list element should be another list
#'   of annotation databases. The names correspond to the annotation databases.
#'   Each of these elements should be a data frame with enrichment results. Each
#'   table must have a column named "termID" that contains the annotation terms.
#'
#' @export
addEnrichments <- function(study, enrichments, overwrite = FALSE) {
  stopifnot(inherits(study, "oaStudy"), inherits(enrichments, "list"))

  if (!all(names(study$models) %in% names(enrichments))) {
    stop(sprintf("The names of the list do not include all of the model names"))
  }

  for (i in seq_along(enrichments)) {
    model <- enrichments[[i]]
    model_name <- names(enrichments)[i]
    stopifnot(inherits(model, "list"))
    stopifnot(model_name %in% names(study$models))
    for (j in seq_along(model)) {
      contrast <- model[[j]]
      contrast_name <- names(model)[j]
      stopifnot(inherits(contrast, "list"))
      stopifnot(contrast_name %in% names(study$contrasts))
      for (k in seq_along(contrast)) {
        annotation <- contrast[[k]]
        annotation_name <- names(contrast)[k]
        stopifnot(inherits(annotation, "data.frame"))
        stopifnot(annotation_name %in% names(study$annotations))
        stopifnot("termID" %in% colnames(annotation))
      }
    }
  }

  if (overwrite || is.null(study$enrichments)) {
    study$enrichments <- enrichments
  } else {
    stop("The enrichment results already exist. Set overwrite=TRUE to overwrite.")
  }

  return(study)
}

#' Add meta-feature metadata
#'
#' @param metaFeatures A table of metadata variables that describe the
#'   meta-features in the study. This is useful anytime there are metadata
#'   variables that cannot be mapped 1:1 to your features. For example, a
#'   peptide may be associated with multiple proteins. The table must contain
#'   the unique featureID used for the study. Also, the object must inherit from
#'   the class data.frame.
#'
#' @export
addMetaFeatures <- function(study, metaFeatures, overwrite = FALSE) {
  stopifnot(inherits(study, "oaStudy"), inherits(metaFeatures, "data.frame"))

  if (!study$featureID %in% colnames(metaFeatures)) {
    stop(
      sprintf("The metaFeatures table doesn't contain the featureID column named \"%s\"",
              study$featureID)
    )
  }

  if (is.null(study$features)) {
    stop("Please add the features table with addFeatures() prior to adding the metaFeatures table")
  }

  if (!all(metaFeatures[[study$featureID]] %in% study$features[[study$featureID]])) {
    stop("The metaFeatures table contains features that are not in the features table")
  }

  if (overwrite || is.null(study$metaFeatures)) {
    study$metaFeatures <- metaFeatures
  } else {
    stop("Feature metadata already exists. Set overwrite=TRUE to overwrite.")
  }

  return(study)
}

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

  message("* Adding meta-features")
  fields_metaFeatures <- c(
    sprintf("varchar(50) REFERENCES features (%s)", study$featureID)
  )
  names(fields_metaFeatures) <- study$featureID
  DBI::dbWriteTable(con, "metaFeatures", study$metaFeatures,
                    field.types = fields_metaFeatures)

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
