
checkStudy <- function(study) {
  stopifnot(
    inherits(study, "oaStudy"),
    !is.null(study[["name"]])
  )
}

checkSamples <- function(samples, study = NULL) {
  stopifnot(inherits(samples, "data.frame"))

  if (is.null(study)) return(NULL)

  if (!study[["sampleID"]] %in% colnames(samples)) {
    stop(
      sprintf("The samples table doesn't contain the sampleID column named \"%s\"",
              study[["sampleID"]])
    )
  }

}

checkFeatures <- function(features, study = NULL) {
  stopifnot(inherits(features, "data.frame"))

  if (is.null(study)) return(NULL)

  if (!study[["featureID"]] %in% colnames(features)) {
    stop(
      sprintf("The features table doesn't contain the featureID column named \"%s\"",
              study[["featureID"]])
    )
  }

  key_column <- features[[study[["featureID"]]]]
  if (length(key_column) != length(unique(key_column))) {
    stop(sprintf("The key column \"%s\" contains duplicates", study[["featureID"]]))
  }

  return(NULL)
}

checkModels <- function(models, study = NULL) {
  stopifnot(inherits(models, "list"))

  return(NULL)
}

checkAssays <- function(assays, study = NULL) {
  stopifnot(inherits(assays, "list"))

  if (is.null(study)) return(NULL)

  if (!all(names(study[["models"]]) %in% names(assays))) {
    stop(sprintf("The names of the list do not include all of the model names"))
  }

  for (assay in assays) {
    stopifnot(inherits(assay, "matrix"))
    stopifnot(all(colnames(assay) %in% study[["samples"]][[study[["sampleID"]]]]))
    stopifnot(all(rownames(assay) %in% study[["features"]][[study[["featureID"]]]]))
  }

  return(NULL)
}

checkTests <- function(tests, study = NULL) {
  stopifnot(inherits(tests, "list"))

  return(NULL)
}

checkAnnotations <- function(annotations, study = NULL) {
  stopifnot(
    inherits(annotations, "list"),
    length(annotations) > 0
  )

  if (is.null(study)) return(NULL)

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
      annotations[[i]][["featureID"]] <- study[["featureID"]]
    }
    if (!annotations[[i]][["featureID"]] %in% colnames(study[["features"]])) {
      stop(sprintf("The ID \"%s\" for \"%s\" is not a column in the features table",
                   annotations[[i]][["featureID"]], annotationID))
    }
    if (is.null(annotations[[i]][["terms"]])) {
      stop(sprintf("Missing the list of terms for \"%s\"", annotationID))
    }
    universe <- unique(unlist(annotations[[i]][["terms"]]))
    if (!any(study[["features"]][[annotations[[i]][["featureID"]]]] %in% universe)) {
      stop(sprintf("None of the terms in \"%s\" contain feature IDs from \"%s\"\n",
                   annotationID, annotations[[i]][["featureID"]]),
           "Do you need specify the features column that was used for this enrichment analysis?")
    }
  }

  return(NULL)
}

checkResults <- function(results, study = NULL) {
  stopifnot(inherits(results, "list"))

  if (is.null(study)) return(NULL)

  if (!all(names(study[["models"]]) %in% names(results))) {
    stop(sprintf("The names of the list do not include all of the model names"))
  }

  for (i in seq_along(results)) {
    model <- results[[i]]
    model_name <- names(results)[i]
    stopifnot(inherits(model, "list"))
    stopifnot(model_name %in% names(study[["models"]]))
    for (j in seq_along(model)) {
      test <- model[[j]]
      test_name <- names(model)[j]
      stopifnot(inherits(test, "data.frame"))
      stopifnot(test_name %in% names(study[["tests"]]))
      stopifnot(study[["featureID"]] %in% colnames(test))
    }
  }

  return(NULL)
}

checkEnrichments <- function(enrichments, study = NULL) {
  stopifnot(inherits(enrichments, "list"))

  if (is.null(study)) return(NULL)

  if (!all(names(study[["models"]]) %in% names(enrichments))) {
    stop(sprintf("The names of the list do not include all of the model names"))
  }

  for (i in seq_along(enrichments)) {
    model <- enrichments[[i]]
    model_name <- names(enrichments)[i]
    stopifnot(inherits(model, "list"))
    stopifnot(model_name %in% names(study[["models"]]))
    for (j in seq_along(model)) {
      test <- model[[j]]
      test_name <- names(model)[j]
      stopifnot(inherits(test, "list"))
      stopifnot(test_name %in% names(study[["tests"]]))
      for (k in seq_along(test)) {
        annotation <- test[[k]]
        annotation_name <- names(test)[k]
        stopifnot(inherits(annotation, "data.frame"))
        stopifnot(annotation_name %in% names(study[["annotations"]]))
        stopifnot(c("termID", "description", "nominal", "adjusted")
                  %in% colnames(annotation))
        enrichments[[i]][[j]][[k]] <-
          annotation[, c("termID", "description", "nominal", "adjusted")]
      }
    }
  }

  return(NULL)
}

checkMetaFeatures <- function(metaFeatures, study = NULL) {
  stopifnot(inherits(metaFeatures, "data.frame"))

  if (is.null(study)) return(NULL)

  if (!study[["featureID"]] %in% colnames(metaFeatures)) {
    stop(
      sprintf("The metaFeatures table doesn't contain the featureID column named \"%s\"",
              study[["featureID"]])
    )
  }

  if (is.null(study[["features"]])) {
    stop("Please add the features table with addFeatures() prior to adding the metaFeatures table")
  }

  if (!all(metaFeatures[[study[["featureID"]]]] %in% study[["features"]][[study[["featureID"]]]])) {
    stop("The metaFeatures table contains features that are not in the features table")
  }

  return(NULL)
}

checkPlots <- function(plots, study = NULL) {
  stopifnot(
    inherits(plots, "list"),
    length(plots) > 0
  )

  # To do: check function signatures
  for (i in seq_along(plots)) {
    plotID <- names(plots)[i]
    if (is.null(plotID)) {
      stop("The plots list needs to be named")
    }
    if (!is.function(plots[[i]][["definition"]])) {
      stop(sprintf("%s is missing its function definition", plotID))
    }
    if (is.null(plots[[i]][["displayName"]])) {
      plots[[i]][["displayName"]] <- plotID
    }
  }

  return(NULL)
}
