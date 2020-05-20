
checkStudy <- function(study) {
  stopifnot(
    inherits(study, "oaStudy"),
    !is.null(study[["name"]])
  )
}

checkSamples <- function(samples) {
  stopifnot(
    is.list(samples),
    !is.data.frame(samples),
    length(samples) > 0,
    !is.null(names(samples))
  )

  for (i in seq_along(samples)) {
    stopifnot(
      inherits(samples[[i]], "data.frame"),
      nrow(samples[[i]]) > 0,
      ncol(samples[[i]]) > 0
    )
    hasUniqueIdColumn(samples[[i]])
  }

  return(NULL)
}

checkFeatures <- function(features) {
  stopifnot(
    is.list(features),
    !is.data.frame(features),
    length(features) > 0,
    !is.null(names(features))
  )

  for (i in seq_along(features)) {
    stopifnot(
      inherits(features[[i]], "data.frame"),
      nrow(features[[i]]) > 0,
      ncol(features[[i]]) > 0
    )
    hasUniqueIdColumn(features[[i]])
  }

  return(NULL)
}

checkModels <- function(models, study = NULL) {
  stopifnot(inherits(models, "list"))

  return(NULL)
}

checkAssays <- function(assays) {
  stopifnot(
    is.list(assays),
    !is.data.frame(assays),
    length(assays) > 0,
    !is.null(names(assays))
  )

  for (i in seq_along(assays)) {
    stopifnot(
      inherits(assays[[i]], "data.frame"),
      nrow(assays[[i]]) > 0,
      ncol(assays[[i]]) > 0
    )
  }

  return(NULL)
}

checkTests <- function(tests, study = NULL) {
  stopifnot(
    is.list(tests),
    !is.data.frame(tests),
    length(tests) > 0,
    !is.null(names(tests))
  )

  for (i in seq_along(tests)) {
    stopifnot(
      inherits(tests[[i]], "data.frame"),
      nrow(tests[[i]]) > 0,
      ncol(tests[[i]]) > 0
    )
    hasUniqueIdColumn(tests[[i]])
  }

  return(NULL)
}

checkAnnotations <- function(annotations) {
  stopifnot(
    is.list(annotations),
    !is.data.frame(annotations),
    length(annotations) > 0,
    !is.null(names(annotations))
  )

  for (i in seq_along(annotations)) {
    annotationID <- names(annotations)[i]
    if (is.null(annotations[[i]][["description"]])) {
      stop(sprintf("Missing description for annotation \"%s\"", annotationID))
    }
    if (is.null(annotations[[i]][["featureID"]])) {
      stop(sprintf("Missing featureID for annotation \"%s\"", annotationID))
    }
    if (is.null(annotations[[i]][["terms"]])) {
      stop(sprintf("Missing the list of terms for \"%s\"", annotationID))
    }
    terms <- annotations[[i]][["terms"]]
    if (!is.list(terms) ||
        is.data.frame(terms) ||
        length(terms) <= 0 ||
        is.null(names(terms)) ||
        !all(vapply(terms, is.character, logical(1)))) {
      stop(sprintf("The terms for \"%s\" must be a named list of character vectors",
                   annotationID))
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
