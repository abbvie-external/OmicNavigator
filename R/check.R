
checkStudy <- function(study) {
  stopifnot(
    inherits(study, "oaStudy"),
    !is.null(study[["name"]])
  )
}

checkList <- function(x) {
  listName <- deparse(substitute(x))

  if (!is.list(x)) {
    stop(sprintf("The object \"%s\" must be a list", listName))
  }

  if (is.data.frame(x)) {
    stop(sprintf("The object \"%s\" must be a list, not a data frame", listName))
  }

  if (isEmpty(x)) {
    stop(sprintf("The list \"%s\" cannot be empty", listName))
  }

  if (is.null(names(x))) {
    stop(sprintf("The elements of list \"%s\" must be named", listName))
  }

  return(NULL)
}

checkSamples <- function(samples) {
  checkList(samples)

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
  checkList(features)

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
  checkList(assays)

  for (i in seq_along(assays)) {
    stopifnot(
      inherits(assays[[i]], "data.frame"),
      nrow(assays[[i]]) > 0,
      ncol(assays[[i]]) > 0
    )
  }

  return(NULL)
}

checkTests <- function(tests) {
  checkList(tests)

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
  checkList(annotations)

  for (i in seq_along(annotations)) {
    checkList(annotations[[i]])
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
    checkList(terms)
    if (!all(vapply(terms, is.character, logical(1)))) {
      stop(sprintf("The terms for \"%s\" must be a named list of character vectors",
                   annotationID))
    }
  }

  return(NULL)
}

checkResults <- function(results) {
  checkList(results)

  if ("defaults" %in% names(results)) {
    stop("The results cannot be shared using the model ID \"defaults\"")
  }

  for (i in seq_along(results)) {
    checkList(results[[i]])
    for (j in seq_along(results[[i]])) {
      dataFrame <- results[[i]][[j]]
      stopifnot(
        is.data.frame(dataFrame),
        is.character(dataFrame[, 1]),
        vapply(dataFrame[, -1], is.numeric, logical(1))
      )
      hasUniqueIdColumn(dataFrame)
    }
  }

  return(NULL)
}

checkEnrichments <- function(enrichments) {
  checkList(enrichments)

  if ("defaults" %in% names(enrichments)) {
    stop("The enrichments cannot be shared using the model ID \"defaults\"")
  }

  for (i in seq_along(enrichments)) {
    model <- enrichments[[i]]
    checkList(model)
    for (j in seq_along(model)) {
      test <- model[[j]]
      checkList(test)
      for (k in seq_along(test)) {
        annotation <- test[[k]]
        stopifnot(inherits(annotation, "data.frame"))
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
  checkList(metaFeatures)

  for (i in seq_along(metaFeatures)) {
    stopifnot(
      inherits(metaFeatures[[i]], "data.frame"),
      nrow(metaFeatures[[i]]) > 0,
      ncol(metaFeatures[[i]]) > 0
    )
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
