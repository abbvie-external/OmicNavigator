
checkStudy <- function(study) {
  stopifnot(
    inherits(study, "onStudy"),
    !is.null(study[["name"]])
  )
}

checkName <- function(name) {
  stopifnot(
    is.character(name),
    length(name) == 1
  )

  # Confirm package name is valid
  regexPackage <- .standard_regexps()[["valid_package_name"]]
  regexPackage <- paste0("^", regexPackage, "$")
  nameIsValid <- grepl(regexPackage, name)
  if (!nameIsValid) {
    stop("Invalid name for a study package. It must follow these rules:\n",
         "* Begin with a letter\n",
         "* End with a letter or a number\n",
         "* Be at least two characters long\n",
         "* Only contain alphanumeric characters and periods (full stops)\n"
    )
  }
}

checkDescription <- function(description) {
  stopifnot(
    is.character(description),
    length(description) == 1
  )
}

checkVersion <- function(version) {
  if (is.null(version)) return(NULL)
  stopifnot(
    is.character(version),
    length(version) == 1
  )

  # Confirm version string is valid
  regexVersion <- .standard_regexps()[["valid_package_version"]]
  regexVersion <- paste0("^", regexVersion, "$")
  versionIsValid <- grepl(regexVersion, version)
  if (!versionIsValid) {
    stop("Invalid version for a study package. It must follow these rules:\n",
         "* Begin with a number\n",
         "* End with a number\n",
         "* Contain at least one period (full stop) or dash\n",
         "* No letters\n"
    )
  }
}

checkList <- function(x) {
  listName <- deparse(substitute(x))

  if (!is.list(x)) {
    stop(sprintf("The object \"%s\" must be a list", listName))
  }

  if (is.data.frame(x)) {
    stop(sprintf("The object \"%s\" must be a list, not a data frame", listName))
  }

  if (!isEmpty(x) && is.null(names(x))) {
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
    warnIfNonCharacterCols(features[[i]])
  }

  return(NULL)
}

checkModels <- function(models) {
  checkList(models)

  for (i in seq_along(models)) {
    stopifnot(
      is.character(models[[i]]),
      length(models[[i]]) == 1
    )
  }

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
    checkList(tests[[i]])
    for (j in seq_along(tests[[i]])) {
      stopifnot(
        is.character(tests[[i]][[j]]),
        length(tests[[i]][[j]]) == 1
      )
    }
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
    stop("The results cannot be shared using the modelID \"defaults\"")
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
    stop("The enrichments cannot be shared using the modelID \"defaults\"")
  }

  for (i in seq_along(enrichments)) {
    model <- enrichments[[i]]
    checkList(model)
    for (j in seq_along(model)) {
      annotation <- model[[j]]
      checkList(annotation)
      for (k in seq_along(annotation)) {
        test <- annotation[[k]]
        stopifnot(inherits(test, "data.frame"))
        stopifnot(c("termID", "description", "nominal", "adjusted")
                  %in% colnames(test))
        enrichments[[i]][[j]][[k]] <-
          test[, c("termID", "description", "nominal", "adjusted")]
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
    warnIfNonCharacterCols(metaFeatures[[i]])
  }

  return(NULL)
}

checkPlots <- function(plots) {
  checkList(plots)

  for (i in seq_along(plots)) {
    checkList(plots[[i]])
    for (j in seq_along(plots[[i]])) {
      plotEntry <- plots[[i]][[j]]
      checkList(plotEntry)
      plotID <- names(plots[[i]])[j]
      plotFunction <- getPlotFunction(plotID)
      if (!is.function(plotFunction)) {
        stop(sprintf("Unable to find function \"%s\"", plotID))
      }
      argsObserved <- names(formals(plotFunction))
      argsExpected <- c("x", "featureID")
      if (!identical(argsObserved, argsExpected)) {
        stop(
          sprintf("%s has an incorrect function signature\n", plotID),
          sprintf("Expected arguments: %s\n", paste(argsExpected, collapse = ", ")),
          sprintf("Observed arguments: %s\n", paste(argsObserved, collapse = ", "))
        )
      }
      if (is.null(plotEntry[["displayName"]])) {
        stop(sprintf("Must define displayName for plot \"%s\"", plotID))
      }
    }
  }

  return(NULL)
}

checkBarcodes <- function(barcodes) {
  checkList(barcodes)

  for (i in seq_along(barcodes)) {
    barcode <- barcodes[[i]]
    checkList(barcode)
    statistic <- barcode[["statistic"]]
    stopifnot(
      is.character(statistic),
      length(statistic) == 1
    )
    elements <- setdiff(names(barcode), "statistic")
    for (e in elements) {
      stopifnot(length(barcode[[e]]) == 1)
    }
  }

  return(NULL)
}

checkReports <- function(reports) {
  checkList(reports)

  for (i in seq_along(reports)) {
    report <- reports[[i]]
    stopifnot(
      is.character(report),
      length(report) == 1
    )
    if (!isUrl(report) && !file.exists(report)) {
      stop("Report must be a URL or a path to an existing file")
    }
  }

  return(NULL)
}

checkOverlaps <- function(overlaps) {
  checkList(overlaps)

  for (i in seq_along(overlaps)) {
    overlap <- overlaps[[i]]
    stopifnot(
      is.data.frame(overlap),
      nrow(overlap) > 0,
      ncol(overlap) == 5,
      identical(colnames(overlap),
                c("term1", "term2", "overlapSize", "overlap", "jaccard"))
    )
  }
}

checkResultsLinkouts <- function(resultsLinkouts) {
  checkList(resultsLinkouts)

  for (i in seq_along(resultsLinkouts)) {
    checkList(resultsLinkouts[[i]])
    for (j in seq_along(resultsLinkouts[[i]])) {
      stopifnot(is.character(resultsLinkouts[[i]][[j]]))
    }
  }

  return(NULL)
}
