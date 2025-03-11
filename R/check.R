checkNamingConvention <- function(featureObjectName, attr) {
   # Check study name, models, and tests
   forbidden <- c("^", ":", "*", "\\",  ">", "<", "$", "|", "?", "/")
   for (forbid in forbidden) {
     if(grepl(forbid, featureObjectName, fixed=TRUE)) {
       stop(sprintf("Error: Forbidden character detected in %s", attr))
     }
   }
   if (substr(featureObjectName, nchar(featureObjectName), nchar(featureObjectName)) == ".") {
     stop(sprintf("Error: %s cannot end in a period", attr))
   }
   return(featureObjectName)
}

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

  checkNamingConvention(name, "study name")

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


checkMaintainer <- function(maintainer) {
  if (is.null(maintainer)) return(NULL)
  stopifnot(
    is.character(maintainer),
    length(maintainer) == 1
  )
}

checkMaintainerEmail <- function(maintainerEmail) {
  if (is.null(maintainerEmail)) return(NULL)
  stopifnot(
    is.character(maintainerEmail),
    length(maintainerEmail) == 1
  )

  # Confirm email string is valid with a few sanity checks
  # https://stackoverflow.com/a/48170419
  if (!grepl("@", maintainerEmail)) {
    # The email address should contain at least one '@'
    stop(
      "A valid email address should contain at least one '@'\n",
      "Invalid maintainer email: ", maintainerEmail, "\n"
    )
  }
  if (grepl("@$", maintainerEmail)) {
    # The email address can't end with '@' (domain would be empty)
    stop(
      "Invalid maintainer email: ", maintainerEmail, "\n"
    )
  }
  emailParts <- strsplit(maintainerEmail, split = "@")[[1]]
  if (sum(nchar(emailParts[-length(emailParts)])) == 0) {
    # The string to the left of the rightmost '@' should be non-empty (local-part)
    stop(
      "Invalid maintainer email: ", maintainerEmail, "\n"
    )
  }
}

checkStudyMeta <- function(studyMeta) {
  checkList(studyMeta)

  if (isEmpty(studyMeta)) return(NULL)

  descriptionFieldsReservedFile <- system.file(
    "extdata/description-fields-reserved.txt",
    package = "OmicNavigator",
    mustWork = TRUE
  )
  descriptionFieldsReserved <- scan(
    file = descriptionFieldsReservedFile,
    what = character(),
    quiet = TRUE
  )

  for (i in seq_along(studyMeta)) {
    metaField <- names(studyMeta)[i]
    metaValue <- studyMeta[[i]]

    # Fields cannot be any of those reserved for R's DESCRIPTION field
    if (metaField %in% descriptionFieldsReserved) {
      stop(
        "studyMeta fields (ie the names of the list) cannot be named the same as any of the reserved fields for R's DESCRIPTION file.\n",
        "Problematic field: ", metaField, "\n",
        "To see the full list of reserved fields, run the following:\n",
        "browseURL(\"https://gist.github.com/jdblischak/f9d946327c9991fb57dde1e6f2bff1c2\")"
      )
    }

    # Fields cannot contain spaces or colons. They can't start with # or -
    # https://www.debian.org/doc/debian-policy/ch-controlfields.html#syntax-of-control-files
    if (grepl("[[:space:]]", metaField)) {
      stop(
        "studyMeta fields (ie the names of the list) cannot contain whitespace.\n",
        "Problematic field: ", metaField, "\n"
      )
    }
    if (grepl(":", metaField)) {
      stop(
        "studyMeta fields (ie the names of the list) cannot contain colons.\n",
        "Problematic field: ", metaField, "\n"
      )
    }
    if (grepl("^#", metaField)) {
      stop(
        "studyMeta fields (ie the names of the list) cannot start with a comment character.\n",
        "Problematic field: ", metaField, "\n"
      )
    }
    if (grepl("-", metaField)) {
      stop(
        "studyMeta fields (ie the names of the list) cannot start with a dash.\n",
        "Problematic field: ", metaField, "\n"
      )
    }

    # Values have to be length 1
    if (length(metaValue) != 1) {
      stop("studyMeta values must be single values.\n",
           "Problematic field: ", metaField, "\n",
           "Problematic value: ", paste(metaValue, collapse = ", "), "\n")
    }

  }
}

checkList <- function(x, allowEmpty = TRUE) {
  listName <- deparse(substitute(x))

  if (!is.list(x)) {
    stop(sprintf("The object \"%s\" must be a list", listName))
  }

  if (is.data.frame(x)) {
    stop(sprintf("The object \"%s\" must be a list, not a data frame", listName))
  }

  if (isEmpty(x)) {
    if (allowEmpty) {
      return(NULL)
    } else {
      stop("An empty list is not allowed in this context")
    }
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
    warnIfNonCharacterCols(features[[i]])
  }

  return(NULL)
}

checkModels <- function(models) {
  checkList(models)

  for (i in seq_along(models)) {
    model_name = names(models)[[i]]
    checkNamingConvention(model_name, "model name")
    # Accepts either a single string or a named list
    if (is.character(models[[i]]) && length(models[[i]]) == 1) {
      next
    }
    checkList(models[[i]], allowEmpty = FALSE)
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
    # All the columns must be numeric
    colsAllNum <- all(vapply(assays[[i]], is.numeric, logical(1)))
    if (!colsAllNum) {
      stop("The columns of the assays data frame must all be numeric.\n",
           sprintf("Problematic modelID: %s", names(assays)[i]))
    }
  }

  return(NULL)
}

checkTests <- function(tests) {
  checkList(tests)

  for (i in seq_along(tests)) {
    checkList(tests[[i]], allowEmpty = FALSE)
    for (j in seq_along(tests[[i]])) {
      test_name = names(tests[[i]])[[j]]
      checkNamingConvention(test_name, "test name")
      # Accepts either a single string or a named list
      if (is.character(tests[[i]][[j]]) && length(tests[[i]][[j]]) == 1) {
        next
      }
      checkList(tests[[i]][[j]], allowEmpty = FALSE)
    }
  }

  return(NULL)
}

checkAnnotations <- function(annotations) {
  checkList(annotations)

  for (i in seq_along(annotations)) {
    checkList(annotations[[i]], allowEmpty = FALSE)
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
    checkList(terms, allowEmpty = FALSE)
    if (!all(vapply(terms, is.na, logical(1)))) {
      stop(sprintf("The terms for \"%s\" must be a named list of character vectors",
                   annotationID))
    }
  }

  return(NULL)
}

checkResults <- function(results) {
  checkList(results)

  if ("default" %in% names(results)) {
    stop("The results cannot be shared using the modelID \"default\"")
  }

  for (i in seq_along(results)) {
    checkList(results[[i]], allowEmpty = FALSE)
    for (j in seq_along(results[[i]])) {
      dataFrame <- results[[i]][[j]]
      stopifnot(
        is.data.frame(dataFrame),
        is.character(dataFrame[[1]]),
        vapply(dataFrame[, -1], is.numeric, logical(1))
      )
      hasUniqueIdColumn(dataFrame)
    }
  }

  return(NULL)
}

checkEnrichments <- function(enrichments) {
  checkList(enrichments)

  if ("default" %in% names(enrichments)) {
    stop("The enrichments cannot be shared using the modelID \"default\"")
  }

  for (i in seq_along(enrichments)) {
    model <- enrichments[[i]]
    checkList(model, allowEmpty = FALSE)
    for (j in seq_along(model)) {
      annotation <- model[[j]]
      checkList(annotation, allowEmpty = FALSE)
      for (k in seq_along(annotation)) {
        test <- annotation[[k]]
        stopifnot(inherits(test, "data.frame"))
        stopifnot(c("termID", "description", "nominal", "adjusted")
                  %in% colnames(test))
        if ("nominal" %in% colnames(test) && !is.numeric(test$nominal)) {
          stop("Column 'nominal' from enrichments must be numeric")
        }
        if ("adjusted" %in% colnames(test) && !is.numeric(test$adjusted)) {
          stop("Column 'adjusted' from enrichments must be numeric")
        }
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

  # Can't name a custom plotting function the same name as any function in the
  # base namespace. When the plotting function is found via dynGet() based on
  # its name, the function in package:base is found first. This is not a problem
  # for any of the other packages on the search path.
  reservedNames <- ls("package:base")

  for (i in seq_along(plots)) {
    checkList(plots[[i]], allowEmpty = FALSE)
    for (j in seq_along(plots[[i]])) {
      plotEntry <- plots[[i]][[j]]
      checkList(plotEntry, allowEmpty = FALSE)
      plotID <- names(plots[[i]])[j]
      if (plotID %in% reservedNames) {
        stop(sprintf("Invalid plotID: \"%s\"\n", plotID),
             "You can't name the custom plotting using the same name as any function in package:base.\n",
             "Run ls(\"package:base\") to see the list of prohibited names\n")
      }
      plotFunction <- getPlotFunction(plotID)
      if (!is.function(plotFunction)) {
        stop(sprintf("Unable to find function \"%s\"", plotID))
      }
      # The custom plotting function will be passed an object to its first
      # argument. Thus none of the arguments can be required
      argsObserved <- formals(plotFunction)
      if (isEmpty(argsObserved)) {
        stop(
          sprintf("The custom plotting function \"%s\" has no arguments.\n", plotID),
          "There must be an argument for plotStudy() to pass the data to your function.\n"
        )
      }
      argsObservedRequired <- vapply(argsObserved, is.symbol, logical(1))
      if (any(argsObservedRequired[-1])) {
        stop(
          sprintf("The custom plotting function \"%s\" has invalid arguments.\n", plotID),
          "Only the first argument can be a required argument.",
          " The remaining arguments must have default values specified.\n"
        )
      }
      if (is.null(plotEntry[["displayName"]])) {
        stop(sprintf("Must define displayName for plot \"%s\"", plotID))
      }
    }
  }

  return(NULL)
}

checkMapping <- function(mapping) {
  checkList(mapping)

  for (i in seq_along(mapping)) {
    # stop if mapping object contains anything else besides data frames
    if (!inherits(mapping[[i]], "data.frame")) {
      stop("mapping object must be a list of at least one data frame")
    }
    # stop if mapping object has less than 2 models or 0 features
    if (!(ncol(mapping[[i]]) > 1 & nrow(mapping[[i]]) > 0)) {
      stop("mapping object requires at least two models and one feature")
    }
    # stop if mapping object has all NAs for a given model
    truth_array <- sapply(X = mapping[[i]], FUN = function(x) sum(is.na(x)) == nrow(mapping[[i]]))
    if(any(truth_array)) {
      stop("mapping object requires at least one feature per model")
    }
    # check if any given model has at least one feature aligned with another model
    mapping_na <- as.data.frame(!sapply(mapping[[i]], is.na),
                                stringsAsFactors = FALSE)

    for (ii in seq_along(mapping_na)) {
      tmpModel_indexFeatures  <- which(!is.na(mapping[[i]][,ii]))
      compModel_indexFeatures <- which(!is.na(mapping[[i]][,-ii]))
      featAligned <- any(tmpModel_indexFeatures %in% compModel_indexFeatures)

      if (!featAligned) {
        stop(sprintf("Model \"%s\" does not present any feature mapped to another model.", colnames(mapping[[i]])[ii]))
      }
    }
  }

  return(NULL)
}

checkBarcodes <- function(barcodes) {
  checkList(barcodes)

  for (i in seq_along(barcodes)) {
    barcode <- barcodes[[i]]
    checkList(barcode, allowEmpty = FALSE)
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
    checkList(resultsLinkouts[[i]], allowEmpty = FALSE)
    for (j in seq_along(resultsLinkouts[[i]])) {
      stopifnot(is.character(resultsLinkouts[[i]][[j]]))
    }
  }

  return(NULL)
}

checkEnrichmentsLinkouts <- function(enrichmentsLinkouts) {
  checkList(enrichmentsLinkouts)

  for (i in seq_along(enrichmentsLinkouts)) {
    stopifnot(is.character(enrichmentsLinkouts[[i]]))
  }

  return(NULL)
}

checkMetaFeaturesLinkouts <- function(metaFeaturesLinkouts) {
  checkList(metaFeaturesLinkouts)

  for (i in seq_along(metaFeaturesLinkouts)) {
    checkList(metaFeaturesLinkouts[[i]], allowEmpty = FALSE)
    for (j in seq_along(metaFeaturesLinkouts[[i]])) {
      stopifnot(is.character(metaFeaturesLinkouts[[i]][[j]]))
    }
  }

  return(NULL)
}

