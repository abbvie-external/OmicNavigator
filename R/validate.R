
#' Validate a study
#'
#' @param study An OmicNavigator study object
#'
#' @return For a valid study object, the logical value \code{TRUE} is invisibly
#'   returned. For an invalid study object, there is no return value because an
#'   error is thrown.
#'
#' @export
validateStudy <- function(study) {
  checkStudy(study)

  emptyElements <- vapply(study, isEmpty, logical(1))
  elements <- names(study)[!emptyElements]
  for (e in elements) {
    checkFunctionName <- paste0("check", capitalize(e))
    checkFunction <- utils::getFromNamespace(checkFunctionName, ns = "OmicNavigator")
    checkFunction(study[[e]])
  }

  validateResults(study)
  validateEnrichments(study)
  validateEnrichmentsLinkouts(study)
  validatePlots(study)
  validateMapping(study)

  return(invisible(TRUE))
}

validateResults <- function(study) {
  results <- study[["results"]]

  if (isEmpty(results)) {
    stop("No results. A valid study requires at least one results table. Use addResults() to add one.")
  }

  for (i in seq_along(results)) {
    modelID <- names(results)[i]
    features <- getFeatures(study, modelID = modelID, quiet = TRUE)
    metaFeatures <- getMetaFeatures(study, modelID = modelID, quiet = TRUE)
    resultsLinkouts <- getResultsLinkouts(study, modelID = modelID, quiet = TRUE)
    metaFeaturesLinkouts <- getMetaFeaturesLinkouts(study, modelID = modelID, quiet = TRUE)

    # Send message if no common columns across tests. This will disable UpSet
    # filtering in app.
    upsetCols <- getUpsetCols(study, modelID)
    if (isEmpty(upsetCols)) {
      message(
        "Validation: ",
        sprintf("The results tables for the tests of modelID \"%s\" do not have any columns in common. ", modelID),
        "You will not be able to perform set analysis in the app. ",
        "If it makes sense for your study, please consider using shared column names."
      )
    }
    for (j in seq_along(results[[i]])) {
      testID <- names(results[[i]])[j]
      dataFrame <- results[[i]][[j]]

      # Validate agreement between results and features tables
      if (!isEmpty(features)) {
        # The first column must have the same name (it has the featureIDs)
        if (colnames(dataFrame)[1] != colnames(features)[1]) {
          stop("Name of featureID column doesn't match between results and features tables")
        }
        # The featureIDs in the results table should match the featureIDs in the
        # features table. Throw error if zero agreement. Send message if some
        # featureIDs in results are missing from features table.
        resultsInFeatures <- dataFrame[, 1] %in% features[, 1]
        if (sum(resultsInFeatures) == 0) {
          stop("The features in the results table do not match the featureID column in the features table\n",
               sprintf("modelID: %s, testID: %s", modelID, testID))
        }
        if (!all(resultsInFeatures)) {
          message("Validation: ",
                  "Some of the features in the results table are missing from the featureID column in the features table\n",
                  sprintf("modelID: %s, testID: %s", modelID, testID))
        }
        # The features and results table can only share one column name, the
        # feature ID column. Otherwise the merge will fail.
        sharedColumns <- intersect(colnames(dataFrame)[-1], colnames(features)[-1])
        if (!isEmpty(sharedColumns)) {
          stop("The results and features tables can only have one shared column name, the featureID column\n",
               sprintf("modelID: %s, testID: %s\n", modelID, testID),
               sprintf("Shared columns: %s", paste(sharedColumns, collapse = ", ")))
        }
      }

      # Validate agreement between results and metaFeatures tables
      if (!isEmpty(metaFeatures)) {
        # The featureIDs in the results table should match the featureIDs in the
        # metaFeatures table. Throw error if zero agreement. Send message if
        # some featureIDs in results are missing from metaFeatures table.
        resultsInMetaFeatures <- dataFrame[, 1] %in% metaFeatures[, 1]
        if (sum(resultsInMetaFeatures) == 0) {
          stop("The features in the results table do not match the featureID column in the metaFeatures table\n",
               sprintf("modelID: %s, testID: %s", modelID, testID))
        }
        if (!all(resultsInMetaFeatures)) {
          message("Validation: ",
                  "Some of the features in the results table are missing from the featureID column in the metaFeatures table\n",
                  sprintf("modelID: %s, testID: %s", modelID, testID))
        }
        # Validate metaFeaturesLinkouts target metaFeatures columns
        if (!isEmpty(metaFeaturesLinkouts)) {
          for (k in seq_along(metaFeaturesLinkouts)) {
            columnName <- names(metaFeaturesLinkouts)[k]
            if (!columnName %in% colnames(metaFeatures)) {
              stop(sprintf("Invalid metaFeatures table linkout for modelID \"%s\"\n", modelID),
                   sprintf("\"%s\" is not the name of an available metaFeature", columnName))
            }
          }
        }
      }

      # Validate agreement between results and linkouts
      if (!isEmpty(resultsLinkouts)) {
        resultsTable <- getResultsTable(study, modelID, testID)
        for (k in seq_along(resultsLinkouts)) {
          columnName <- names(resultsLinkouts)[k]
          if (!columnName %in% colnames(resultsTable)) {
            stop(sprintf("Invalid results table linkout for modelID \"%s\"\n", modelID),
                 sprintf("\"%s\" is not the name of an available feature", columnName))
          }
        }
      }

    } # end of inner loop through testIDs
  } # end of outer loop through modelIDs

  return(invisible(TRUE))
}

validateEnrichments <- function(study) {
  enrichments <- study[["enrichments"]]

  # Enrichments aren't required
  if (isEmpty(enrichments)) return(invisible(NA))

  results <- getResults(study)
  annotations <- getAnnotations(study, quiet = TRUE)
  if (isEmpty(annotations)) {
    message("Validation: The network view will be disabled in the app because the study has no annotations.",
            " Use addAnnotations() to add them.")
  }
  for (i in seq_along(enrichments)) {
    modelID <- names(enrichments)[i]

    # Send message if modelID isn't present in results
    if (!modelID %in% names(results)) {
      message("Validation: ",
              sprintf("The modelID \"%s\" has enrichments but no results. ", modelID),
              "The barcode view in the app will be disabled. Use addResults() to add them.")
    }

    for (j in seq_along(enrichments[[i]])) {
      annotationID <- names(enrichments[[i]])[j]
      # The network view requires annotation data
      if (!isEmpty(annotations) && !annotationID %in% names(annotations)) {
        message(sprintf("The modelID \"%s\" has enrichments for the annotationID \"%s\" but not the annotation terms. ",
                        modelID, annotationID),
                "The network view will be disabled in the app. Use addAnnotations() to add them.")
      }

      for (k in seq_along(enrichments[[i]][[j]])) {
        testID <- names(enrichments[[i]][[j]])[k]
        dataFrame <- enrichments[[i]][[j]][[k]]

        # If annotations were provided...
        if (!isEmpty(annotations[[annotationID]])) {
          # All the terms in the enrichments table should be in the list of
          # terms.
          enrichmentsInTerms <- dataFrame[, 1] %in% names(annotations[[annotationID]][["terms"]])
          if (!all(enrichmentsInTerms)) {
            stop("All the termIDs in the enrichments table must be in the corresponding list of terms for the annotation.\n",
                 sprintf("modelID: %s, annotationID: %s, testID: %s", modelID, annotationID, testID))
          }
          # The features in the enriched terms should match the features in the
          # results table.
          terms <- annotations[[annotationID]][["terms"]]
          termsEnriched <- terms[names(terms) %in% dataFrame[, 1]]
          termsFeatures <- unlist(termsEnriched)
          resultsTable <- getResultsTable(study, modelID, testID)
          termsFeatureID <- annotations[[annotationID]][["featureID"]]
          resultsTableFeatures <- resultsTable[[termsFeatureID]]
          resultsTableFeaturesInTerms <- resultsTableFeatures %in% termsFeatures
          if (sum(resultsTableFeaturesInTerms) == 0) {
            stop("The features in the column \"%s\" do not match the featureIDs in the annotation terms.\n",
                 "You can specify the column with the corresponding featureIDs with addAnnotations().\n",
                 sprintf("modelID: %s, annotationID: %s, testID: %s", modelID, annotationID, testID))
          }
        }

        # If barcodes were provided...
        barcodes <- getBarcodes(study, modelID, quiet = TRUE) # have to query since can use "default"
        if (!isEmpty(barcodes)) {
          resultsTable <- getResultsTable(study, modelID, testID)
          # `statistic` must be a column in the results table
          if (!barcodes[["statistic"]] %in% colnames(resultsTable)) {
            stop(sprintf("The barcode statistic \"%s\" is not in the results table.\n",
                         barcodes[["statistic"]]),
                 sprintf("modelID: %s, annotationID: %s, testID: %s",
                         modelID, annotationID, testID))
          }
          # `logFoldChange` must be a column in the results table
          if (!is.null(barcodes[["logFoldChange"]]) &&
              !barcodes[["logFoldChange"]] %in% colnames(resultsTable)) {
            stop(sprintf("The barcode logFoldChange \"%s\" is not in the results table.\n",
                         barcodes[["logFoldChange"]]),
                 sprintf("modelID: %s, annotationID: %s, testID: %s",
                         modelID, annotationID, testID))
          }
          # `featureDisplay` must be a column in the results table
          if (!is.null(barcodes[["featureDisplay"]]) &&
              !barcodes[["featureDisplay"]] %in% colnames(resultsTable)) {
            stop(sprintf("The barcode featureDisplay \"%s\" is not in the results table.\n",
                         barcodes[["featureDisplay"]]),
                 sprintf("modelID: %s, annotationID: %s, testID: %s",
                         modelID, annotationID, testID))
          }
        }

      } # inner-most loop through testIDs
    } # inner loop through annotationIDs
  } # outer loop through modelIDs

  return(invisible(TRUE))
}

# Validate that enrichments table linkouts only include available annotationIDs
validateEnrichmentsLinkouts <- function(study) {
  enrichmentsLinkouts <- study[["enrichmentsLinkouts"]]

  if (isEmpty(enrichmentsLinkouts)) return(invisible(NA))

  enrichments <- getEnrichments(study, quiet = TRUE)
  annotationIDs <- lapply(enrichments, names)
  annotationIDs <- unique(unlist(annotationIDs))

  for (i in seq_along(enrichmentsLinkouts)) {
    annotationID <- names(enrichmentsLinkouts)[i]
    if (!annotationID %in% annotationIDs) {
      stop("Invalid enrichments table linkout\n",
           sprintf("The annotationID \"%s\" is not an available annotation.\n",
                   annotationID),
           "You can only add linkouts with addEnrichmentsLinkouts() for\n",
           "annotationIDs that have been added for at least one model with addEnrichments()")
    }
  }

  return(invisible(TRUE))
}

validatePlots <- function(study) {
  plots <- study[["plots"]]

  # Plots aren't required
  if (isEmpty(plots)) return(NA)

  models <- names(study[["results"]])
  for (i in seq_along(models)) {
    modelID <- models[i]
    modelPlots <- getPlots(study, modelID, quiet = TRUE)
    if (isEmpty(modelPlots)) next
    # Custom plots no longer require assays, since they can plot columns from
    # the results table. If assays are unavailable, send a message and then
    # skip the rest of the validation between assays with
    # samples/features/results
    assays <- getAssays(study, modelID, quiet = TRUE)
    if (isEmpty(assays)) {
      message(sprintf("Custom plots often use assays. Missing assays for modelID \"%s\"",
                      modelID))
      next
    }

    # featureID column of results must be row names of assays
    tests <- names(study[["results"]][[modelID]])
    rows <- row.names(assays)
    for (j in seq_along(tests)) {
      testID <- tests[j]
      results <- getResults(study, modelID, testID)
      resultsFeaturesInAssaysRows <- results[, 1] %in% rows
      if (sum(resultsFeaturesInAssaysRows) == 0) {
        stop("The featureID column in the results table does not match the row names of the assays table\n",
             sprintf("modelID: %s, testID: %s", modelID, testID))
      }
      if (!all(resultsFeaturesInAssaysRows)) {
        stop("Some of the featureIDs in the results table are missing from the row names of the assays table\n",
             sprintf("modelID: %s, testID: %s", modelID, testID))
      }
    } # inner loop of testIDs

    # Validate concordance between assays and samples
    samples <- getSamples(study, modelID, quiet = TRUE)
    if (isEmpty(samples)) {
      message(sprintf("Custom plots often use samples. Missing samples for modelID \"%s\"",
                      modelID))
      next
    }
    # Column names of assays must be in first column of samples table
    cols <- colnames(assays)
    colsInSamples <- cols %in% samples[, 1]
    if (sum(colsInSamples) == 0) {
      stop("The column names of the assays table do not match the sampleID column in the samples table\n",
           sprintf("modelID: %s", modelID))
    }
    if (!all(colsInSamples)) {
      stop("Some of the column names of the assays table are missing from the sampleID column in the samples table\n",
           sprintf("modelID: %s", modelID))
    }

  } # outer loop of modelIDs

  return(invisible(TRUE))
}

validateMapping <- function(study) {
  mapping <- study[["mapping"]]

  # Mapping isn't required
  if (isEmpty(mapping)) return(NA)

  results <- getResults(study)
  models  <- names(study[["results"]])
  for (i in seq_along(mapping)) {
    for (ii in seq_along(mapping[[i]])) {
      # Check whether mapping names match model names from results table or 'default'
      mappingID <- names(mapping[[i]][ii])
      if (!mappingID %in% c(models, "default")) {
        stop("At least one mapping name is not named as 'default' nor does match any model name from results table.\n",
             "Shared mapping is required to be named as 'default'.",
             sprintf("mappingID: %s", mappingID))
      }
      # Check whether mapping features match results features
      mappingFeatures <- mapping[[i]][!is.na(mapping[[i]][,ii]),ii]
      modelFeatures   <- results[[grep(paste0("^",mappingID,"$"), models)]][1][[1]][,1]
      if (!length(intersect(mappingFeatures, modelFeatures)) > 0) {
        stop("Mapping features for modelID do not match features from modelID results table\n",
             sprintf("modelID: %s", mappingID))
      }
    }
  }
  return(invisible(TRUE))
}

