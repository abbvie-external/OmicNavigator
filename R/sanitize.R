# Sanitize the input to OmicNavigator addX() functions

sanitizeSamples <- function(samples) {
  for (i in seq_along(samples)) {
    samples[[i]] <- as.data.frame(samples[[i]])
  }

  return(samples)
}

sanitizeFeatures <- function(features) {
  for (i in seq_along(features)) {
    features[[i]] <- as.data.frame(features[[i]])
  }

  features <- lapply(features, coerceColsToCharacter)

  return(features)
}

sanitizeModels <- function(models) {
  return(models)
}

sanitizeAssays <- function(assays) {
  for (i in seq_along(assays)) {
    if (isList(assays[[i]])) {
      # support multiple transformations
      for (j in seq_along(assays[[i]])) {
        assays[[i]][[j]] <- as.data.frame(assays[[i]][[j]])
      }
    } else {
      assays[[i]] <- as.data.frame(assays[[i]])
    }
  }

  return(assays)
}

sanitizeTests <- function(tests) {
  return(tests)
}

sanitizeAnnotations <- function(annotations) {
  return(annotations)
}

sanitizeResults <- function(results) {
  for (i in seq_along(results)) {
    for (j in seq_along(results[[i]])) {
      results[[i]][[j]] <- as.data.frame(results[[i]][[j]])
    }
  }

  return(results)
}

sanitizeEnrichments <- function(enrichments) {
  for (i in seq_along(enrichments)) {
    for (j in seq_along(enrichments[[i]])) {
      for (k in seq_along(enrichments[[i]][[j]])) {
        theTable <- enrichments[[i]][[j]][[k]]
        theTable <- as.data.frame(theTable)
        columnsCurrent <- names(theTable)
        columnsToKeep <- c("termID", "description", "nominal", "adjusted")
        columnsToRemove <- !columnsCurrent %in% columnsToKeep
        if (any(columnsToRemove)) {
          warning(
            "The following columns were removed from the enrichments table: ",
            paste(columnsCurrent[columnsToRemove], collapse = ", ")
          )
        }
        theTable <- theTable[, columnsToKeep]
        enrichments[[i]][[j]][[k]] <- theTable
      }
    }
  }

  return(enrichments)
}

sanitizeMetaFeatures <- function(metaFeatures, study = NULL) {
  for (i in seq_along(metaFeatures)) {
    metaFeatures[[i]] <- as.data.frame(metaFeatures[[i]])
  }

  metaFeatures <- lapply(metaFeatures, coerceColsToCharacter)

  return(metaFeatures)
}

sanitizePlots <- function(plots) {
  return(plots)
}

sanitizeMapping <- function(mapping) {
  return(mapping)
}

sanitizeBarcodes <- function(barcodes) {
  return(barcodes)
}

sanitizeReports <- function(reports) {
  return(reports)
}

sanitizeOverlaps <- function(overlaps) {
  for (i in seq_along(overlaps)) {
    overlaps[[i]] <- as.data.frame(overlaps[[i]])
  }

  return(overlaps)
}

sanitizeResultsLinkouts <- function(resultsLinkouts) {
  return(resultsLinkouts)
}

sanitizeEnrichmentsLinkouts <- function(enrichmentsLinkouts) {
  return(enrichmentsLinkouts)
}

sanitizeMetaFeaturesLinkouts <- function(metaFeaturesLinkouts) {
  return(metaFeaturesLinkouts)
}

sanitizeMetaAssays <- function(metaAssays) {
  for (i in seq_along(metaAssays)) {
    if (isList(metaAssays[[i]])) {
      # support multiple transformations
      for (j in seq_along(metaAssays[[i]])) {
        metaAssays[[i]][[j]] <- as.data.frame(metaAssays[[i]][[j]])
      }
    } else {
      metaAssays[[i]] <- as.data.frame(metaAssays[[i]])
    }
  }

  return(metaAssays)
}

sanitizeObjects <- function(objects) {
  return(objects)
}
