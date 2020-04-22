
testStudy <- function(name,
                      description = name,
                      featureID = "featureID",
                      sampleID = "sampleID")
{
  stopifnot(is.character(name), is.character(description))

  study <- createStudy(name = name,
                       description = description,
                       samples = testSamples(),
                       features = testFeatures(),
                       models = testModels(),
                       assays = testAssays(),
                       contrasts = testContrasts(),
                       annotations = testAnnotations(),
                       inferences = testInferences(),
                       enrichments = testEnrichments(),
                       metaFeatures = testMetaFeatures(),
                       plots = testPlots(),
                       featureID = featureID,
                       sampleID = sampleID)

  return(study)
}

testSamples <- function(rows = 10, cols = 3) {
  samples <- matrix(sample(letters, size = rows * cols, replace = TRUE),
                    nrow = rows, ncol = cols)
  sampleID <- paste0("sample_", seq_len(rows))
  samples <- cbind(sampleID, samples)
  return(as.data.frame(samples))
}

testFeatures <- function(rows = 100, cols = 5) {
  features <- matrix(sample(letters, size = rows * cols, replace = TRUE),
                    nrow = rows, ncol = cols)
  featureID <- paste0("feature_", seq_len(rows))
  features <- cbind(featureID, features)
  return(as.data.frame(features))
}

testModels <- function(n = 3) {
  models <- list()
  for (i in seq_len(n)) {
    name <- paste0("model_", i)
    value <- paste("Model", i)
    models[[name]] <- value
  }
  return(models)
}

testAssays<- function(n = 3, rows = 100, cols = 10) {
  assays <- vector(mode = "list", length = n)
  names(assays) <- paste0("model_", seq_len(n))
  for (i in seq_len(n)) {
    assays[[i]] <- matrix(rnorm(rows * cols), nrow = rows, ncol = cols)
    rownames(assays[[i]]) <- paste0("feature_", seq_len(rows))
    colnames(assays[[i]]) <- paste0("sample_", seq_len(cols))
  }
  return(assays)
}

testContrasts<- function(n = 2) {
  contrasts <- list()
  for (i in seq_len(n)) {
    name <- paste0("contrast_", i)
    value <- paste("contrast", i)
    contrasts[[name]] <- value
  }
  return(contrasts)
}

testAnnotations<- function(n = 3, terms = 10, featureID = "featureID") {
  annotations <- vector(mode = "list", length = n)
  names(annotations) <- paste0("annotation_", seq_len(n))
  universe <- paste0("feature_", seq_len(100))
  for (i in seq_len(n)) {
    terms_list <- replicate(terms,
                            sample(universe, size = rpois(1, lambda = 15)),
                            simplify = FALSE)
    names(terms_list) <- paste0("term_", seq_len(terms))
    annotations[[i]] <- list(
      terms = terms_list,
      description = sprintf("Terms from %s", names(annotations)[i]),
      featureID = featureID
    )
  }
  return(annotations)
}

testInferences<- function() return(NULL)

testEnrichments<- function() return(NULL)

testMetaFeatures<- function() return(NULL)

testPlots<- function() return(NULL)
