
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

testAssays<- function() return(NULL)
contrasts = testContrasts<- function() return(NULL)
annotations = testAnnotations<- function() return(NULL)
inferences = testInferences<- function() return(NULL)
enrichments = testEnrichments<- function() return(NULL)
metaFeatures = testMetaFeatures<- function() return(NULL)
plots = testPlots<- function() return(NULL)
