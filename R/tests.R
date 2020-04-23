
testStudy <- function(name,
                      description = name,
                      featureID = "featureID",
                      sampleID = "sampleID", seed = 12345L)
{
  stopifnot(is.character(name), is.character(description), is.integer(seed))

  study <- createStudy(name = name,
                       description = description,
                       samples = testSamples(seed = seed),
                       features = testFeatures(seed = seed),
                       models = testModels(),
                       assays = testAssays(seed = seed),
                       contrasts = testContrasts(),
                       annotations = testAnnotations(seed = seed),
                       inferences = testInferences(seed = seed),
                       enrichments = testEnrichments(seed = seed),
                       metaFeatures = testMetaFeatures(seed = seed),
                       plots = testPlots(),
                       featureID = featureID,
                       sampleID = sampleID)

  return(study)
}

testSamples <- function(rows = 10, cols = 3, seed = 12345L) {
  set.seed(seed)
  samples <- matrix(sample(letters, size = rows * cols, replace = TRUE),
                    nrow = rows, ncol = cols)
  sampleID <- paste0("sample_", seq_len(rows))
  samples <- cbind(sampleID, samples)
  return(as.data.frame(samples))
}

testFeatures <- function(rows = 100, cols = 5, seed = 12345L) {
  set.seed(seed)
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

testAssays <- function(n = 3, rows = 100, cols = 10, seed = 12345L) {
  set.seed(seed)
  assays <- vector(mode = "list", length = n)
  names(assays) <- paste0("model_", seq_len(n))
  for (i in seq_len(n)) {
    assays[[i]] <- matrix(stats::rnorm(rows * cols), nrow = rows, ncol = cols)
    rownames(assays[[i]]) <- paste0("feature_", seq_len(rows))
    colnames(assays[[i]]) <- paste0("sample_", seq_len(cols))
  }
  return(assays)
}

testContrasts <- function(n = 2) {
  contrasts <- list()
  for (i in seq_len(n)) {
    name <- paste0("contrast_", i)
    value <- paste("contrast", i)
    contrasts[[name]] <- value
  }
  return(contrasts)
}

testAnnotations <- function(n = 3, terms = 10, featureID = "featureID", seed = 12345L) {
  set.seed(12345)
  annotations <- vector(mode = "list", length = n)
  names(annotations) <- paste0("annotation_", seq_len(n))
  universe <- paste0("feature_", seq_len(100))
  for (i in seq_len(n)) {
    terms_list <- replicate(terms,
                            sample(universe, size = stats::rpois(1, lambda = 15)),
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

testInferences <- function(n_models = 3, n_contrasts = 2, n_features = 100, seed = 12345L) {
  set.seed(seed)
  inferences <- vector(mode = "list", length = n_models)
  names(inferences) <- paste0("model_", seq_len(n_models))
  for (i in seq_len(n_models)) {
    inferences[[i]] <- vector(mode = "list", length = n_contrasts)
    names(inferences[[i]]) <- paste0("contrast_", seq_len(n_contrasts))
    for (j in seq_len(n_contrasts)) {
      inferences[[i]][[j]] <- data.frame(
        featureID = paste0("feature_", seq_len(n_features)),
        beta = sample(seq(-3, 3, by = 0.1), n_features, replace = TRUE),
        p_val = sample(seq(0.01, 0.99, by = 0.01), n_features, replace = TRUE),
        stringsAsFactors = FALSE
      )
    }
  }
  return(inferences)
}

testEnrichments <- function(n_models = 3, n_contrasts = 2, n_annotations = 3, seed = 12345L) {
  set.seed(seed)
  enrichments <- vector(mode = "list", length = n_models)
  names(enrichments) <- paste0("model_", seq_len(n_models))
  for (i in seq_len(n_models)) {
    enrichments[[i]] <- vector(mode = "list", length = n_contrasts)
    names(enrichments[[i]]) <- paste0("contrast_", seq_len(n_contrasts))
    for (j in seq_len(n_contrasts)) {
      enrichments[[i]][[j]] <- vector(mode = "list", length = n_annotations)
      names(enrichments[[i]][[j]]) <- paste0("annotation_", seq_len(n_annotations))
      for (k in seq_len(n_annotations)) {
        n_terms <- sample(3:5, 1)
        enrichments[[i]][[j]][[k]] <- data.frame(
          termID = paste0("term_", seq_len(n_terms)),
          p_val = sample(seq(0.01, 0.05, by = 0.01), n_terms, replace = TRUE),
          stringsAsFactors = FALSE
        )
      }
    }
  }
  return(enrichments)
}

testMetaFeatures<- function(seed = 12345L) return(NULL)

testPlots<- function() return(NULL)
