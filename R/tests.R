
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
                       tests = testTests(),
                       annotations = testAnnotations(seed = seed),
                       results = testResults(seed = seed),
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
  sampleID <- sprintf("sample_%04d", seq_len(rows))
  samples <- cbind(sampleID, samples)
  return(as.data.frame(samples))
}

testFeatures <- function(rows = 100, cols = 5, seed = 12345L) {
  set.seed(seed)
  features <- matrix(sample(letters, size = rows * cols, replace = TRUE),
                    nrow = rows, ncol = cols)
  featureID <- sprintf("feature_%04d", seq_len(rows))
  features <- cbind(featureID, features)
  return(as.data.frame(features))
}

testModels <- function(n = 3) {
  models <- list()
  for (i in seq_len(n)) {
    name <- sprintf("model_%02d", i)
    value <- paste("Model", i)
    models[[name]] <- value
  }
  return(models)
}

testAssays <- function(n = 3, rows = 100, cols = 10, seed = 12345L) {
  set.seed(seed)
  assays <- vector(mode = "list", length = n)
  names(assays) <- sprintf("model_%02d", seq_len(n))
  for (i in seq_len(n)) {
    assays[[i]] <- matrix(stats::rnorm(rows * cols), nrow = rows, ncol = cols)
    rownames(assays[[i]]) <- sprintf("feature_%04d", seq_len(rows))
    colnames(assays[[i]]) <- sprintf("sample_%04d", seq_len(cols))
  }
  return(assays)
}

testTests <- function(n = 2) {
  tests <- list()
  for (i in seq_len(n)) {
    name <- sprintf("test_%02d", i)
    value <- paste("test", i)
    tests[[name]] <- value
  }
  return(tests)
}

testAnnotations <- function(n = 3, terms = 10, featureID = "featureID", seed = 12345L) {
  set.seed(12345)
  annotations <- vector(mode = "list", length = n)
  names(annotations) <- sprintf("annotation_%02d", seq_len(n))
  universe <- sprintf("feature_%04d", seq_len(100))
  for (i in seq_len(n)) {
    terms_list <- replicate(terms,
                            sample(universe, size = stats::rpois(1, lambda = 15)),
                            simplify = FALSE)
    names(terms_list) <- sprintf("term_%02d", seq_len(terms))
    annotations[[i]] <- list(
      terms = terms_list,
      description = sprintf("Terms from %s", names(annotations)[i]),
      featureID = featureID
    )
  }
  return(annotations)
}

testResults <- function(n_models = 3, n_tests = 2, n_features = 100, seed = 12345L) {
  set.seed(seed)
  results <- vector(mode = "list", length = n_models)
  names(results) <- sprintf("model_%02d", seq_len(n_models))
  for (i in seq_len(n_models)) {
    results[[i]] <- vector(mode = "list", length = n_tests)
    names(results[[i]]) <- sprintf("test_%02d", seq_len(n_tests))
    for (j in seq_len(n_tests)) {
      results[[i]][[j]] <- data.frame(
        featureID = sprintf("feature_%04d", seq_len(n_features)),
        beta = sample(seq(-3, 3, by = 0.1), n_features, replace = TRUE),
        p_val = sample(seq(0.01, 0.99, by = 0.01), n_features, replace = TRUE),
        stringsAsFactors = FALSE
      )
    }
  }
  return(results)
}

testEnrichments <- function(n_models = 3, n_tests = 2, n_annotations = 3, seed = 12345L) {
  set.seed(seed)
  enrichments <- vector(mode = "list", length = n_models)
  names(enrichments) <- sprintf("model_%02d", seq_len(n_models))
  for (i in seq_len(n_models)) {
    enrichments[[i]] <- vector(mode = "list", length = n_tests)
    names(enrichments[[i]]) <- sprintf("test_%02d", seq_len(n_tests))
    for (j in seq_len(n_tests)) {
      enrichments[[i]][[j]] <- vector(mode = "list", length = n_annotations)
      names(enrichments[[i]][[j]]) <- sprintf("annotation_%02d", seq_len(n_annotations))
      for (k in seq_len(n_annotations)) {
        n_terms <- sample(3:5, 1)
        tmp <- data.frame(
          termID = sprintf("term_%02d", seq_len(n_terms)),
          nominal = sample(seq(0.01, 0.05, by = 0.01), n_terms, replace = TRUE),
          stringsAsFactors = FALSE
        )
        tmp[["description"]] <- sprintf("Description of %s", tmp[["termID"]])
        tmp[["adjusted"]] <- tmp[["nominal"]] + 0.02
        tmp <- tmp[, c("termID", "description", "nominal", "adjusted")]
        enrichments[[i]][[j]][[k]] <- tmp
      }
    }
  }
  return(enrichments)
}

testMetaFeatures<- function(seed = 12345L) return(NULL)

testPlots<- function() return(NULL)
