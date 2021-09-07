
testStudy <- function(name,
                      description = name,
                      version = NULL,
                      maintainer = NULL,
                      maintainerEmail = NULL,
                      seed = 12345L,
                      numericFeatureID = FALSE,
                      nFeatures = 100)
{
  stopifnot(is.character(name), is.character(description), is.integer(seed))

  study <- createStudy(name = name,
                       description = description,
                       samples = testSamples(seed = seed),
                       features = testFeatures(seed = seed, rows = nFeatures,
                                               numericFeatureID = numericFeatureID),
                       models = testModels(),
                       assays = testAssays(seed = seed, rows = nFeatures,
                                           numericFeatureID = numericFeatureID),
                       tests = testTests(),
                       annotations = testAnnotations(seed = seed, nFeatures = nFeatures,
                                                     numericFeatureID = numericFeatureID),
                       results = testResults(seed = seed, nFeatures = nFeatures,
                                             numericFeatureID = numericFeatureID),
                       enrichments = testEnrichments(seed = seed),
                       metaFeatures = testMetaFeatures(seed = seed, rows = nFeatures,
                                                       numericFeatureID = numericFeatureID),
                       plots = list(),
                       barcodes = testBarcodes(),
                       reports = testReports(),
                       resultsLinkouts = testResultsLinkouts(),
                       enrichmentsLinkouts = testEnrichmentsLinkouts(),
                       metaFeaturesLinkouts = testMetaFeaturesLinkouts(),
                       version = version,
                       maintainer = maintainer,
                       maintainerEmail = maintainerEmail,
                       studyMeta = testStudyMeta())

  return(study)
}

testSamples <- function(rows = 10, cols = 5, seed = 12345L) {
  set.seed(seed)
  samples <- matrix(sample(letters, size = rows * cols, replace = TRUE),
                    nrow = rows, ncol = cols)
  colnames(samples) <- sprintf("sampleVar%02d", seq_len(cols))
  sampleID <- sprintf("sample_%04d", seq_len(rows))
  samples <- cbind(sampleID, samples)
  samples <- as.data.frame(samples, stringsAsFactors = FALSE)
  samples <- list(default = samples)
  return(samples)
}

testFeatures <- function(rows = 100, cols = 5, seed = 12345L, numericFeatureID = FALSE) {
  set.seed(seed)
  features <- matrix(sample(letters, size = rows * (cols - 2), replace = TRUE),
                    nrow = rows, ncol = cols - 2)
  colnames(features) <- sprintf("featureVar%02d", seq_len(cols - 2))
  featureVarNumeric <- sample(1:100, size = rows, replace = TRUE)
  features <- cbind(featureVarNumeric, features)
  if (numericFeatureID) {
    featureID <- sprintf("%04d", seq_len(rows))
  } else {
    featureID <- sprintf("feature_%04d", seq_len(rows))
  }
  features <- cbind(customID = featureID, features)
  features <- as.data.frame(features, stringsAsFactors = FALSE)
  features <- list(default = features)
  return(features)
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

testAssays <- function(n = 3, rows = 100, cols = 10, seed = 12345L, numericFeatureID = FALSE) {
  set.seed(seed)
  assays <- vector(mode = "list", length = n)
  names(assays) <- sprintf("model_%02d", seq_len(n))
  if (numericFeatureID) {
    featureID <- sprintf("%04d", seq_len(rows))
  } else {
    featureID <- sprintf("feature_%04d", seq_len(rows))
  }
  sampleID <- sprintf("sample_%04d", seq_len(cols))
  for (i in seq_len(n)) {
    assays[[i]] <- matrix(sample(seq(-2, 2, by = 0.0001), size = rows * cols,
                                 replace = TRUE),
                          nrow = rows, ncol = cols)
    rownames(assays[[i]]) <- featureID
    colnames(assays[[i]]) <- sampleID
    assays[[i]] <- as.data.frame(assays[[i]])
  }
  return(assays)
}

testTests <- function(n = 2) {
  tests <- sprintf("test %d", seq_len(n))
  tests <- as.list(tests)
  names(tests) <- sprintf("test_%02d", seq_len(n))
  tests <- list(default = tests)
  return(tests)
}

testAnnotations <- function(n = 3, terms = 50, featureID = "customID", seed = 12345L,
                            numericFeatureID = FALSE, nFeatures = 100) {
  set.seed(12345)
  annotations <- vector(mode = "list", length = n)
  names(annotations) <- sprintf("annotation_%02d", seq_len(n))
  if (numericFeatureID) {
    universe <- sprintf("%04d", seq_len(nFeatures))
  } else {
    universe <- sprintf("feature_%04d", seq_len(nFeatures))
  }
  for (i in seq_len(n)) {
    terms_list <- replicate(terms,
                            sample(universe, size = sample(5:25, size = 1, replace = TRUE)),
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

testResults <- function(nModels = 3, nTests = 2, nFeatures = 100, seed = 12345L,
                        numericFeatureID = FALSE) {
  set.seed(seed)
  results <- vector(mode = "list", length = nModels)
  names(results) <- sprintf("model_%02d", seq_len(nModels))
  if (numericFeatureID) {
    featureID <- sprintf("%04d", seq_len(nFeatures))
  } else {
    featureID <- sprintf("feature_%04d", seq_len(nFeatures))
  }
  for (i in seq_len(nModels)) {
    results[[i]] <- vector(mode = "list", length = nTests)
    names(results[[i]]) <- sprintf("test_%02d", seq_len(nTests))
    for (j in seq_len(nTests)) {
      beta <- stats::rnorm(n = nFeatures, sd = 1.5)
      beta_x <- stats::rnorm(n = nFeatures, sd = 1.5)
      # Calculate the p-value from the beta so that they are concordant,
      # otherwise UpSet filters often don't make sense.
      beta_se <- stats::runif(nFeatures, min = 0.90, max = 1.10)
      z <- beta / beta_se
      p_val <- stats::pnorm(-abs(z)) * 2
      tmpResults <- data.frame(
        customID = featureID,
        beta = beta,
        beta_x = beta_x,
        p_val = p_val,
        stringsAsFactors = FALSE
      )
      tmpResults <- tmpResults[order(tmpResults[["p_val"]]), ]
      row.names(tmpResults) <- seq_len(nrow(tmpResults))
      results[[i]][[j]] <- tmpResults
      # Give beta_x a test-specific name
      colnames(results[[i]][[j]])[3] <- paste0("beta_", j)
      # For a very thorough testing of tibble input:
      # class(results[[i]][[j]]) <- c("tbl_df", "tbl", "data.frame")
      # For a very thorough testing of data.table input:
      # class(results[[i]][[j]]) <- c("data.table", "data.frame")
    }
  }
  return(results)
}

testEnrichments <- function(nModels = 3, nAnnotations = 3, nTests = 2, terms = 50, seed = 12345L) {
  set.seed(seed)
  enrichments <- vector(mode = "list", length = nModels)
  names(enrichments) <- sprintf("model_%02d", seq_len(nModels))
  for (i in seq_len(nModels)) {
    enrichments[[i]] <- vector(mode = "list", length = nAnnotations)
    names(enrichments[[i]]) <- sprintf("annotation_%02d", seq_len(nAnnotations))
    for (j in seq_len(nAnnotations)) {
      enrichments[[i]][[j]] <- vector(mode = "list", length = nTests)
      names(enrichments[[i]][[j]]) <- sprintf("test_%02d", seq_len(nTests))
      for (k in seq_len(nTests)) {
        tmp <- data.frame(
          termID = sprintf("term_%02d", seq_len(terms)),
          nominal = sample(seq(0.01, 0.1, by = 0.01), terms, replace = TRUE),
          stringsAsFactors = FALSE
        )
        tmp[["description"]] <- sprintf("Description of %s", tmp[["termID"]])
        tmp[["adjusted"]] <- tmp[["nominal"]] + 0.02
        tmp <- tmp[, c("termID", "description", "nominal", "adjusted")]
        enrichments[[i]][[j]][[k]] <- tmp
        # For a very thorough testing of tibble input:
        # class(enrichments[[i]][[j]][[k]]) <- c("tbl_df", "tbl", "data.frame")
        # For a very thorough testing of data.table input:
        # class(enrichments[[i]][[j]][[k]]) <- c("data.table", "data.frame")
      }
    }
  }
  return(enrichments)
}

# Assigns 3 metaFeatures to each feature
testMetaFeatures <- function(rows = 100, cols = 3, seed = 12345L,
                             numericFeatureID = FALSE) {
  set.seed(seed)
  metaFeatures <- matrix(sample(letters, size = 3 * rows * cols, replace = TRUE),
                     nrow = 3 * rows, ncol = cols)
  colnames(metaFeatures) <- sprintf("metaFeatureVar%02d", seq_len(cols))
  metaFeatureVarNumeric <- sample(seq(3 * rows), size = rows, replace = TRUE)
  metaFeatures <- cbind(metaFeatureVarNumeric, metaFeatures)
  if (numericFeatureID) {
    featureID <- rep(sprintf("%04d", seq_len(rows)), times = 3)
  } else {
    featureID <- rep(sprintf("feature_%04d", seq_len(rows)), times = 3)
  }
  metaFeatureID <- sprintf("metaFeature_%04d", seq_len(3 * rows))
  metaFeatures <- cbind(customID = featureID, metaFeatureID, metaFeatures)
  metaFeatures <- as.data.frame(metaFeatures, stringsAsFactors = FALSE)
  metaFeatures <- list(default = metaFeatures)
  return(metaFeatures)
}

testPlots <- function() {
  plotBase <- function(x) {
    plotPoints <- as.numeric(x[["assays"]][1, ])
    plotTitle <- sprintf("Feature %s (ID: %s)",
                         x[["features"]][1, "featureVar01"],
                         x[["features"]][1, "customID"])
    plotLabels <- x[["samples"]][["sampleVar01"]]
    graphics::par(cex.main = 2)
    graphics::plot(x = plotPoints,
                   main = plotTitle,
                   xlab = "Samples",
                   ylab = "Expression level")
    graphics::text(x = plotPoints, labels = plotLabels, pos = 4)
  }
  assign("plotBase", plotBase, envir = parent.frame())
  plotGg <- function(x) {
    plotPoints <- as.numeric(x[["assays"]][1, ])
    featureMedian <- stats::median(plotPoints)
    plotTitle <- sprintf("%s, median: %0.2f", x[["features"]][["customID"]],
                         featureMedian)
    ggplot2::qplot(seq_along(plotPoints), plotPoints, main = plotTitle,
                   xlab = "Samples", ylab = "Expression level")
  }
  assign("plotGg", plotGg, envir = parent.frame())
  plotMultiFeature <- function(x) {
    if (nrow(x[["assays"]]) < 2) {
      stop("This plotting function requires at least 2 features")
    }
    pca <- stats::prcomp(t(x[["assays"]]), scale. = TRUE)$x
    graphics::plot(pca[, 1], pca[, 2], col = as.factor(x$samples$sampleVar01),
                   xlab = "PC 1", ylab = "PC 2", main = "PCA")
  }
  assign("plotMultiFeature", plotMultiFeature, envir = parent.frame())
  plotMultiTest_sf <- function(x) {
    var_x <- data.frame(lapply(x$results, `[`, 2))
    colnames(var_x)<- names(x$results)
    var_x <- data.table::data.table(features = rownames(var_x), var_x)
    var_x <- data.table::melt(var_x, measure.vars = c(2,3))

    var_y <- data.frame(lapply(x$results, `[`, 3))
    colnames(var_y) <- names(x$results)
    var_y <- data.table::data.table(features = rownames(var_y), var_y)
    var_y <- data.table::melt(var_y, measure.vars = c(2,3))

    df <- merge(var_x, var_y, by=c("variable", "features"))

    plot(df$value.x ~ df$value.y, col = factor(df$variable))
  }
  assign("plotMultiTest_sf", plotMultiTest_sf, envir = parent.frame())
  plotMultiTest_mf <- function(x) {
    var_x <- data.frame(lapply(x$results, `[`, 2))
    colnames(var_x)<- names(x$results)
    var_x <- data.table::data.table(features = rownames(var_x), var_x)
    var_x <- data.table::melt(var_x, measure.vars = c(2,3))

    var_y <- data.frame(lapply(x$results, `[`, 3))
    colnames(var_y) <- names(x$results)
    var_y <- data.table::data.table(features = rownames(var_y), var_y)
    var_y <- data.table::melt(var_y, measure.vars = c(2,3))

    df <- merge(var_x, var_y, by=c("variable", "features"))

    plot(df$value.x ~ df$value.y, col = factor(df$variable))
  }
  assign("plotMultiTest_mf", plotMultiTest_mf, envir = parent.frame())

  plots <- list(
    default = list(
      plotBase = list(
        displayName = "Custom plot"
        # purposefully omit "plotType", which should default to "singleFeature"
      ),
      plotMultiFeature = list(
        displayName = "PCA",
        plotType = "multiFeature",
        packages = "stats"
      ),
      plotMultiTest_sf = list(
        displayName = "scatterplot_singlefeat",
        plotType = "multiTest",
        # default should set plotType to c("singleFeature", "multiTest")
        packages = "data.table"
      ),
      plotMultiTest_mf = list(
        displayName = "scatterplot_multifeat",
        plotType = c("multiFeature", "multiTest"),
        packages = "data.table"
      )
    ),
    model_03 = list(
      plotGg = list(
        displayName = "Custom ggplot2 plot",
        plotType = "singleFeature",
        packages = c("ggplot2", "stats")
      )
    )
  )
  return(plots)
}

testBarcodes <- function(n = 3) {
  barcodes <- list(
    default = list(
      statistic = "beta",
      labelStat = "Beta coefficient",
      labelLow = "Small effect size",
      labelHigh = "Large effect size"
    ),
    model_03 = list(
      statistic = "beta",
      labelStat = "Effect size",
      labelLow = "Low effect size",
      labelHigh = "High effect size"
    )
  )
  return(barcodes)
}

testReports <- function(n = 3) {
  reports <- list(
    default = "https://www.domain.com/default.html",
    model_03 = "https://www.domain.com/model_03.html"
  )
  return(reports)
}

testResultsLinkouts <- function(n = 3) {
  resultsLinkouts <- list(
    default = list(
      customID = c("https://ensembl.org/Homo_sapiens/Gene/Summary?g=",
                   "https://www.targetvalidation.org/target/"),
      featureVar01 = "https://www.ncbi.nlm.nih.gov/gene/"
    ),
    model_03 = list(
      featureVar02 = "https://www.ncbi.nlm.nih.gov/nuccore?term="
    )
  )
  return(resultsLinkouts)
}

testEnrichmentsLinkouts <- function(n = 3) {
  enrichmentsLinkouts <- list(
    annotation_01 = c("http://amigo.geneontology.org/amigo/term/",
                      "https://www.ebi.ac.uk/QuickGO/term/"),
    annotation_03 = "https://reactome.org/content/detail/"
  )
  return(enrichmentsLinkouts)
}

testMetaFeaturesLinkouts <- function(n = 3) {
  metaFeaturesLinkouts <- list(
    default = list(
      metaFeatureVar01 = c("https://ensembl.org/Homo_sapiens/Gene/Summary?g=",
                   "https://www.targetvalidation.org/target/"),
      metaFeatureVar02 = "https://www.ncbi.nlm.nih.gov/gene/"
    ),
    model_03 = list(
      metaFeatureVar03 = "https://www.ncbi.nlm.nih.gov/nuccore?term="
    )
  )
  return(metaFeaturesLinkouts)
}

testStudyMeta <- function() {
  list(
    department = "immunology",
    organism = "Mus musculus"
  )
}

testStudyMinimal <- function() {
  createStudy(
    name = "minimal",
    description = "A minimal study for testing",
    results = testResults(),
    enrichments = testEnrichments()
  )
}

testStudyNumeric <- function() {
  testStudy(
    name = "numeric",
    description = "A study with numeric feature IDs.",
    numericFeatureID = TRUE
  )
}
