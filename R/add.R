#' Create a study
#'
#' @param name Name of the study
#' @param description Description of the study
#' @param featureID The column name that contains the unique identifiers for the
#'   features measured in the study
#' @param sampleID The column name that contains the unique identifiers for the
#'   samples measured in the study
#' @inheritParams addSamples
#' @inheritParams addFeatures
#' @inheritParams addModels
#' @inheritParams addAssays
#' @inheritParams addTests
#' @inheritParams addAnnotations
#' @inheritParams addResults
#' @inheritParams addEnrichments
#' @inheritParams addMetaFeatures
#' @inheritParams addPlots
#'
#' @examples
#'
#' study <- createStudy(name = "ABC",
#'                      description = "An analysis of ABC")
#'
#' @export
createStudy <- function(name,
                        description = name,
                        samples = NULL,
                        features = NULL,
                        models = NULL,
                        assays = NULL,
                        tests = NULL,
                        annotations = NULL,
                        results = NULL,
                        enrichments = NULL,
                        metaFeatures = NULL,
                        plots = NULL,
                        featureID = "featureID",
                        sampleID = "sampleID")
{
  stopifnot(is.character(name), is.character(description))

  study <- list(name = name,
                description = description,
                samples = NULL,
                features = NULL,
                models = NULL,
                assays = NULL,
                tests = NULL,
                annotations = NULL,
                results = NULL,
                enrichments = NULL,
                metaFeatures = NULL,
                plots = NULL,
                featureID = featureID,
                sampleID = sampleID)
  class(study) <- "oaStudy"

  if (!is.null(samples)) study <- addSamples(study, samples = samples)
  if (!is.null(features)) study <- addFeatures(study, features = features)
  if (!is.null(models)) study <- addModels(study, models = models)
  if (!is.null(assays)) study <- addAssays(study, assays = assays)
  if (!is.null(tests)) study <- addTests(study, tests = tests)
  if (!is.null(annotations)) study <- addAnnotations(study, annotations = annotations)
  if (!is.null(results)) study <- addResults(study, results = results)
  if (!is.null(enrichments)) study <- addEnrichments(study, enrichments = enrichments)
  if (!is.null(metaFeatures)) study <- addMetaFeatures(study, metaFeatures = metaFeatures)
  if (!is.null(plots)) study <- addPlots(study, plots = plots)

  return(study)
}

#' @export
print.oaStudy <- function(x, ...) {

  cat("== OmicAnalyzer ==\n")
  cat(sprintf("* Study name: %s\n", x$name))
  cat(sprintf("* Feature ID column name: %s\n", x$featureID))
  cat(sprintf("* Sample ID column name: %s\n", x$sampleID))

  if (!is.null(x$samples)) {
    cat(sprintf("* Samples: %d\n", nrow(x$samples)))
    cat(sprintf("* Sample metadata variables: %d\n", ncol(x$samples)))
  }

  if (!is.null(x$features)) {
    cat(sprintf("* Features: %d\n", nrow(x$features)))
    cat(sprintf("* Feature metadata variables: %d\n", ncol(x$features)))
  }

  if (!is.null(x$metaFeatures)) {
    cat(sprintf("* Meta-feature metadata variables: %d\n", ncol(x$metaFeatures) - 1))
  }

  if (!is.null(x$models)) {
    cat(sprintf("* Models: %d\n", length(x$models)))
    for (i in seq_along(x$models)) {
      cat(sprintf("  * \"%s\"\n", names(x$models)[i]))
    }
  }

  if (!is.null(x$assays)) {
    cat(sprintf("* Assays: %d\n", length(x$assays)))
    for (i in seq_along(x$assays)) {
      cat(sprintf("  * \"%s\": %d x %d\n", names(x$assays)[i], nrow(x$assays[[i]]),
          ncol(x$assays[[i]])))
    }
  }

  if (!is.null(x$tests)) {
    cat(sprintf("* Tests: %d\n", length(x$tests)))
    for (i in seq_along(x$tests)) {
      cat(sprintf("  * \"%s\"\n", names(x$tests)[i]))
    }
  }

  if (!is.null(x$annotations)) {
    cat(sprintf("* Annotations: %d\n", length(x$annotations)))
    for (i in seq_along(x$annotations)) {
      cat(sprintf("  * \"%s\"\n", names(x$annotations)[i]))
    }
  }

  if (!is.null(x$results)) {
    cat("* Results:\n")
    for (i in seq_along(x$results)) {
      cat(sprintf("  * \"%s\":\n", names(x$results)[i]))
      for (j in seq_along(x$results[[i]])) {
        cat(sprintf("    * \"%s\": %d results\n", names(x$results[[i]])[j],
                    nrow(x$results[[i]][[j]])))
      }
    }
  }

  if (!is.null(x$enrichments)) {
    cat("* Enrichments:\n")
    for (i in seq_along(x$enrichments)) {
      cat(sprintf("  * \"%s\":\n", names(x$enrichments)[i]))
      for (j in seq_along(x$enrichments[[i]])) {
        cat(sprintf("    * \"%s\":\n", names(x$enrichments[[i]])[j]))
        for (k in seq_along(x$enrichments[[i]][[j]])) {
          cat(sprintf("      * \"%s\": %d results\n",
                      names(x$enrichments[[i]][[j]])[k],
                      nrow(x$enrichments[[i]][[j]][[k]])))
        }
      }
    }
  }

  if (!is.null(x$plots)) {
    cat(sprintf("* Custom plots: %d\n", length(x$plots)))
    for (i in seq_along(x$plots)) {
      cat(sprintf("  * \"%s\" - \"%s\"\n", names(x$plots)[i],
                  x$plots[[i]][["displayName"]]))
    }
  }

  return(invisible(x))
}

#' Add sample metadata
#'
#' @param samples A table of metadata variables that describe the samples in the
#'   study. The table must contain the unique sampleID used for the study. Also,
#'   the object must inherit from the class data.frame.
#'
#' @export
addSamples <- function(study, samples, overwrite = FALSE) {
  stopifnot(inherits(study, "oaStudy"), inherits(samples, "data.frame"))

  if (!study$sampleID %in% colnames(samples)) {
    stop(
      sprintf("The samples table doesn't contain the sampleID column named \"%s\"",
              study$sampleID)
    )
  }

  if (overwrite || is.null(study$samples)) {
    study$samples <- samples
  } else {
    stop("Sample metadata already exists. Set overwrite=TRUE to overwrite.")
  }

  return(study)
}

#' Add feature metadata
#'
#' @param features A table of metadata variables that describe the features in the
#'   study. The table must contain the unique featureID used for the study. Also,
#'   the object must inherit from the class data.frame.
#'
#' @export
addFeatures <- function(study, features, overwrite = FALSE) {
  stopifnot(inherits(study, "oaStudy"), inherits(features, "data.frame"))

  if (!study$featureID %in% colnames(features)) {
    stop(
      sprintf("The features table doesn't contain the featureID column named \"%s\"",
              study$featureID)
    )
  }

  key_column <- features[[study$featureID]]
  if (length(key_column) != length(unique(key_column))) {
    stop(sprintf("The key column \"%s\" contains duplicates", study$featureID))
  }

  if (overwrite || is.null(study$features)) {
    study$features <- features
  } else {
    stop("Feature metadata already exists. Set overwrite=TRUE to overwrite.")
  }

  return(study)
}

#' Add models
#'
#' @param models The models analyzed in the study. The input is a named
#'   list. The names correspond to the names of the models. The
#'   elements correspond to the descriptions of the models.
#'
#' @export
addModels <- function(study, models, overwrite = FALSE) {
  stopifnot(inherits(study, "oaStudy"), inherits(models, "list"))

  if (overwrite || is.null(study$models)) {
    study$models <- models
  } else {
    stop("Models metadata already exists. Set overwrite=TRUE to overwrite.")
  }

  return(study)
}

#' Add assays
#'
#' @param assays The assays from the study. The input is a named list. The names
#'   of the list correspond to the model names. Each element in the list should
#'   be a matrix of quantifications for the assays. The column names should be
#'   the sample IDs and the rows should be the feature IDs.
#'
#' @export
addAssays <- function(study, assays, overwrite = FALSE) {
  stopifnot(inherits(study, "oaStudy"), inherits(assays, "list"))

  if (!all(names(study$models) %in% names(assays))) {
    stop(sprintf("The names of the list do not include all of the model names"))
  }

  for (assay in assays) {
    stopifnot(inherits(assay, "matrix"))
    stopifnot(all(colnames(assay) %in% study$samples[[study$sampleID]]))
    stopifnot(all(rownames(assay) %in% study$features[[study$featureID]]))
  }

  if (overwrite || is.null(study$assays)) {
    study$assays <- assays
  } else {
    stop("assays metadata already exists. Set overwrite=TRUE to overwrite.")
  }

  return(study)
}

#' Add tests
#'
#' @param tests The tests tested by the model(s). A named list.
#'   The names correspond to the name of each test. The elements
#'   correspond to the description of each test.
#'
#' @export
addTests <- function(study, tests, overwrite = FALSE) {
  stopifnot(inherits(study, "oaStudy"), inherits(tests, "list"))

  if (overwrite || is.null(study$tests)) {
    study$tests <- tests
  } else {
    stop("The tests already exist. Set overwrite=TRUE to overwrite.")
  }

  return(study)
}

#' Add annotations
#'
#' @param annotations The annotations used for the enrichment analyses. The
#'   input is a nested list. The top-level list contains one entry per
#'   annotation database, e.g. reactome. The names correspond to the name of
#'   each annotation database. Each of these elements should be list of that
#'   contains more information about each annotation database. Specifically the
#'   sublist should contain 1) \code{description}, a character vector that
#'   describes the resource, 2) \code{featureID}, the name of the column in the
#'   features table that was used for the enrichment analysis (if omitted, it is
#'   assumed to be the main featureID used for the study), and 3) \code{terms},
#'   a list of annotation terms. The names of \code{terms} sublist correspond to
#'   the name of the annotation terms. Each of the annotation terms should be a
#'   character vector of feature IDs.
#'
#' @export
addAnnotations <- function(study, annotations, overwrite = FALSE) {
  stopifnot(inherits(study, "oaStudy"), inherits(annotations, "list"),
            length(annotations) > 0)

  for (i in seq_along(annotations)) {
    annotationID <- names(annotations)[i]
    if (is.null(annotationID)) {
      stop("The annotation list needs to be named")
    }
    if (is.null(annotations[[i]][["description"]])) {
      annotations[[i]][["description"]] <- sprintf("Annotation terms from %s",
                                                   annotationID)
    }
    if (is.null(annotations[[i]][["featureID"]])) {
      annotations[[i]][["featureID"]] <- study$featureID
    }
    if (!annotations[[i]][["featureID"]] %in% colnames(study$features)) {
      stop(sprintf("The ID \"%s\" for \"%s\" is not a column in the features table",
           annotations[[i]][["featureID"]], annotationID))
    }
    if (is.null(annotations[[i]][["terms"]])) {
      stop(sprintf("Missing the list of terms for \"%s\"", annotationID))
    }
    universe <- unique(unlist(annotations[[i]][["terms"]]))
    if (!any(study$features[[annotations[[i]][["featureID"]]]] %in% universe)) {
      stop(sprintf("None of the terms in \"%s\" contain feature IDs from \"%s\"\n",
                   annotationID, annotations[[i]][["featureID"]]),
           "Do you need specify the features column that was used for this enrichment analysis?")
    }
  }

  if (overwrite || is.null(study$annotations)) {
    study$annotations <- annotations
  } else {
    stop("The annotations already exist. Set overwrite=TRUE to overwrite.")
  }

  return(study)
}

#' Add result results
#'
#' @param results The result results from each model. The input is a
#'   nested named list. The names of the list correspond to the model names.
#'   Each element in the list should be a list of data frames with result
#'   results, one for each test. The featureID column needs to be included
#'   in each table.
#'
#' @export
addResults <- function(study, results, overwrite = FALSE) {
  stopifnot(inherits(study, "oaStudy"), inherits(results, "list"))

  if (!all(names(study$models) %in% names(results))) {
    stop(sprintf("The names of the list do not include all of the model names"))
  }

  for (i in seq_along(results)) {
    model <- results[[i]]
    model_name <- names(results)[i]
    stopifnot(inherits(model, "list"))
    stopifnot(model_name %in% names(study$models))
    for (j in seq_along(model)) {
      test <- model[[j]]
      test_name <- names(model)[j]
      stopifnot(inherits(test, "data.frame"))
      stopifnot(test_name %in% names(study$tests))
      stopifnot(study$featureID %in% colnames(test))
    }
  }

  if (overwrite || is.null(study$results)) {
    study$results <- results
  } else {
    stop("The result results already exist. Set overwrite=TRUE to overwrite.")
  }

  return(study)
}

#' Add enrichment results
#'
#' To reduce storage and compute time, it is highly recommended to first filter
#' the enrichment results for statistical signficance prior to adding them to
#' the study.
#'
#' @param enrichments The enrichment results from each model. The input is a
#'   nested named list. The names of the list correspond to the model names.
#'   Each list element should be a list of the tests tested. The names
#'   correspond to the test names. Each list element should be another list
#'   of annotation databases. The names correspond to the annotation databases.
#'   Each of these elements should be a data frame with enrichment results. Each
#'   table must have a column named "termID" that contains the annotation terms.
#'
#' @export
addEnrichments <- function(study, enrichments, overwrite = FALSE) {
  stopifnot(inherits(study, "oaStudy"), inherits(enrichments, "list"))

  if (!all(names(study$models) %in% names(enrichments))) {
    stop(sprintf("The names of the list do not include all of the model names"))
  }

  for (i in seq_along(enrichments)) {
    model <- enrichments[[i]]
    model_name <- names(enrichments)[i]
    stopifnot(inherits(model, "list"))
    stopifnot(model_name %in% names(study$models))
    for (j in seq_along(model)) {
      test <- model[[j]]
      test_name <- names(model)[j]
      stopifnot(inherits(test, "list"))
      stopifnot(test_name %in% names(study$tests))
      for (k in seq_along(test)) {
        annotation <- test[[k]]
        annotation_name <- names(test)[k]
        stopifnot(inherits(annotation, "data.frame"))
        stopifnot(annotation_name %in% names(study$annotations))
        stopifnot("termID" %in% colnames(annotation))
      }
    }
  }

  if (overwrite || is.null(study$enrichments)) {
    study$enrichments <- enrichments
  } else {
    stop("The enrichment results already exist. Set overwrite=TRUE to overwrite.")
  }

  return(study)
}

#' Add meta-feature metadata
#'
#' @param metaFeatures A table of metadata variables that describe the
#'   meta-features in the study. This is useful anytime there are metadata
#'   variables that cannot be mapped 1:1 to your features. For example, a
#'   peptide may be associated with multiple proteins. The table must contain
#'   the unique featureID used for the study. Also, the object must inherit from
#'   the class data.frame.
#'
#' @export
addMetaFeatures <- function(study, metaFeatures, overwrite = FALSE) {
  stopifnot(inherits(study, "oaStudy"), inherits(metaFeatures, "data.frame"))

  if (!study$featureID %in% colnames(metaFeatures)) {
    stop(
      sprintf("The metaFeatures table doesn't contain the featureID column named \"%s\"",
              study$featureID)
    )
  }

  if (is.null(study$features)) {
    stop("Please add the features table with addFeatures() prior to adding the metaFeatures table")
  }

  if (!all(metaFeatures[[study$featureID]] %in% study$features[[study$featureID]])) {
    stop("The metaFeatures table contains features that are not in the features table")
  }

  if (overwrite || is.null(study$metaFeatures)) {
    study$metaFeatures <- metaFeatures
  } else {
    stop("Feature metadata already exists. Set overwrite=TRUE to overwrite.")
  }

  return(study)
}

#' Add custom plotting functions
#'
#' Include custom plots that the app will display when a feature is selected by
#' the user.
#'
#' All custom plotting functions are required to have the same function
#' signature. The first argument is always \code{x}, which will be a data frame
#' that combines the sample metadata with a column containing the assay
#' measurements for a specific feature. That column will always be named
#' \code{feature}. The second argument, \code{featureName}, is the unique ID of
#' the feature (idea: pass in the entire row from the features table so that the
#' user can decide what metadata to display). This is for labeling the plot.
#'
#' Note that any ggplot2 plots will require extra care. This is because the
#' plotting code will be inserted into a study package, and thus must follow the
#' \href{https://ggplot2.tidyverse.org/articles/ggplot2-in-packages.html#using-aes-and-vars-in-a-package-function-1}{best
#' practices for using ggplot2 within packages}. Specifically, when you refer to
#' columns of the data frame, e.g. \code{aes(x = group)}, you need to prefix it
#' with \code{.data$}, so that it becomes \code{aes(x = .data$group)}.
#' Fortunately this latter code will also run fine as you interactively develop
#' the function.
#'
#' @param plots Custom plotting functions. The input is a nested list. Each
#'   element of the list defines a custom plotting function via a list with
#'   multiple options. The only required option is \code{definition}, which
#'   contains the function definition. You can optionally include
#'   \code{displayName} to control how the plot will be named in the app.
#'   Lastly, if the plottting function requires external packages, these can be
#'   defined in the argument \code{packages}.
#'
#' @export
addPlots <- function(study, plots, overwrite = FALSE) {
  stopifnot(inherits(study, "oaStudy"), inherits(plots, "list"),
            length(plots) > 0)

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

  if (overwrite || is.null(study$plots)) {
    study$plots <- plots
  } else {
    stop("The plots already exist. Set overwrite=TRUE to overwrite.")
  }

  return(study)
}
