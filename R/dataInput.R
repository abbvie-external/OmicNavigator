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
                        contrasts = NULL,
                        annotations = NULL,
                        inferences = NULL,
                        enrichments = NULL,
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
                contrasts = NULL,
                annotations = NULL,
                inferences = NULL,
                enrichments = NULL,
                featureID = featureID,
                sampleID = sampleID)
  class(study) <- "oaStudy"

  if (!is.null(samples)) study <- addSamples(study, samples = samples)
  if (!is.null(features)) study <- addFeatures(study, features = features)
  if (!is.null(models)) study <- addModels(study, models = models)
  if (!is.null(contrasts)) study <- addContrasts(study, contrasts = contrasts)
  if (!is.null(annotations)) study <- addAnnotations(study, annotations = annotations)
  if (!is.null(inferences)) study <- addInferences(study, inferences = inferences)
  if (!is.null(enrichments)) study <- addEnrichments(study, enrichments = enrichments)

  return(study)
}

#' @export
print.oaStudy <- function(x, ...) {

  cat("== OmicAnalyzer ==\n")
  cat(sprintf("* Study name: %s\n", x$name))
  cat(sprintf("* Feature ID column name: %s\n", x$featureID))
  cat(sprintf("* Sample ID column name: %s\n", x$sampleID))

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
#'   character vector. The names correspond to the names of the models. The
#'   elements correspond to the descriptions of the models.
#'
#' @export
addModels <- function(study, models, overwrite = FALSE) {
  stopifnot(inherits(study, "oaStudy"), is.character(models))

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

  if (!all(names(study$models)) %in% names(assays)) {
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
