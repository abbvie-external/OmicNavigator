
#' @export
getResultsStudies <- function(libraries = NULL) {
  getInstalledStudies(hasElements = "results", libraries = libraries)
}

#' @export
getResultsModels <- function(study, libraries = NULL) {
  if (!is.character(study)) {
    stop("Only installed study packages are supported")
  }

  onDirectory <- getDirectory(study, libraries)
  resultsDirectory <- file.path(onDirectory, "results")
  resultsModels <- dir(resultsDirectory)

  if (isEmpty(resultsModels)) return(list())

  dropdown <- vector("list", length = length(resultsModels))
  names(dropdown) <- resultsModels

  # Check for available tooltips added via addModels(). If not available, use
  # the modelID itself for the tooltip
  for (modelID in resultsModels) {
    modelDisplay <- getModels(study, modelID = modelID, quiet = TRUE, libraries = libraries)
    # The tooltip is either added as a single string per modelID, or as a named
    # list with other metadata fields, where the field "description" is the
    # metadata field
    if (is.list(modelDisplay)) modelDisplay <- modelDisplay[["description"]]
    if (isEmpty(modelDisplay)) modelDisplay <- modelID
    dropdown[[modelID]] <- modelDisplay
  }

  return(dropdown)
}

#' @export
getResultsTests <- function(study, modelID, libraries = NULL) {
  if (!is.character(study)) {
    stop("Only installed study packages are supported")
  }

  onDirectory <- getDirectory(study, libraries)
  elementsFiles <- getFiles(onDirectory)
  resultsTests <- names(elementsFiles[["results"]][[modelID]])

  if (isEmpty(resultsTests)) return(list())

  dropdown <- vector("list", length = length(resultsTests))
  names(dropdown) <- resultsTests

  # Check for available tooltips added via addTests(). If not available, use the
  # testID itself for the tooltip
  for (testID in resultsTests) {
    testDisplay <- getTests(study, modelID = modelID, testID = testID, quiet = TRUE, libraries = libraries)
    # The tooltip is either added as a single string per testID, or as a named
    # list with other metadata fields, where the field "description" is the
    # metadata field
    if (is.list(testDisplay)) testDisplay <- testDisplay[["description"]]
    if (isEmpty(testDisplay)) testDisplay <- testID
    dropdown[[testID]] <- testDisplay
  }

  return(dropdown)
}

#' @export
getEnrichmentsStudies <- function(libraries = NULL) {
  getInstalledStudies(hasElements = "enrichments", libraries = libraries)
}

#' @export
getEnrichmentsModels <- function(study, libraries = NULL) {
  if (!is.character(study)) {
    stop("Only installed study packages are supported")
  }

  onDirectory <- getDirectory(study, libraries)
  enrichmentsDirectory <- file.path(onDirectory, "enrichments")
  enrichmentsModels <- dir(enrichmentsDirectory)

  if (isEmpty(enrichmentsModels)) return(list())

  dropdown <- vector("list", length = length(enrichmentsModels))
  names(dropdown) <- enrichmentsModels

  # Check for available tooltips added via addModels(). If not available, use
  # the modelID itself for the tooltip
  for (modelID in enrichmentsModels) {
    modelDisplay <- getModels(study, modelID = modelID, quiet = TRUE, libraries = libraries)
    # The tooltip is either added as a single string per modelID, or as a named
    # list with other metadata fields, where the field "description" is the
    # metadata field
    if (is.list(modelDisplay)) modelDisplay <- modelDisplay[["description"]]
    if (isEmpty(modelDisplay)) modelDisplay <- modelID
    dropdown[[modelID]] <- modelDisplay
  }

  return(dropdown)
}

#' @export
getEnrichmentsAnnotations <- function(study, modelID, libraries = NULL) {
  if (!is.character(study)) {
    stop("Only installed study packages are supported")
  }

  onDirectory <- getDirectory(study, libraries)
  modelAnnotations <- dir(file.path(onDirectory, "enrichments", modelID))

  if (isEmpty(modelAnnotations )) return(list())

  dropdown <- vector("list", length = length(modelAnnotations ))
  names(dropdown) <- modelAnnotations

  # Check for available tooltips added via addAnnotations(). If not available,
  # use the annotationID itself for the tooltip
  for (annotationID in modelAnnotations) {
    annotationDisplay <- getAnnotations(study, annotationID = annotationID, quiet = TRUE, libraries = libraries)
    annotationDisplay <- annotationDisplay[["description"]]
    if (isEmpty(annotationDisplay)) annotationDisplay <- annotationID
    dropdown[[annotationID]] <- annotationDisplay
  }

  return(dropdown)
}
