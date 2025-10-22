#' Import a study package
#'
#' Create an onStudy object by importing an installed study package
#'
#' @param study Name of an installed OmicNavigator study
#' @inheritParams shared-get
#'
#' @return Returns the \code{onStudy} object imported from the OmicNavigator
#'   study package
#'
#' @export
importStudy <- function(study, libraries = NULL) {
  pkg <- studyToPkg(study)
  if (!requireNamespace(pkg, lib.loc = libraries, quietly = TRUE)) {
    stop("The package ", pkg, " is not installed")
  }
  pathToPkg <- find.package(pkg, lib.loc = libraries, quiet = TRUE, verbose = FALSE)

  # Export plotting functions to global environment
  functions <- getNamespaceExports(pkg)
  for (fname in functions) {
    f <- getFromNamespace(fname, pkg)
    assign(fname, f, envir = parent.frame())
  }

  studyMeta <- getStudyMeta(study, libraries = libraries)

  # Make paths to reports absolute
  reports <- getReports(study, quiet = TRUE, libraries = libraries)
  reports <- lapply(reports, function(x) {
    if (isUrl(x)) x else file.path(dirname(pathToPkg), x)
  })

  onStudy <- createStudy(
    name = study,
    description = studyMeta[["description"]],
    samples = getSamples(study, quiet = TRUE, libraries = libraries),
    features = getFeatures(study, quiet = TRUE, libraries = libraries),
    models = getModels(study, quiet = TRUE, libraries = libraries),
    assays = getAssays(study, quiet = TRUE, libraries = libraries),
    tests = getTests(study, quiet = TRUE, libraries = libraries),
    annotations = getAnnotations(study, quiet = TRUE, libraries = libraries),
    results = getResults(study, quiet = TRUE, libraries = libraries),
    enrichments = getEnrichments(study, quiet = TRUE, libraries = libraries),
    metaFeatures = getMetaFeatures(study, quiet = TRUE, libraries = libraries),
    plots = getPlots(study, quiet = TRUE, libraries = libraries),
    barcodes = getBarcodes(study, quiet = TRUE, libraries = libraries),
    reports = reports,
    mapping = getMapping(study, quiet = TRUE, libraries = libraries),
    resultsLinkouts = getResultsLinkouts(study, quiet = TRUE, libraries = libraries),
    enrichmentsLinkouts = getEnrichmentsLinkouts(study, quiet = TRUE, libraries = libraries),
    metaFeaturesLinkouts = getMetaFeaturesLinkouts(study, quiet = TRUE, libraries = libraries),
    metaAssays = getMetaAssays(study, quiet = TRUE, libraries = libraries),
    version = studyMeta[["version"]],
    maintainer = studyMeta[["maintainer"]],
    maintainerEmail = studyMeta[["maintainerEmail"]],
    studyMeta = studyMeta[["studyMeta"]]
  )
  onStudy[["overlaps"]] <- getOverlaps(study, quiet = TRUE, libraries = libraries)

  # Unload the package namespace from the search path. Otherwise find.package()
  # will continue to identify this package, even if it has subsequently been
  # deleted.
  unloadNamespace(pkg)

  return(onStudy)
}
