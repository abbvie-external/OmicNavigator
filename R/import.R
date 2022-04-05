#' Import a study package
#'
#' Create an onStudy object by importing an installed study package
#'
#' @param study Named of an installed OmicNavigator study
#' @inheritParams listStudies
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
    f <- utils::getFromNamespace(fname, pkg)
    assign(fname, f, envir = parent.frame())
  }

  # Import info from DESCRIPTION
  description <- utils::packageDescription(pkg, lib.loc = libraries)
  if (is.null(description[["Maintainer"]])) {
    description[["Maintainer"]] <- "Unknown <unknown@unknown>"
    message(
      "This study package didn't have a maintainer listed",
      "\nUsing the placeholder: ", description[["Maintainer"]],
      "\nHighly recommended to update this with your own name and email:",
      "\n<name of study object>$maintainer <- \"<Your Name>\"",
      "\n<name of study object>$maintainer <- \"<youremail@domain.com>\"",
      "\n(replace the text in between the brackets; make sure to delete the brackets)"
    )
  }
  maintainerField <- strsplit(description[["Maintainer"]], "<|>")[[1]]
  maintainer <- sub("[[:space:]]$", "", maintainerField[1])
  maintainerEmail <- maintainerField[2]
  descriptionFieldsReservedFile <- system.file(
    "extdata/description-fields-reserved.txt",
    package = "OmicNavigator",
    mustWork = TRUE
  )
  descriptionFieldsReserved <- scan(
    file = descriptionFieldsReservedFile,
    what = character(),
    quiet = TRUE
  )
  studyMeta <- description[setdiff(names(description), descriptionFieldsReserved)]

  # Make paths to reports absolute
  reports <- getReports(study, quiet = TRUE, libraries = libraries)
  reports <- lapply(reports, function(x) {
    if (isUrl(x)) x else file.path(dirname(pathToPkg), x)
  })

  onStudy <- createStudy(
    name = study,
    description = description[["Description"]],
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
    resultsLinkouts = getResultsLinkouts(study, quiet = TRUE, libraries = libraries),
    enrichmentsLinkouts = getEnrichmentsLinkouts(study, quiet = TRUE, libraries = libraries),
    metaFeaturesLinkouts = getMetaFeaturesLinkouts(study, quiet = TRUE, libraries = libraries),
    version = description[["Version"]],
    maintainer = maintainer,
    maintainerEmail = maintainerEmail,
    studyMeta = studyMeta
  )
  onStudy[["overlaps"]] <- getOverlaps(study, quiet = TRUE, libraries = libraries)

  # Unload the package namespace from the search path. Otherwise find.package()
  # will continue to identify this package, even if it has subsequently been
  # deleted.
  unloadNamespace(pkg)

  return(onStudy)
}
