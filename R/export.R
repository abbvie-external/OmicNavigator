#' Export a study
#'
#' @param study An OmicNavigator study
#' @param type Export study as a package tarball ("tarball") or as a package
#'   directory ("package")
#' @param path Optional file path to save the object
#' @param requireValid Require that study is valid before exporting
#'
#' @return Invisibly returns the name of the tarball file ("tarball") or the
#'   path to the package directory ("package")
#'
#' @seealso \code{\link{validateStudy}}
#'
#' @export
exportStudy <- function(
  study,
  type = c("tarball", "package"),
  path = NULL,
  requireValid = TRUE
)
{
  if (requireValid) validateStudy(study) else checkStudy(study)
  type <- match.arg(type)

  message(sprintf("Exporting study \"%s\" as an R package", study[["name"]]))
  directoryname <- paste0(getPrefix(), study[["name"]])
  if (!is.null(path)) directoryname <- file.path(path, directoryname)
  # If anything goes wrong, clean up everything
  on.exit(unlink(directoryname, recursive = TRUE, force = TRUE) , add = TRUE)
  createPackage(study, directoryname)

  if (type == "package") {
    message(sprintf("Exported study to %s", directoryname))
    on.exit() # Don't delete the package directory
    return(invisible(directoryname))
  }

  # Build package tarball
  message("Converting study package to a package tarball")
  tarball <- buildPkg(directoryname)
  if (!is.null(path)) {
    tarballOriginal <- tarball
    tarball <- file.path(path, tarballOriginal)
    renameFile(tarballOriginal, tarball)
  }
  message(sprintf("Exported study to %s", tarball))
  return(invisible(tarball))
}

buildPkg <- function(pkgDir) {
  r <- file.path(R.home("bin"), "R")
  stdout <- system2(
    command = r,
    args = c("CMD", "build", shQuote(pkgDir)),
    stdout = TRUE,
    stderr = NULL
  )
  regex <- sprintf("%s.*\\.tar\\.gz", getPrefix())
  regexMatch <- regexpr(regex, stdout[length(stdout)])
  tarball <- regmatches(stdout[length(stdout)], regexMatch)
  return(invisible(tarball))
}

createTextFiles <- function(study, directoryname, calcOverlaps = FALSE) {

  dir.create(directoryname, showWarnings = FALSE, recursive = TRUE)
  exportSamples(study, directoryname)
  exportFeatures(study, directoryname)
  exportModels(study, directoryname)
  exportAssays(study, directoryname)
  exportTests(study, directoryname)
  exportAnnotations(study, directoryname)
  exportResults(study, directoryname)
  exportEnrichments(study, directoryname)
  exportMetaFeatures(study, directoryname)
  exportPlots(study, directoryname)
  exportBarcodes(study, directoryname)
  exportReports(study, directoryname)
  exportResultsLinkouts(study, directoryname)
  exportEnrichmentsLinkouts(study, directoryname)
  exportMetaFeaturesLinkouts(study, directoryname)
  exportSummary(study, directoryname)
  if (calcOverlaps) study <- addOverlaps(study)
  exportOverlaps(study, directoryname)
}

exportElements <- function(
  study,
  elements,
  path = ".",
  fileType = c("txt", "json"),
  hasRowNames = FALSE,
  nested = 0
)
{
  x <- study[[elements]]
  if (isEmpty(x)) return(NULL)

  directory <- file.path(path, elements)
  dir.create(directory, showWarnings = FALSE, recursive = TRUE)
  fileType <- match.arg(fileType)

  if (nested == 0) { # base case
    exportElementsWrite(
      x,
      path = directory,
      fileType = fileType,
      hasRowNames = hasRowNames
    )
  } else if (nested > 0) { # recursive case
    for (i in seq_along(x)) {
      exportElements(
        x,
        elements = names(x)[i],
        path = directory,
        fileType = fileType,
        hasRowNames = hasRowNames,
        nested = nested - 1
      )
    }
  } else {
    stop("Something has gone wrong with the recursion")
  }
}

exportElementsWrite <- function(
  x,
  path = ".",
  fileType = c("txt", "json"),
  hasRowNames = FALSE
)
{
  for (i in seq_along(x)) {
    fileName <- file.path(path, names(x)[i])
    if (fileType == "txt") {
      fileName <- paste0(fileName, ".txt")
      writeTable(x[[i]], file = fileName, row.names = hasRowNames)
    } else {
      fileName <- paste0(fileName, ".json")
      writeJson(x[[i]], file = fileName)
    }
  }
}

exportSamples <- function(study, path = ".") {
  exportElements(
    study,
    elements = "samples",
    path = path
  )
}

exportFeatures <- function(study, path = ".") {
  exportElements(
    study,
    elements = "features",
    path = path
  )
}

exportModels <- function(study, path = ".") {
  exportElements(
    study,
    elements = "models",
    path = path,
    fileType = "json"
  )
}

exportAssays <- function(study, path = ".") {
  exportElements(
    study,
    elements = "assays",
    path = path,
    hasRowNames = TRUE
  )
}

exportTests <- function(study, path = ".") {
  exportElements(
    study,
    elements = "tests",
    path = path,
    fileType = "json",
    nested = 1
  )
}

exportAnnotations <- function(study, path = ".") {
  exportElements(
    study,
    elements = "annotations",
    path = path,
    fileType = "json"
  )
}

exportResults <- function(study, path = ".") {
  exportElements(
    study,
    elements = "results",
    path = path,
    nested = 1
  )
}

exportEnrichments <- function(study, path = ".") {
  exportElements(
    study,
    elements = "enrichments",
    path = path,
    nested = 2
  )
}

exportMetaFeatures <- function(study, path = ".") {
  exportElements(
    study,
    elements = "metaFeatures",
    path = path
  )
}

exportPlots <- function(study, path = ".") {
  exportElements(
    study,
    elements = "plots",
    path = path,
    fileType = "json"
  )
}

exportBarcodes <- function(study, path = ".") {
  exportElements(
    study,
    elements = "barcodes",
    path = path,
    fileType = "json"
  )
}

exportReports <- function(study, path = ".") {
  exportElements(
    study,
    elements = "reports",
    path = path,
    fileType = "json"
  )
}

exportResultsLinkouts <- function(study, path = ".") {
  exportElements(
    study,
    elements = "resultsLinkouts",
    path = path,
    fileType = "json"
  )
}

exportEnrichmentsLinkouts <- function(study, path = ".") {
  exportElements(
    study,
    elements = "enrichmentsLinkouts",
    path = path,
    fileType = "json"
  )
}

exportMetaFeaturesLinkouts <- function(study, path = ".") {
  exportElements(
    study,
    elements = "metaFeaturesLinkouts",
    path = path,
    fileType = "json"
  )
}

exportSummary <- function(x, path = ".") {

  resultsModels <- names(x[["results"]])
  enrichmentsModels <- names(x[["enrichments"]])
  # Plots can be shared across models using modelID "default". Thus need to
  # consider all models that have inference results or enrichments available.
  plotsModels <- unique(c(resultsModels, enrichmentsModels))

  output <- list(
    results = vector("list", length(resultsModels)),
    enrichments = vector("list", length(enrichmentsModels)),
    plots = vector("list", length(plotsModels))
  )

  for (i in seq_along(resultsModels)) {
    modelID <- resultsModels[i]
    modelDisplay <- getModels(x, modelID = modelID, quiet = TRUE)
    if (isEmpty(modelDisplay)) modelDisplay <- modelID
    output[["results"]][[i]] <- list(
      modelID = modelID,
      modelDisplay = modelDisplay
    )
    modelTests <- names(x[["results"]][[modelID]])
    output[["results"]][[i]][["tests"]] <- vector("list", length(modelTests))
    for (j in seq_along(modelTests)) {
      testID <- modelTests[j]
      testDisplay <- getTests(x, modelID = modelID, testID = testID, quiet = TRUE)
      if (isEmpty(testDisplay)) testDisplay <- testID
      output[["results"]][[i]][["tests"]][[j]] <- list(
        testID = testID,
        testDisplay = testDisplay
      )
    }
  }

  for (i in seq_along(enrichmentsModels)) {
    modelID <- enrichmentsModels[i]
    modelDisplay <- getModels(x, modelID = modelID, quiet = TRUE)
    if (isEmpty(modelDisplay)) modelDisplay <- modelID
    output[["enrichments"]][[i]] <- list(
      modelID = modelID,
      modelDisplay = modelDisplay
    )
    modelAnnotations <- names(x[["enrichments"]][[modelID]])
    output[["enrichments"]][[i]][["annotations"]] <- vector("list", length(modelAnnotations))
    for (j in seq_along(modelAnnotations)) {
      annotationID <- modelAnnotations[j]
      annotationDisplay <- getAnnotations(x, annotationID = annotationID, quiet = TRUE)[["description"]]
      if (isEmpty(annotationDisplay)) annotationDisplay <- annotationID
      output[["enrichments"]][[i]][["annotations"]][[j]] <- list(
        annotationID = annotationID,
        annotationDisplay = annotationDisplay
      )
    }
  }

  for (i in seq_along(plotsModels)) {
    modelID <- plotsModels[i]
    modelDisplay <- getModels(x, modelID = modelID, quiet = TRUE)
    if (isEmpty(modelDisplay)) modelDisplay <- modelID
    output[["plots"]][[i]] <- list(
      modelID = modelID,
      modelDisplay = modelDisplay
    )
    modelPlots <- getPlots(x, modelID = modelID, quiet = TRUE)
    output[["plots"]][[i]][["plots"]] <- vector("list", length(modelPlots))
    for (j in seq_along(modelPlots)) {
      plotID <- names(modelPlots)[j]
      plotDisplay = modelPlots[[j]][["displayName"]]
      if (isEmpty(plotDisplay)) plotDisplay <- plotID
      plotType <- modelPlots[[j]][["plotType"]]
      if (isEmpty(plotType)) plotType <- "singleFeature"
      output[["plots"]][[i]][["plots"]][[j]] <- list(
        plotID = plotID,
        plotDisplay = plotDisplay,
        plotType = plotType
      )
    }
  }

  fileName <- file.path(path, "summary.json")
  writeJson(output, file = fileName)
}

exportOverlaps <- function(study, path = ".") {
  exportElements(
    study,
    elements = "overlaps",
    path = path
  )
}

createPackage <- function(study, directoryname) {

  dir.create(directoryname, showWarnings = FALSE, recursive = TRUE)

  # DESCRIPTION
  description_file <- file.path(directoryname, "DESCRIPTION")
  pkgname <- basename(directoryname)
  pkgversion <- if (is.null(study[["version"]])) "0.0.0.9000" else study[["version"]]
  if (study[["description"]] == study[["name"]]) {
    pkgdescription <- sprintf("The OmicNavigator data package for the study \"%s\"",
                              study[["name"]])
  } else {
    pkgdescription <- study[["description"]]
  }
  if (isEmpty(study[["maintainer"]])) {
    pkgmaintainer <- "Unknown"
  } else {
    pkgmaintainer <- study[["maintainer"]]
  }
  if (isEmpty(study[["maintainerEmail"]])) {
    pkgmaintainer <- paste(pkgmaintainer, "<unknown@unknown>")
  } else {
    pkgmaintainer <- sprintf("%s <%s>", pkgmaintainer, study[["maintainerEmail"]])
  }
  description <- data.frame(
    Package = pkgname,
    Title = sprintf("OmicNavigator study %s", study[["name"]]),
    Version = pkgversion,
    Maintainer = pkgmaintainer,
    Description = pkgdescription,
    OmicNavigatorVersion = utils::packageVersion("OmicNavigator"),
    stringsAsFactors = FALSE
  )
  if (!isEmpty(study[["studyMeta"]])) {
    description <- cbind(
      description,
      study[["studyMeta"]],
      stringsAsFactors = FALSE)
  }
  write.dcf(description, file = description_file)

  # Reports
  reports <- study[["reports"]]
  if (!isEmpty(reports) && any(!isUrl(unlist(reports)))) {
    reportsdir <- file.path(directoryname, "inst", "OmicNavigatorReports")
    dir.create(reportsdir, showWarnings = FALSE, recursive = TRUE)
    for (i in seq_along(reports)) {
      report <- reports[[i]]
      modelID <- names(reports)[i]
      if (!isUrl(report)) {
        newPath <- file.path(reportsdir, modelID)
        dir.create(newPath, showWarnings = FALSE, recursive = TRUE)
        fileExtension <- tools::file_ext(report)
        newFile <- paste0("report.", fileExtension)
        newPath <- file.path(newPath, newFile)
        file.copy(report, newPath)
        reports[[i]] <- file.path(pkgname, "OmicNavigatorReports", modelID, newFile)
      }
    }
    # Update study object to use new paths to installed reports
    # Note: Can't use addReports() here b/c the file doesn't exist until package
    # is installed.
    study[["reports"]] <- reports
  }

  # Data
  datadir <- file.path(directoryname, "inst", "OmicNavigator")
  dir.create(datadir, showWarnings = FALSE, recursive = TRUE)
  annotations <- study[["annotations"]]
  createTextFiles(study, datadir, calcOverlaps = !isEmpty(annotations))

  # Plots
  if (!isEmpty(study[["plots"]])) {
    # Can't have duplicate plots in different models
    plotsAll <- lapply(study[["plots"]], function(x) names(x))
    if (length(plotsAll) != length(unique(plotsAll))) {
      stop("Cannot have duplicate plots in different studies")
    }
    namespace_file <- file.path(directoryname, "NAMESPACE")
    r_dir <- file.path(directoryname, "R")
    dir.create(r_dir, showWarnings = FALSE)
    r_file <- file.path(r_dir, "plots.R")
    dependencies <- character()
    exports <- character()
    code <- character()
    for (i in seq_along(study[["plots"]])) {
      for (j in seq_along(study[["plots"]][[i]])) {
        plotID <- names(study[["plots"]][[i]])[j]
        plotDependencies <- study[["plots"]][[i]][[j]][["packages"]]
        # Base plotting functions like plot, boxplot, barplot, etc. rely on the
        # graphics package. It's a recommended package that is automatically
        # loaded when R starts, so a user is unlikely to think it add it. The
        # logic below adds graphics only if needed:
        #
        # * If the user manually adds graphics, then no need to add it again
        # * If ggplot2 is a dependency, it doesn't need graphics because it uses
        #   grid
        # * If lattice is a dependency, it doesn't need graphics because lattice
        #   imports graphics
        if (!any(c("graphics", "ggplot2", "lattice") %in% plotDependencies)) {
          plotDependencies <- c(plotDependencies, "graphics")
        }
        dependencies <- c(dependencies, plotDependencies)
        exports <- c(exports, sprintf("export(%s)", plotID))
        plotFunction <- getPlotFunction(plotID)
        plot_code <- deparse(plotFunction)
        plot_code[1] <- paste(plotID, "<-", plot_code[1])
        code <- c(code, plot_code)
      }
    }
    if ("ggplot2" %in% dependencies) {
      dependencies <- c(dependencies, "rlang")
      exports <- c(exports, "importFrom(rlang,\".data\")")
    }
    dependencies <- sort(unique(dependencies))
    imports <- paste(dependencies, collapse = ", ")
    imports <- paste("Imports:", imports)
    cat(imports, file = description_file, sep = "\n", append = TRUE)
    exports <- sort(exports)
    writeLines(exports, namespace_file)
    writeLines(code, r_file)
  }

  return(invisible(directoryname))
}

#' Install a study as an R package
#'
#' @param study An OmicNavigator study to install (class \code{onStudy})
#' @param library Directory to install package. Defaults to first directory
#'   returned by \code{\link{.libPaths}}.
#'
#' @export
installStudy <- function(study, library = .libPaths()[1]) {
  stopifnot(inherits(study, "onStudy"), dir.exists(library))
  message(sprintf("Installing study \"%s\" in %s", study[["name"]], library))

  tmpPath <- if (getRversion() >= "3.5.0") tempdir(check = TRUE) else tempdir()
  tmpPkgDir <- exportStudy(study, type = "package", path = tmpPath)
  on.exit(unlink(tmpPkgDir, recursive = TRUE, force = TRUE), add = TRUE)
  optionWarn <- getOption("warn", default = 0)
  on.exit(options(warn = optionWarn), add = TRUE)
  options(warn = 2) # warnings are errors
  utils::install.packages(
    tmpPkgDir,
    lib = library,
    repos = NULL,
    type = "source",
    verbose = FALSE,
    quiet = TRUE
  )

  message("Success!")
  return(invisible(study))
}

#' Remove an installed study R package
#'
#' @param study The name of the study or an \code{onStudy} object. Do \strong{not} include
#' the prefix of the installed package, e.g. \code{ONstudy}.
#' @param library Directory where the study package is installed. Defaults to first directory
#'   returned by \code{\link{.libPaths}}.
#'
#' @return Invisibly returns the path of the removed study package
#'
#' @export
removeStudy <- function(study, library = .libPaths()[1]) {
  if (!dir.exists(library)) {
    stop("This directory does not exist: ", library)
  }

  # Convert to R package name
  if (is.character(study)) {
    studyName <- study
  } else if (inherits(study, "onStudy")) {
    studyName <- study[["name"]]
  } else {
    stop("Argument `study` must be a string or an onStudy object")
  }
  package <- studyToPkg(studyName)

  packagePath <- find.package(
    package = package,
    lib.loc = library,
    quiet = TRUE
  )
  if (length(packagePath) == 0) {
    stop(sprintf("Couldn't find %s in %s", package, library))
  } else {
    message("Removing study package ", packagePath)
  }

  utils::remove.packages(
    pkgs = package,
    lib = library
  )
  return(invisible(packagePath))
}
