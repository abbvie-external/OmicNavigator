#' Export a study
#'
#' @param study An OmicAnalyzer study
#' @param type Export to a RDS file ("rds"), text files ("text"), or an
#'   R package ("package")
#' @param path Optional file path to save the object
#'
#' @export
exportStudy <- function(study, type = c("rds", "text", "package"), path = NULL) {
  checkStudy(study)

  type <- match.arg(type)

  if (type == "rds") {
    message(sprintf("Exporting study \"%s\" to an RDS file", study[["name"]]))
    filename <- paste0(study[["name"]], ".rds")
    if (!is.null(path)) filename <- file.path(path, filename)
    saveRDS(object = study, file = filename)
    message(sprintf("Exported study to %s", filename))
    return(invisible(filename))
  } else if (type == "text") {
    message(sprintf("Exporting study \"%s\" to directory of text files", study[["name"]]))
    directoryname <- paste0(study[["name"]])
    if (!is.null(path)) directoryname <- file.path(path, directoryname)
    createTextFiles(study, directoryname)
    message(sprintf("Exported study to %s", directoryname))
    return(invisible(directoryname))
  } else if (type == "package") {
    message(sprintf("Exporting study \"%s\" to an R package", study[["name"]]))
    directoryname <- paste0("OAstudy", study[["name"]])
    if (!is.null(path)) directoryname <- file.path(path, directoryname)
    createPackage(study, directoryname)
    message(sprintf("Exported study to %s", directoryname))
    return(invisible(directoryname))
  }
}

createTextFiles <- function(study, directoryname, calcOverlaps = FALSE) {

  dir.create(directoryname, showWarnings = FALSE, recursive = TRUE)
  exportSamples(study, directoryname)
  exportFeatures(study, directoryname)
  exportModels(study, directoryname)
  exportAssays(study, directoryname)
  exportTests(study, directoryname)
  exportAnnotations(study, directoryname)
  exportResults(study[["results"]], directoryname)
  exportEnrichments(study[["enrichments"]], directoryname)
  exportMetaFeatures(study, directoryname)
  exportPlots(study, directoryname)
  exportBarcodes(study, directoryname)
  exportSummary(study, directoryname)
  if (calcOverlaps && is.null(study[["overlaps"]])) {
    study <- addOverlaps(study)
  }
  exportOverlaps(study, directoryname)
}

exportElements <- function(
  study,
  elements,
  path = ".",
  fileType = c("txt", "json"),
  hasRowNames = FALSE
)
{
  directory <- file.path(path, elements)
  dir.create(directory, showWarnings = FALSE, recursive = TRUE)
  x <- study[[elements]]
  fileType <- match.arg(fileType)
  for (i in seq_along(x)) {
    fileName <- file.path(directory, names(x)[i])
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
    path = path
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

exportResults <- function(x, path = ".") {
  directory <- file.path(path, "results")
  dir.create(directory, showWarnings = FALSE, recursive = TRUE)
  for (i in seq_along(x)) {
    subDirectory <- file.path(directory, names(x)[i])
    dir.create(subDirectory, showWarnings = FALSE, recursive = TRUE)
    for (j in seq_along(x[[i]])) {
      fileName <- file.path(subDirectory, names(x[[i]])[j])
      fileName <- paste0(fileName, ".txt")
      writeTable(x[[i]][[j]], file = fileName)
    }
  }
}

exportEnrichments <- function(x, path = ".") {
  directory <- file.path(path, "enrichments")
  dir.create(directory, showWarnings = FALSE, recursive = TRUE)
  for (i in seq_along(x)) {
    subDirectory <- file.path(directory, names(x)[i])
    dir.create(subDirectory, showWarnings = FALSE, recursive = TRUE)
    for (j in seq_along(x[[i]])) {
      subSubDirectory <- file.path(subDirectory, names(x[[i]])[j])
      dir.create(subSubDirectory, showWarnings = FALSE, recursive = TRUE)
      for (k in seq_along(x[[i]][[j]])) {
        fileName <- file.path(subSubDirectory, names(x[[i]][[j]])[k])
        fileName <- paste0(fileName, ".txt")
        writeTable(x[[i]][[j]][[k]], file = fileName)
      }
    }
  }
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
    output[["results"]][[i]] <- list(
      modelID = modelID,
      modelDisplay = getModels(x, modelID = modelID)
    )
    modelTests <- names(x[["results"]][[modelID]])
    output[["results"]][[i]][["tests"]] <- vector("list", length(modelTests))
    for (j in seq_along(modelTests)) {
      testID <- modelTests[j]
      output[["results"]][[i]][["tests"]][[j]] <- list(
        testID = testID,
        testDisplay = getTests(x, modelID = modelID)[j, "description"]
      )
    }
  }

  for (i in seq_along(enrichmentsModels)) {
    modelID <- enrichmentsModels[i]
    output[["enrichments"]][[i]] <- list(
      modelID = modelID,
      modelDisplay = getModels(x, modelID = modelID)
    )
    modelAnnotations <- names(x[["enrichments"]][[modelID]])
    output[["enrichments"]][[i]][["annotations"]] <- vector("list", length(modelAnnotations))
    for (j in seq_along(modelAnnotations)) {
      annotationID <- modelAnnotations[j]
      output[["enrichments"]][[i]][["annotations"]][[j]] <- list(
        annotationID = annotationID,
        annotationDisplay = getAnnotations(x, annotationID = annotationID)[["description"]]
      )
    }
  }

  for (i in seq_along(plotsModels)) {
    modelID <- plotsModels[i]
    output[["plots"]][[i]] <- list(
      modelID = modelID,
      modelDisplay = getModels(x, modelID = modelID)
    )
    modelPlots <-tryCatch(
      getPlots(x, modelID = modelID),
      error = function(e) list()
    )
    output[["plots"]][[i]][["plots"]] <- vector("list", length(modelPlots))
    for (j in seq_along(modelPlots)) {
      plotID <- names(modelPlots)[j]
      output[["plots"]][[i]][["plots"]][[j]] <- list(
        plotID = plotID,
        plotDisplay = modelPlots[[j]][["displayName"]]
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
  description <- data.frame(
    Package = pkgname,
    Title = sprintf("OmicAnalyzer study %s", study[["name"]]),
    Version = pkgversion,
    Description = sprintf("The OmicAnalyzer data package for the study \"%s\"",
                          study[["name"]]),
    OmicAnalyzerVersion = utils::packageVersion("OmicAnalyzer"),
    stringsAsFactors = FALSE
  )
  write.dcf(description, file = description_file)

  # Data
  datadir <- file.path(directoryname, "inst", "OmicAnalyzer")
  dir.create(datadir, showWarnings = FALSE, recursive = TRUE)
  createTextFiles(study, datadir, calcOverlaps = TRUE)

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
#' @param study An OmicAnalyzer study to install (class \code{oaStudy})
#' @param library Directory to install package. Defaults to first directory
#'   returned by \code{\link{.libPaths}}.
#'
#' @export
installStudy <- function(study, library = .libPaths()[1]) {
  stopifnot(inherits(study, "oaStudy"), dir.exists(library))

  tmpPath <- if (getRversion() >= "3.5.0") tempdir(check = TRUE) else tempdir()
  tmpPkgDir <- exportStudy(study, type = "package", path = tmpPath)
  on.exit(unlink(tmpPkgDir, recursive = TRUE, force = TRUE), add = TRUE)
  buildPkg(tmpPkgDir)
  tarball <- Sys.glob(sprintf("OAstudy%s_*.tar.gz", study[["name"]]))
  stopifnot(length(tarball) == 1)
  on.exit(file.remove(tarball), add = TRUE)
  utils::install.packages(tarball, lib = library, repos = NULL, quiet = TRUE)

  return(invisible(study))
}

buildPkg <- function(pkgDir) {
  r <- file.path(R.home("bin"), "R")
  system2(r, args = c("CMD", "build", pkgDir), stdout = NULL, stderr = NULL)
  return(invisible(pkgDir))
}
