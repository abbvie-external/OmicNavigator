importStudy <- function(x) {
  stopifnot(is.character(x), length(x) == 1)
  if (endsWith(x, ".tar.gz")) {
    importStudyFromTarball(x)
  } else if (dir.exists(x)) {
    importStudyFromDirectory(x)
  } else if (nchar(find.package(x, quiet = TRUE)) > 0) {
    importStudyFromDirectory(x, inst = "")
  } else {
    stop("Please pass a study package tarball, study package source directory, or the name of an installed study package")
  }
}

installStudyFromTarball <- function(x) {
  tempDir <- tempfile()
  utils::untar(x, exdir = tempDir)
  importStudyFromDirectory(tempDir)
}

importStudyFromDirectory <- function(x, inst = "inst/") {
  descriptionFile <- file.path(x, "DESCRIPTION")
  onData <- file.path(x, inst, "OmicNavigator")
}
