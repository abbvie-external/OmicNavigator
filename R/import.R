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
  descriptionFields <- read.dcf(descriptionFile)
  descriptionFields <- structure(
    as.list(descriptionFields),
    .Names = colnames(descriptionFields)
  )
  onData <- file.path(x, inst, "OmicNavigator")

  name <- pkgToStudy(descriptionFields[["Package"]])
  description <- descriptionFields[["Description"]]
  version <- descriptionFields[["Version"]]
  maintainerField <- strsplit(descriptionFields[["Maintainer"]], "<|>")[[1]]
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
  studyMeta <- descriptionFields[setdiff(names(descriptionFields), descriptionFieldsReserved)]

  createStudy(
    name = name,
    description = description,
    version = version,
    maintainer = maintainer,
    maintainerEmail = maintainerEmail,
    studyMeta = studyMeta
  )
}
