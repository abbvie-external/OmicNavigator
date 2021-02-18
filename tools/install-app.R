#!/usr/bin/env Rscript

# Download the web app zipfile during R package installation

installDir <- commandArgs(trailingOnly = TRUE)
appDir <- file.path(installDir, "www")

installAppDuringPkgInstall <- function(installDir) {

  zipurl <- "https://github.com/AbbVie-External/OmicNavigatorWebApp/releases/download/v1.0.0/build.zip"
  zipfile <- file.path(installDir, "build.zip")
  dirBuild <- file.path(installDir, "build")
  dirWww <- file.path(installDir, "www")

  if (dir.exists(dirWww)) {
    message("\nWeb app is already included in R package. Skipping download.\n")
    return()
  }

  message("\nAttempting to download and install the OmicNavigator web app\n")

  on.exit(unlink(zipfile, force = TRUE), add = TRUE)
  on.exit(unlink(dirBuild, recursive = TRUE, force = TRUE), add = TRUE)

  suppressWarnings(
    utils::download.file(url = zipurl, destfile = zipfile)
  )
  utils::unzip(zipfile, exdir = installDir)
  dir.create(dirWww, showWarnings = FALSE)
  file.copy(from = file.path(dirBuild, "."), to = dirWww, recursive = TRUE)
  return(invisible(dirWww))
}

tryCatch(
  installAppDuringPkgInstall(installDir),
  error = function(e) {
    unlink(appDir, recursive = TRUE, force = TRUE)
    message("\nFailed to download and install the app. Run installApp()\n")
  }
)
