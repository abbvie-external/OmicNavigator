#!/usr/bin/env Rscript

# Download the web app zipfile during R package installation

installDir <- commandArgs(trailingOnly = TRUE)
zipurl <- "https://github.com/AbbVie-External/OmicNavigatorWebApp/releases/download/v1.0.0/build.zip"
zipfile <- file.path(installDir, "build.zip")
utils::download.file(url = zipurl, destfile = zipfile)
utils::unzip(zipfile, exdir = installDir)
unlink(zipfile)
# Change name to www/
dirBuild <- file.path(installDir, "build")
dirWww <- file.path(installDir, "www")
success <- file.rename(dirBuild, dirWww)
if (!success) {
  # Copy instead
  dir.create(dirWww, showWarnings = FALSE)
  file.copy(from = file.path(dirBuild, "."), to = dirWww, recursive = TRUE)
  unlink(dirBuild, recursive = TRUE, force = TRUE)
}
