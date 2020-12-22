#!/usr/bin/env Rscript

# Proof of concept for downloading zipfile to R package during installation

installDir <- commandArgs(trailingOnly = TRUE)
zipurl <- "https://github.com/jdblischak/workflowr/archive/v1.6.2.zip"
zipfile <- file.path(installDir, "build.zip")
utils::download.file(url = zipurl, destfile = zipfile)
utils::unzip(zipfile, exdir = installDir)
unlink(zipfile)
