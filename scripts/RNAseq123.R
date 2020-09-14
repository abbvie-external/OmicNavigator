#!/usr/bin/env Rscript

# Re-run RNAseq123 analysis, subset the objects, and save as examples for
# User's Guide.
#
# https://master.bioconductor.org/packages/release/workflows/html/RNAseq123.html

if (!requireNamespace("BiocManager", quietly = TRUE))
  stop("Package {BiocManager} is required: install.packages(\"BiocManager\")")

if (!requireNamespace("RNAseq123", quietly = TRUE))
  stop("Package {RNAseq123} is required: BiocManager::install(\"RNAseq123\")")

pkgVignette <- utils::vignette("limmaWorkflow", package = "RNAseq123")
script <- file.path(pkgVignette$Dir, "doc", pkgVignette$R)
stopifnot(file.exists(script))

# The call to gplots::heatmap.2() causes an error, so remove it.
# Error in plot.new() : figure margins too large
code <- parse(script)
heatmapCall <- grep("heatmap", code)
code[[heatmapCall]] <- NULL

cwd <- setwd(tempdir())
message("Executing vignette...")
suppressMessages(output <- utils::capture.output(base::eval(code)))
setwd(cwd)
ls()
