#!/usr/bin/env Rscript

# Re-run RNAseq123 analysis, subset the objects, and save as examples for
# User's Guide.
#
# https://bioconductor.org/packages/release/workflows/html/RNAseq123.html

if (!file.exists("DESCRIPTION"))
  stop("RNAseq123.R must be executed in the root directory of the package")

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

# Subset the data to only include the features in the 4 smallest reactome terms.
reactome <- grepl("^REACTOME", names(Mm.c2))
sizes <- lengths(Mm.c2)
terms <- names(Mm.c2)[reactome & sizes < 10]
stopifnot(length(terms) == 4)
Mm.c2 <- Mm.c2[terms]
genesSubset <- unlist(Mm.c2, use.names = FALSE)
genesSubsetMeasured <- genesSubset[genesSubset %in% x$genes$ENTREZID]
# Results
basal.vs.lp <- basal.vs.lp[rownames(basal.vs.lp) %in% genesSubsetMeasured, ]
basal.vs.ml <- basal.vs.ml[rownames(basal.vs.ml) %in% genesSubsetMeasured, ]
# Enrichments
cam.BasalvsLP <- cam.BasalvsLP[rownames(cam.BasalvsLP) %in% terms, ]
cam.BasalvsML <- cam.BasalvsML[rownames(cam.BasalvsML) %in% terms, ]
# Assays
lcpm <- lcpm[rownames(lcpm) %in% genesSubsetMeasured, ]

# Export
dir.create("data/", showWarnings = FALSE)
save(
  Mm.c2,
  basal.vs.lp,
  basal.vs.ml,
  cam.BasalvsLP,
  cam.BasalvsML,
  lcpm,
  samplenames,
  group,
  lane,
  file = "data/RNAseq123.RData",
  version = 2
)
