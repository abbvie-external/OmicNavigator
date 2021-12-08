# Create OmicNavigator study from files in the directory results/

library(ggplot2)
library(gplots)
library(viridis)
library(OmicNavigator)
#library(plotly)

# Create a new study -----------------------------------------------------------

study <- createStudy("RNAseq123",
                     "Bioc workflow package converted to OmicNavigator",
                     version = "0.1.1")

# Models -----------------------------------------------------------------------

models <- list(
  Differential_Expression = "A standard group-means analysis of 3 mammary cell populations"
)
study <- addModels(study, models)

# Samples ----------------------------------------------------------------------

samples <- read.delim("results/samples.txt", stringsAsFactors = FALSE)
samples <- list(Differential_Expression = samples)
study <- addSamples(study, samples)

# Features ---------------------------------------------------------------------

features <- read.delim("results/features.txt", stringsAsFactors = FALSE)
# The featureIDs in the first column must be a character vector
features$entrez <- as.character(features$entrez)
features <- list(Differential_Expression = features)
study <- addFeatures(study, features)

# MetaFeatures -----------------------------------------------------------------

metaFeatures <- read.delim("results/metaFeatures.txt", stringsAsFactors = FALSE,
                           colClasses = c(entrez = "character"))
metaFeatures <- list(Differential_Expression = metaFeatures)
study <- addMetaFeatures(study, metaFeatures)

# Assays -----------------------------------------------------------------------

assays <- read.delim("results/assays.txt", stringsAsFactors = FALSE)
assays <- list(Differential_Expression = assays)
study <- addAssays(study, assays)

# Results (differential expression) --------------------------------------------

# The featureID in the first column must be a character vector
BasalvsLP <- read.delim("results/BasalvsLP.txt", stringsAsFactors = FALSE,
                        colClasses = c(entrez = "character"))
BasalvsML <- read.delim("results/BasalvsML.txt", stringsAsFactors = FALSE,
                        colClasses = c(entrez = "character"))
LPvsML <- read.delim("results/LPvsML.txt", stringsAsFactors = FALSE,
                     colClasses = c(entrez = "character"))
results <- list(
  Differential_Expression = list(
    BasalvsLP = BasalvsLP,
    BasalvsML = BasalvsML,
    LPvsML = LPvsML
  )
)
study <- addResults(study, results)

# Tests ------------------------------------------------------------------------

tests <- list(
  Differential_Expression = list(
    BasalvsLP = "Which genes are DE between Basal and LP cells?",
    BasalvsML = "Which genes are DE between Basal and ML cells?",
    LPvsML = "Which genes are DE between LP and ML cells?"
  )
)
study <- addTests(study, tests)

# Linkouts to external resources for the results table -------------------------

resultsLinkouts <- list(
  Differential_Expression = list(
    entrez = c("https://www.ncbi.nlm.nih.gov/gene/",
               "https://ensembl.org/Mus_musculus/Gene/Summary?g="),
    symbol = "http://www.informatics.jax.org/searchtool/Search.do?query="
  )
)
study <- addResultsLinkouts(study, resultsLinkouts)


# Custom plots -----------------------------------------------------------------

x <- getPlottingData(study, modelID = "Differential_Expression", featureID = "497097")

#single feature
expression_by_cell_type <- function(x) {
  ggDataFrame <- cbind(x$samples,
                       feature = as.numeric(x$assays))
  p <- ggplot(ggDataFrame, aes(x = .data$group, y = .data$feature, fill = .data$group)) +
    geom_boxplot(alpha = .75) +
    labs(x = "Cell type", y = "Gene expression",
         title = sprintf("%s (Entrez %s)", x$features$symbol, x$features$entrez)) +
    scale_fill_viridis(discrete = TRUE, begin = .25) +
    theme_classic()
  ggplotly(p)
}
# expression_by_cell_type(x)
# p <- expression_by_cell_type(x)
# p


#multi-feature
IntFeatures <- study$results$Differential_Expression$BasalvsLP[1:10,1]
plottingData <- getPlottingData(study, modelID = "Differential_Expression", featureID = IntFeatures)

heatmap.custom <- function(plottingData){
  if (nrow(plottingData[["assays"]]) < 2) {
    stop("This plotting function requires at least 2 features")
  }
  plotMatrix <- as.matrix(plottingData$assays)
  colnames(plotMatrix) <- paste(plottingData$samples[['group']], plottingData$samples[['lane']], sep = "_")
  row.names(plotMatrix) <- plottingData$features$symbol
  heatmap.2(x = plotMatrix,
            trace = "none",
            key = TRUE,
            key.title = NA,
            key.xlab = "Gene Expression",
            key.ylab = NA,
            col = viridis(75),
            cexRow = 1.25,
            cexCol = 1.25,
            srtCol= 60,
            margins = c(9,8)
  )
}

#heatmap.custom(plottingData)

plots <- list(
  Differential_Expression = list(
    expression_by_cell_type = list(
      displayName = "Expression by cell type",
      plotType = c("singleFeature", "plotly"),
      packages = c("plotly", "ggplot2", "viridis")
    ),
    heatmap.custom = list(
      displayName = "Expression Heatmap",
      plotType = "multiFeature",
      packages = c("gplots", "viridis")
    )
  )
)
study <- addPlots(study, plots = plots)

p <- plotStudy(study, modelID = "Differential_Expression", featureID = "497097",
               plotID = "expression_by_cell_type")
plotStudy(study, modelID = "Differential_Expression", featureID = "27395",
          plotID = "expression_by_cell_type")
plotStudy(study, modelID = "Differential_Expression", featureID = IntFeatures,
          plotID = "heatmap.custom")


# Annotations (used for enrichments) -------------------------------------------

load("data/mouse_H_v5p2.rdata")
annotations <- list(
  mouse_H_v5p2 = list(
    terms = Mm.H,
    description = "The gene signatures from the Broad Institute's MSigDB Hallmark collection",
    featureID = "entrez"
  )
)
study <- addAnnotations(study, annotations)

# Enrichments ------------------------------------------------------------------

enrichedBasalvsLP <- read.delim("results/enrichedBasalvsLP.txt",
                                stringsAsFactors = FALSE)
enrichedBasalvsML <- read.delim("results/enrichedBasalvsML.txt",
                                stringsAsFactors = FALSE)
enrichedLPvsML <- read.delim("results/enrichedLPvsML.txt",
                             stringsAsFactors = FALSE)

create_enrichments <- function(x) {
  data.frame(
    termID = row.names(x),
    description = gsub("_", " ", tolower(row.names(x))),
    nominal = x$PValue,
    adjusted = x$FDR,
    stringsAsFactors = FALSE
  )
}
enrichments <- list(
  Differential_Expression = list(
    mouse_H_v5p2 = list(
      BasalvsLP = create_enrichments(enrichedBasalvsLP),
      BasalvsML = create_enrichments(enrichedBasalvsML),
      LPvsML = create_enrichments(enrichedLPvsML)
    )
  )
)
study <- addEnrichments(study, enrichments)

# Linkouts to external resources for the enrichments table ---------------------

enrichmentsLinkouts <- list(
  mouse_H_v5p2 = "https://www.gsea-msigdb.org/gsea/msigdb/cards/"
)
study <- addEnrichmentsLinkouts(study, enrichmentsLinkouts)

# Barcodes (for barcode plot of features enriched in a given annotation) -------

barcodes <- list(
  default = list(
    statistic = "t", # Use the t-statistics from the results table
    absolute = TRUE, # Take the absolute value of the t-statistics
    logFoldChange = "logFC", # Use the column "logFC" from the results table for violin plot
    labelStat = "abs(t)",
    labelLow = "Small effect",
    labelHigh = "Large effect"
  )
)
study <- addBarcodes(study, barcodes = barcodes)

# Reports ----------------------------------------------------------------------

reports <- list(
  Differential_Expression = "results/report.html"
)
study <- addReports(study, reports)

# Install study package and start app ------------------------------------------

installStudy(study)
message("Starting app. Should open in new browser tab.")
startApp()
