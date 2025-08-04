# Test optional extra metadata capabilities

# Setup ------------------------------------------------------------------------

source("tinytestSettings.R")
using(ttdo)

library(OmicNavigator)

testStudyName <- "toyCrispr"
testStudyObj <- createStudy(testStudyName, description = "Test metaAssays")

tmplib <- tempfile()
dir.create(tmplib)
libOrig <- .libPaths()
.libPaths(c(tmplib, libOrig))

# Add toy CRISPR data ----------------------------------------------------------

# 1 model
# 5 samples
# 3 genes
# 4 guides
#
# guide:gene relationships:
#
# guide1 -> gene1
# guide2 -> gene2
# guide3 -> gene2
# guide3 -> gene3
# guide4 -> gene3

features <- data.frame(
  featureID = c("gene1", "gene2", "gene3"),
  name = c("1st gene", "2nd gene", "3rd gene")
)

metaFeatures <- data.frame(
  featureID = c("gene1", "gene2", "gene2", "gene3", "gene3"),
  metaFeatureID = c("guide1", "guide2", "guide3", "guide3", "guide4"),
  name = c("1st guide", "2nd guide", "3rd guide", "3rd guide", "4th guide")
)

set.seed(1)
metaAssays <- matrix(rnorm(4 * 5), nrow = 4, ncol = 5)
rownames(metaAssays) <- sprintf("guide%d", 1:4)
colnames(metaAssays) <- sprintf("sample%d", 1:5)
metaAssays <- as.data.frame(metaAssays)

# Gene measurements are averages of guide measurements
assays <- matrix(nrow = 3, ncol = 5)
rownames(assays) <- sprintf("gene%d", 1:3)
colnames(assays) <- sprintf("sample%d", 1:5)
assays["gene1", ] <- colMeans(metaAssays[c("guide1"), , drop = FALSE])
assays["gene2", ] <- colMeans(metaAssays[c("guide2", "guide3"), , drop = FALSE])
assays["gene3", ] <- colMeans(metaAssays[c("guide3", "guide4"), , drop = FALSE])
assays <- as.data.frame(assays)

testStudyObj <- addFeatures(testStudyObj, list(crispr = features))
testStudyObj <- addAssays(testStudyObj, list(crispr = assays))
testStudyObj <- addMetaFeatures(testStudyObj, list(crispr = metaFeatures))
testStudyObj <- addMetaAssays(testStudyObj, list(crispr = metaAssays))

x <- getPlottingData(testStudyObj, modelID = "crispr", featureID = "gene2")

expect_identical_xl(
  names(x),
  c("assays", "samples", "features", "metaFeatures", "metaAssays")
)
expect_identical(row.names(x$assays), "gene2")
expect_identical(row.names(x$metaAssays), c("guide2", "guide3"))


plotGeneWithGuides <- function(x) {
  geneWithGuides <- rbind(x[["assays"]], x[["metaAssays"]])
  geneName <- x[["features"]][["name"]]
  guides <- x[["metaFeatures"]][[2]]
  title <- sprintf("%s (%s)", geneName, paste(guides, collapse = ", "))
  boxplot(t(geneWithGuides), main = title)
}
plotGeneWithGuides(x)

plots = list(
  crispr = list(
    plotGeneWithGuides = list(
      displayName = "Boxplot",
      description = "Boxplot of averaged gene measurement alongside inidividual guide measurements"
    )
  )
)
testStudyObj <- addPlots(testStudyObj, plots)

plotStudy(testStudyObj, modelID = "crispr", featureID = "gene1", plotID = "plotGeneWithGuides")
plotStudy(testStudyObj, modelID = "crispr", featureID = "gene2", plotID = "plotGeneWithGuides")
plotStudy(testStudyObj, modelID = "crispr", featureID = "gene3", plotID = "plotGeneWithGuides")

# Export and import metaAssays -------------------------------------------------

suppressMessages(installStudy(testStudyObj, requireValid = FALSE))
metaAssaysFromPkg <- getMetaAssays(testStudyName, modelID = "crispr")

expect_equal_xl(metaAssaysFromPkg, metaAssays)

# Teardown ---------------------------------------------------------------------

testPkgName <- paste0(OmicNavigator:::getPrefix(), testStudyName)
unloadNamespace(testPkgName)
unlink(tmplib, recursive = TRUE, force = TRUE)
.libPaths(libOrig)
