# Test importStudy()

# Setup ------------------------------------------------------------------------

source("tinytestSettings.R")
using(ttdo)

library(OmicNavigator)

testStudyName <- "ABC"
testStudyObj <- OmicNavigator:::testStudy(
  name = testStudyName,
  version = "0.1.0",
  maintainer = "My Name",
  maintainerEmail = "me@domain.com"
)
testStudyObj <- addPlots(testStudyObj, OmicNavigator:::testPlots())
testModelName <- names(testStudyObj[["models"]])[1]
testTestName <- names(testStudyObj[["tests"]][[1]])[1]
testAnnotationName <- names(testStudyObj[["annotations"]])[1]

minimalStudyObj <- OmicNavigator:::testStudyMinimal()
minimalStudyName <- minimalStudyObj[["name"]]

emptyStudyObj <- createStudy(name = "empty", description = "An empty study")
emptyStudyName <- emptyStudyObj[["name"]]

assaysOnlyStudyObj <- createStudy(name = "assaysOnly", description = "A study with only assays")
assaysOnlyStudyName <- assaysOnlyStudyObj[["name"]]
assaysOnlyStudyObj <- addAssays(assaysOnlyStudyObj, OmicNavigator:::testAssays())

tmplib <- tempfile()
dir.create(tmplib)
libOrig <- .libPaths()
.libPaths(c(tmplib, libOrig))
suppressMessages(installStudy(testStudyObj))
suppressMessages(installStudy(minimalStudyObj))
suppressMessages(installStudy(emptyStudyObj))
suppressMessages(installStudy(assaysOnlyStudyObj))

# importStudy ------------------------------------------------------------------

imported <- importStudy(testStudyName)

expect_identical_xl(
  imported[["name"]],
  testStudyObj[["name"]]
)

expect_identical_xl(
  imported[["samples"]],
  testStudyObj[["samples"]]
)

expect_identical_xl(
  imported[["features"]],
  testStudyObj[["features"]]
)

expect_identical_xl(
  imported[["models"]],
  testStudyObj[["models"]]
)

expect_equal_xl(
  imported[["assays"]],
  testStudyObj[["assays"]]
)

expect_identical_xl(
  imported[["tests"]],
  testStudyObj[["tests"]]
)

expect_identical_xl(
  imported[["annotations"]],
  testStudyObj[["annotations"]]
)

expect_equal_xl(
  imported[["results"]],
  testStudyObj[["results"]]
)

expect_equal_xl(
  imported[["enrichments"]],
  testStudyObj[["enrichments"]]
)

expect_identical_xl(
  imported[["metaFeatures"]],
  testStudyObj[["metaFeatures"]]
)

expect_identical_xl(
  imported[["plots"]],
  testStudyObj[["plots"]]
)

expect_identical_xl(
  imported[["barcodes"]],
  testStudyObj[["barcodes"]]
)

expect_identical_xl(
  imported[["reports"]],
  testStudyObj[["reports"]],
  info = "These are currently fake URLs. I think files should also work, but not tested"
)

expect_identical_xl(
  imported[["resultsLinkouts"]],
  testStudyObj[["resultsLinkouts"]]
)

expect_identical_xl(
  imported[["enrichmentsLinkouts"]],
  testStudyObj[["enrichmentsLinkouts"]]
)

expect_identical_xl(
  imported[["metaFeaturesLinkouts"]],
  testStudyObj[["metaFeaturesLinkouts"]]
)

expect_equal_xl(
  imported[["metaAssays"]],
  testStudyObj[["metaAssays"]]
)

expect_identical_xl(
  imported[["version"]],
  testStudyObj[["version"]]
)

expect_identical_xl(
  imported[["maintainer"]],
  testStudyObj[["maintainer"]]
)

expect_identical_xl(
  imported[["maintainerEmail"]],
  testStudyObj[["maintainerEmail"]]
)

# The -1 removes the OmicNavigatorVersion from the imported studyMeta
expect_identical_xl(
  imported[["studyMeta"]][-1],
  testStudyObj[["studyMeta"]]
)

# importStudy() - minimal ------------------------------------------------------

importedMinimal <- importStudy(minimalStudyName)

expect_identical_xl(
  importedMinimal[["name"]],
  minimalStudyObj[["name"]]
)

expect_identical_xl(
  importedMinimal[["samples"]],
  minimalStudyObj[["samples"]]
)

expect_identical_xl(
  importedMinimal[["features"]],
  minimalStudyObj[["features"]]
)

expect_identical_xl(
  importedMinimal[["models"]],
  minimalStudyObj[["models"]]
)

expect_equal_xl(
  importedMinimal[["assays"]],
  minimalStudyObj[["assays"]]
)

expect_identical_xl(
  importedMinimal[["tests"]],
  minimalStudyObj[["tests"]]
)

expect_identical_xl(
  importedMinimal[["annotations"]],
  minimalStudyObj[["annotations"]]
)

expect_equal_xl(
  importedMinimal[["results"]],
  minimalStudyObj[["results"]]
)

expect_equal_xl(
  importedMinimal[["enrichments"]],
  minimalStudyObj[["enrichments"]]
)

expect_identical_xl(
  importedMinimal[["metaFeatures"]],
  minimalStudyObj[["metaFeatures"]]
)

expect_identical_xl(
  importedMinimal[["plots"]],
  minimalStudyObj[["plots"]]
)

expect_identical_xl(
  importedMinimal[["barcodes"]],
  minimalStudyObj[["barcodes"]]
)

expect_identical_xl(
  importedMinimal[["reports"]],
  minimalStudyObj[["reports"]],
  info = "These are currently fake URLs. I think files should also work, but not tested"
)

expect_identical_xl(
  importedMinimal[["resultsLinkouts"]],
  minimalStudyObj[["resultsLinkouts"]]
)

expect_identical_xl(
  importedMinimal[["enrichmentsLinkouts"]],
  minimalStudyObj[["enrichmentsLinkouts"]]
)

expect_identical_xl(
  importedMinimal[["metaFeaturesLinkouts"]],
  minimalStudyObj[["metaFeaturesLinkouts"]]
)

expect_identical_xl(
  importedMinimal[["version"]],
  "0.0.0.9000"
)

expect_identical_xl(
  importedMinimal[["maintainer"]],
  "Unknown"
)

expect_identical_xl(
  importedMinimal[["maintainerEmail"]],
  "unknown@unknown"
)

# importStudy - empty ----------------------------------------------------------

importedEmpty <- importStudy(emptyStudyName)

# These elements are assigned a default value on export
elementsWithDefaults <- c("version", "maintainer", "maintainerEmail", "studyMeta")

expect_identical_xl(
  importedEmpty[!names(importedEmpty) %in% elementsWithDefaults],
  emptyStudyObj[!names(emptyStudyObj) %in% elementsWithDefaults]
)

# importStudy - assays only ----------------------------------------------------

importedAssaysOnly <- importStudy(assaysOnlyStudyName)

expect_equal_xl(
  importedAssaysOnly[["assays"]],
  assaysOnlyStudyObj[["assays"]]
)

# Teardown ---------------------------------------------------------------------

unlink(tmplib, recursive = TRUE, force = TRUE)
.libPaths(libOrig)
