# Test exportStudy()

# Setup ------------------------------------------------------------------------

source("tinytestSettings.R")
using(ttdo)

library(OmicNavigator)

testStudyName <- "ABC"
testStudyObj <- OmicNavigator:::testStudy(name = testStudyName)
testStudyObj <- addPlots(testStudyObj, OmicNavigator:::testPlots())
minimalStudyObj <- OmicNavigator:::testStudyMinimal()
minimalStudyName <- minimalStudyObj[["name"]]

tmplib <- tempfile()
dir.create(tmplib)

# Export as package directory --------------------------------------------------

observed <- exportStudy(testStudyObj, type = "package", path = tmplib)
expected <- file.path(tmplib, OmicNavigator:::studyToPkg(testStudyName))
expect_identical_xl(observed, expected)
expect_true_xl(dir.exists(expected))

suppressWarnings(
  observed <- exportStudy(minimalStudyObj, type = "package", path = tmplib)
)
expected <- file.path(tmplib, OmicNavigator:::studyToPkg(minimalStudyName))
expect_identical_xl(observed, expected)
expect_true_xl(dir.exists(expected))

# Export as package tarball ----------------------------------------------------

tarball <- exportStudy(testStudyObj, type = "tarball", path = tmplib)
expect_true_xl(file.exists(tarball))
expect_true_xl(startsWith(tarball, tmplib))
directoryname <- file.path(tmplib, OmicNavigator:::studyToPkg(testStudyName))
expect_false_xl(dir.exists(directoryname))

suppressWarnings(
  tarball <- exportStudy(minimalStudyObj, type = "tarball", path = tmplib)
)
expect_true_xl(file.exists(tarball))
expect_true_xl(startsWith(tarball, tmplib))
directoryname <- file.path(tmplib, OmicNavigator:::studyToPkg(minimalStudyName))
expect_false_xl(dir.exists(directoryname))

# Teardown ---------------------------------------------------------------------

unlink(tmplib, recursive = TRUE, force = TRUE)
