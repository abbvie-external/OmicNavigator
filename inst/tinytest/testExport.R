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
tmplibSpace <- file.path(tempdir(), "a space")
dir.create(tmplibSpace)
tmplibQuote <- file.path(tempdir(), "project's results")
dir.create(tmplibQuote)

# Export as package directory --------------------------------------------------

observed <- exportStudy(testStudyObj, type = "package", path = tmplib)
expected <- file.path(tmplib, OmicNavigator:::studyToPkg(testStudyName))
expect_identical_xl(observed, expected, info = "Export as package directory")
expect_true_xl(dir.exists(expected))

# Export to a directory with a space
observed <- exportStudy(testStudyObj, type = "package", path = tmplibSpace)
expected <- file.path(tmplibSpace, OmicNavigator:::studyToPkg(testStudyName))
expect_identical_xl(observed, expected,
                    info = "Export as package directory to path with a space")
expect_true_xl(dir.exists(expected))

# Export to a directory with a single quote
observed <- exportStudy(testStudyObj, type = "package", path = tmplibQuote)
expected <- file.path(tmplibQuote, OmicNavigator:::studyToPkg(testStudyName))
expect_identical_xl(observed, expected,
                    info = "Export as package directory to path with a quote")
expect_true_xl(dir.exists(expected))

# Export minimal study
suppressWarnings(
  observed <- exportStudy(minimalStudyObj, type = "package", path = tmplib)
)
expected <- file.path(tmplib, OmicNavigator:::studyToPkg(minimalStudyName))
expect_identical_xl(observed, expected,
                    info = "Export minimal study as package directory")
expect_true_xl(dir.exists(expected))

# Export as package tarball ----------------------------------------------------

tarball <- exportStudy(testStudyObj, type = "tarball", path = tmplib)
expect_true_xl(file.exists(tarball))
expect_true_xl(startsWith(tarball, tmplib))
directoryname <- file.path(tmplib, OmicNavigator:::studyToPkg(testStudyName))
expect_false_xl(dir.exists(directoryname))

# Confirm tarball is overwritten when re-exported
modtimeOriginal <- file.mtime(tarball)
tarball <- exportStudy(testStudyObj, type = "tarball", path = tmplib)
modtimeReexport <- file.mtime(tarball)
expect_true_xl(
  modtimeReexport > modtimeOriginal,
  info = "Confirm tarball is overwritten when re-exported"
)

# Export to a directory with a space
tarball <- exportStudy(testStudyObj, type = "tarball", path = tmplibSpace)
expect_true_xl(file.exists(tarball))
expect_true_xl(startsWith(tarball, tmplibSpace))
directoryname <- file.path(tmplibSpace, OmicNavigator:::studyToPkg(testStudyName))
expect_false_xl(dir.exists(directoryname))

# Export to a directory with a single quote
tarball <- exportStudy(testStudyObj, type = "tarball", path = tmplibQuote)
expect_true_xl(file.exists(tarball))
expect_true_xl(startsWith(tarball, tmplibQuote))
directoryname <- file.path(tmplibQuote, OmicNavigator:::studyToPkg(testStudyName))
expect_false_xl(dir.exists(directoryname))

# Export minimal study
suppressWarnings(
  tarball <- exportStudy(minimalStudyObj, type = "tarball", path = tmplib)
)
expect_true_xl(file.exists(tarball))
expect_true_xl(startsWith(tarball, tmplib))
directoryname <- file.path(tmplib, OmicNavigator:::studyToPkg(minimalStudyName))
expect_false_xl(dir.exists(directoryname))

# Teardown ---------------------------------------------------------------------

unlink(c(tmplib, tmplibSpace, tmplibQuote), recursive = TRUE, force = TRUE)
