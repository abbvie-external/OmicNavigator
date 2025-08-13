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
libOrig <- .libPaths()
.libPaths(c(tmplib, libOrig))

tmplibSpace <- file.path(tempdir(), "a space")
dir.create(tmplibSpace)
tmplibQuote <- file.path(tempdir(), "project's results")
dir.create(tmplibQuote)

# Export with special encoding description ------------------------------------

specialDesc <- "β‐catenin and neural cell adhesion molecule (NCAM)"
specialTestStudyObj <- OmicNavigator:::testStudy(name = "ABCspecial",
                                                 description = specialDesc)

specialPath <- exportStudy(specialTestStudyObj, type = "package", path = tmplib)
observed <- read.dcf(file.path(specialPath, "DESCRIPTION"), fields = "Description")
expect_identical_xl(as.character(observed), specialDesc,
                    info = "Export special character in description field")

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
observed <- exportStudy(minimalStudyObj, type = "package", path = tmplib)
expected <- file.path(tmplib, OmicNavigator:::studyToPkg(minimalStudyName))
expect_identical_xl(observed, expected,
                    info = "Export minimal study as package directory")
expect_true_xl(dir.exists(expected))

# Export as package tarball ----------------------------------------------------

# These tests fail on all CRAN macOS machines and most CRAN Linux machines. I
# have no idea why. They also fail in GitHub Actions. The call to `R CMD build`
# looks fine, so I don't know what more I could do on my end to fix the failed
# tarball creation. They are only tested locally when running
# tinytest::test_all() or tinytest::run_test_file()

if (at_home()) {
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
  expect_message_xl(
    tarball <- exportStudy(minimalStudyObj, type = "tarball", path = tmplib),
    "Note: No maintainer email was specified. Using the placeholder: "
  )
  expect_true_xl(file.exists(tarball))
  expect_true_xl(startsWith(tarball, tmplib))
  directoryname <- file.path(tmplib, OmicNavigator:::studyToPkg(minimalStudyName))
  expect_false_xl(dir.exists(directoryname))

  # Return warning message if package fails to build
  pkgDir <- tempfile(pattern = OmicNavigator:::getPrefix())
  dir.create(pkgDir, showWarnings = FALSE, recursive = TRUE)
  # Use invalid package name to trigger build failure
  writeLines("Package: 1pkg", con = file.path(pkgDir, "DESCRIPTION"))

  expect_warning_xl(
    OmicNavigator:::buildPkg(pkgDir),
    "Malformed package name",
    info = "Return warning message for failed package build"
  )

  # Fix the problematic package name
  writeLines("Package: onepkg", con = file.path(pkgDir, "DESCRIPTION"))

  expect_silent_xl(
    tarball <- OmicNavigator:::buildPkg(pkgDir)
  )
  expect_identical_xl(tarball, "onepkg_NA.tar.gz")
  # note: The NA in the tarball name is because I only provided this minimal
  # package with a name but not a version in DESCRIPTION

  unlink(pkgDir, recursive = TRUE, force = TRUE)
  file.remove(tarball)
}

expect_identical_xl(
  OmicNavigator:::extractTarballName("* building 'ONstudyABC_0.0.0.9000.tar.gz'"),
  "ONstudyABC_0.0.0.9000.tar.gz"
)

expect_identical_xl(
  OmicNavigator:::extractTarballName("* building 'onepkg_NA.tar.gz'"),
  "onepkg_NA.tar.gz"
)

expect_identical_xl(
  OmicNavigator:::extractTarballName("* building 'OmicNavigator_1.16.0.tar.gz'"),
  "OmicNavigator_1.16.0.tar.gz"
)

expect_warning_xl(
  OmicNavigator:::extractTarballName(""),
  "Unable to determine name of tarball after build"
)

# Check package metadata -------------------------------------------------------

suppressMessages(installStudy(testStudyObj, library = tmplib))
studyMetadata <- listStudies(libraries = tmplib)

expect_identical_xl(
  studyMetadata[[1]][["name"]],
  testStudyObj[["name"]]
)

expect_identical_xl(
  studyMetadata[[1]][["package"]][["Description"]],
  sprintf("The OmicNavigator data package for the study \"%s\"",
          testStudyObj[["description"]]),
  info = "Default package description when description==name"
)

expect_identical_xl(
  studyMetadata[[1]][["package"]][["Version"]],
  "0.0.0.9000",
  info = "Default package version used when version=NULL"
)

expect_identical_xl(
  studyMetadata[[1]][["package"]][["Maintainer"]],
  "Unknown <unknown@unknown>",
  info = "Default package maintainer"
)

expect_identical_xl(
  studyMetadata[[1]][["package"]][["department"]],
  "immunology",
  info = "Custom study metadata passed via studyMeta"
)

expect_identical_xl(
  studyMetadata[[1]][["package"]][["organism"]],
  "Mus musculus",
  info = "Custom study metadata passed via studyMeta"
)

updatedStudyObj <- testStudyObj
updatedStudyObj[["description"]] <- "A custom description"
updatedStudyObj[["version"]] <- "1.0.0"
updatedStudyObj[["maintainer"]] <- "My Name"
updatedStudyObj[["maintainerEmail"]] <- "me@email.com"

suppressMessages(installStudy(updatedStudyObj, library = tmplib))
studyMetadata <- listStudies(libraries = tmplib)

expect_identical_xl(
  studyMetadata[[1]][["name"]],
  updatedStudyObj[["name"]]
)

expect_identical_xl(
  studyMetadata[[1]][["package"]][["Description"]],
  updatedStudyObj[["description"]]
)

expect_identical_xl(
  studyMetadata[[1]][["package"]][["Version"]],
  updatedStudyObj[["version"]]
)

expect_identical_xl(
  studyMetadata[[1]][["package"]][["Maintainer"]],
  sprintf("%s <%s>", updatedStudyObj[["maintainer"]],
          updatedStudyObj[["maintainerEmail"]])
)

# Remove installed study -------------------------------------------------------

studyToRemoveName <- "remove"
studyToRemove <- OmicNavigator:::testStudy(name = studyToRemoveName)
suppressMessages(installStudy(studyToRemove, library = tmplib))
studyToRemoveDir <- file.path(tmplib, OmicNavigator:::studyToPkg(studyToRemoveName))

expect_true_xl(dir.exists(studyToRemoveDir))

expect_message_xl(
  removeStudy(studyToRemove, library = tmplib),
  studyToRemoveName
)

expect_false_xl(dir.exists(studyToRemoveDir))

# Error handling
expect_error_xl(
  removeStudy(removeStudyName, library = "xyz"),
  "This directory does not exist: xyz"
)

expect_error_xl(
  removeStudy(1),
  "Argument `study` must be a string or an onStudy object"
)

expect_error_xl(
  removeStudy("nonExistent"),
  "Couldn't find"
)

# Install invalid study object -------------------------------------------------

# Create invalid study due to an invalid column name for featureID
invalidStudy <- testStudyObj
colnames(invalidStudy[["results"]][[1]][[1]])[1] <- "wrongFeatureID"
expect_error_xl(validateStudy(invalidStudy))

expect_error_xl(installStudy(invalidStudy, library = tmplib))
expect_silent_xl(installStudy(invalidStudy, requireValid = FALSE, library = tmplib))

# Shared plotting functions across models --------------------------------------

# In general it is best practice to use `modelID = "default"` to share custom
# plotting functions across models. However, if you have more complex
# requirements (eg share across only a subset of the models), or prefer to be
# more explicit, it is now possible to share plotting functions.

# This example study shares a plot across models 1 and 2 but not 3
sharedPlot <- function(x) plot(1:10)
sharedPlotList <- list(
  model_01 = list(
    sharedPlot = list(
      displayName = "Plot for model 1",
      plotType = "singleFeature"
    )
  ),
  model_02 = list(
    sharedPlot = list(
      displayName = "Plot for model 2",
      plotType = "singleFeature"
    )
  )
)
studyWithSharedPlots <- OmicNavigator:::testStudy(name = "sharedPlots")
studyWithSharedPlots <- addPlots(studyWithSharedPlots, sharedPlotList)
tmpExportDir <- tempfile()
exportStudy(studyWithSharedPlots, type = "package", path = tmpExportDir)

# Ensure only a single copy of the plot function is exported to the package
namespace <- readLines(file.path(tmpExportDir, "ONstudysharedPlots", "NAMESPACE"))
expect_identical_xl(namespace, "export(sharedPlot)")
plotsr <- readLines(file.path(tmpExportDir, "ONstudysharedPlots", "R", "plots.R"))
expect_identical_xl(plotsr, c("sharedPlot <- function (x) ", "plot(1:10)"))

# Ensure the plot is available for models 1 and 2 but not 3
installStudy(studyWithSharedPlots)
rm(sharedPlot) # Not necessary; just to be extra safe package function is called
plotStudy("sharedPlots", modelID = "model_01", featureID = "feature_0001", plotID = "sharedPlot")
plotStudy("sharedPlots", modelID = "model_02", featureID = "feature_0001", plotID = "sharedPlot")
expect_error_xl(
  plotStudy("sharedPlots", modelID = "model_03", featureID = "feature_0001", plotID = "sharedPlot"),
  'The plot "sharedPlot" is not available.'
)

# Teardown ---------------------------------------------------------------------

# todo: plotStudy() should unload study package namespace if it wasn't already loaded
unloadNamespace("ONstudysharedPlots")
unlink(c(tmplib, tmplibSpace, tmplibQuote, tmpExportDir), recursive = TRUE, force = TRUE)
.libPaths(libOrig)
