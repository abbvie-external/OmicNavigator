# Test backwards compatibility of study packages created with previous versions

# Setup ------------------------------------------------------------------------

source("tinytestSettings.R")
using(ttdo)

library(OmicNavigator)

tmplib <- tempfile()
dir.create(tmplib)

tarballs <- Sys.glob("backwardsCompatibility/*.tar.gz")
for (tball in tarballs) {
  install.packages(tball, lib = tmplib, repos = NULL, quiet = TRUE)
}

# Import and validate  ---------------------------------------------------------

studies <- getInstalledStudies(libraries = tmplib)

for (study in studies) {
  expect_silent_xl(
    x <- importStudy(study, libraries = tmplib),
    info = sprintf("Unable to import legacy study %s", study)
  )
  expect_true_xl(
    validateStudy(x),
    info = sprintf("Legacy study %s is invalid", study)
  )
}

# Teardown ---------------------------------------------------------------------

unlink(tmplib, recursive = TRUE, force = TRUE)
