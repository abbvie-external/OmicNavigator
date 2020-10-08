# Test utility functions

# Setup ------------------------------------------------------------------------

source("tinytestSettings.R")
using(ttdo)

library(OmicAnalyzer)

tmplib <- tempfile()
dir.create(tmplib)

# Test isUrl() -----------------------------------------------------------------

testUrls <- c("http://somewhere.net", "https://secure.com/", "C:/path/to/file")

expect_identical_xl(
  OmicAnalyzer:::isUrl(testUrls),
  c(TRUE, TRUE, FALSE)
)
