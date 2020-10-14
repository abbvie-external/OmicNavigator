# Test utility functions

# Setup ------------------------------------------------------------------------

source("tinytestSettings.R")
using(ttdo)

library(OmicNavigator)

# Test isUrl() -----------------------------------------------------------------

testUrls <- c("http://somewhere.net", "https://secure.com/", "C:/path/to/file")

expect_identical_xl(
  OmicNavigator:::isUrl(testUrls),
  c(TRUE, TRUE, FALSE)
)
