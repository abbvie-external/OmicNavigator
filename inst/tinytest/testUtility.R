# Test utility functions

# Setup ------------------------------------------------------------------------

source("tinytestSettings.R")
using(ttdo)

library(OmicNavigator)

# Test IO ----------------------------------------------------------------------

x <- data.frame(
  int = 1:3,
  num = c(1.2, -3.9, 10.0),
  char = letters[1:3],
  stringsAsFactors = FALSE
)
f <- tempfile()
OmicNavigator:::writeTable(x, f)

expect_equal_xl(
  OmicNavigator:::readTable(f),
  x,
  info = "Data is preserved wen writing and reading plain text files"
)

x <- data.frame(
  int = c(1:3, NA),
  num = c(1.2, -3.9, 10.0, NA),
  char = c(letters[1:3], NA),
  stringsAsFactors = FALSE
)
f <- tempfile()
OmicNavigator:::writeTable(x, f)

expect_equal_xl(
  OmicNavigator:::readTable(f),
  x,
  info = "Missing values are preserved when writing and reading plain text files"
)

# Test isUrl() -----------------------------------------------------------------

testUrls <- c("http://somewhere.net", "https://secure.com/", "C:/path/to/file")

expect_identical_xl(
  OmicNavigator:::isUrl(testUrls),
  c(TRUE, TRUE, FALSE)
)
