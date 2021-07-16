# Test overlap functions

# Setup ------------------------------------------------------------------------

source("tinytestSettings.R")
using(ttdo)

library(OmicNavigator)

# Test calc_overlap() ----------------------------------------------------------

expect_equal_xl(
  OmicNavigator:::calc_overlap(letters[1:3], letters[4:6]),
  list(overlapSize = 0, overlap = 0, jaccard = 0)
)

expect_equal_xl(
  OmicNavigator:::calc_overlap(letters[1:6], letters[4:6]),
  list(overlapSize = 3, overlap = 1, jaccard = 1/2)
)

expect_equal_xl(
  OmicNavigator:::calc_overlap(letters[1:6], letters[4:9]),
  list(overlapSize = 3, overlap = 1/2, jaccard = 1/3)
)

# Test calc_pairwise_overlaps() ------------------------------------------------

sets <- list(
  a = letters[1:6],
  b = letters[4:6],
  c = letters[4:9]
)

expect_equal_xl(
  OmicNavigator:::calc_pairwise_overlaps(sets),
  data.frame(
    term1 = c("a", "a", "b"),
    term2 = c("b", "c", "c"),
    overlapSize = c(3, 3, 3),
    overlap = c(1, 1/2, 1),
    jaccard = c(1/2, 1/3, 1/2)
  )
)
