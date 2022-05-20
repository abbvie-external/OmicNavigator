# Test combineStudies()

# Setup ------------------------------------------------------------------------

source("tinytestSettings.R")
using(ttdo)

library(OmicNavigator)

# combineStudies ---------------------------------------------------------------

# Combining two empty studies returns an empty study
emptyOne <- createStudy("emptyOne")
emptyTwo <- createStudy("emptyTwo")
combinedEmpty <- c(emptyOne, emptyTwo)
expect_identical_xl(emptyTwo, combinedEmpty)

# Combining two full studies returns a full study
fullOne <- OmicNavigator:::testStudy("fullOne")
fullTwo <- OmicNavigator:::testStudy("fullTwo")
combinedFull <- c(fullOne, fullTwo)
expect_identical_xl(fullTwo, combinedFull)

# Combining an empty study with a full study returns full study
combinedEmptyFull <- c(emptyOne, fullOne)
expect_identical_xl(fullOne, combinedEmptyFull)

# Combining a full study with an empty study returns full study with new name
combinedFullEmpty <- c(fullOne, emptyOne)
expect_identical_xl(fullOne[-1:-2], combinedFullEmpty[-1:-2])
expect_identical_xl(emptyOne[["name"]], combinedFullEmpty[["name"]])

# Plots are kept from first study
studyWithPlots <- OmicNavigator:::testStudy("studyWithPlots")
studyWithPlots <- addPlots(studyWithPlots, OmicNavigator:::testPlots())
combinedPlotsFirst <- c(studyWithPlots, fullOne)
expect_identical_xl(studyWithPlots[["plots"]], combinedPlotsFirst[["plots"]])

# Plots are kept from second study
combinedPlotsSecond <- c(fullOne, studyWithPlots)
expect_identical_xl(studyWithPlots[["plots"]], combinedPlotsSecond[["plots"]])

# More than one study is required
expect_error_xl(
  c(fullOne),
  "combineStudies requires two or more onStudy objects as input"
)

# All objects must be study objects
expect_error_xl(c(fullOne, list(a = 1)))

# Combine three studies
studyOne <- createStudy(name = "One",
                       description = "First study",
                       studyMeta = list(metafield1 = "metavalue1"))
studyTwo <- createStudy(name = "Two",
                       description = "Second study",
                       maintainer = "The Maintainer",
                       studyMeta = list(metafield2 = "metavalue2"))
studyThree <- createStudy(name = "Three",
                         description = "Third study",
                         studyMeta = list(metafield3 = "metavalue3"))
combinedThree <- combineStudies(studyOne, studyTwo, studyThree)
expect_identical_xl(studyThree[["name"]], combinedThree[["name"]])
expect_identical_xl(studyTwo[["maintainer"]], combinedThree[["maintainer"]])
expect_null_xl(combinedThree[["version"]])
expect_null_xl(combinedThree[["maintainerEmail"]])
expect_identical_xl(
  list(metafield1 = "metavalue1",
       metafield2 = "metavalue2",
       metafield3 = "metavalue3"),
  combinedThree[["studyMeta"]]
)

# Combine studies with different modelIDs
omicOne <- createStudy("transcriptomics")
resultsTranscriptomics <- data.frame(id = paste0("t", 1:3), pval = runif(3),
                                     stringsAsFactors = FALSE)
omicOne <- addResults(omicOne,
                      list(transcriptomics = list(test = resultsTranscriptomics)))
omicTwo <- createStudy("proteomics")
resultsProteomics <- data.frame(id = paste0("p", 1:3), pval = runif(3),
                                     stringsAsFactors = FALSE)
omicTwo <- addResults(omicTwo,
                      list(proteomics = list(test = resultsProteomics)))
multiOmics <- c(omicOne, omicTwo)
expect_identical_xl(
  sort(c("transcriptomics", "proteomics")),
  sort(names(multiOmics[["results"]]))
)
expect_equal_xl(
  resultsTranscriptomics,
  getResults(multiOmics, modelID = "transcriptomics", testID = "test")
)
expect_equal_xl(
  resultsProteomics,
  getResults(multiOmics, modelID = "proteomics", testID = "test")
)

# Combine studies with the same modelIDs - only the data from last study is kept
study1 <- OmicNavigator:::testStudy("study1")
study2 <- OmicNavigator:::testStudy("study2")
study2[["results"]][["model_01"]][["test_01"]][1, 2] <- 1
study2[["enrichments"]][["model_03"]][["annotation_02"]][["test_02"]][1, 3] <- 0.5

studyCombined <- combineStudies(study1, study2)

expect_equal_xl(
  studyCombined[["results"]][["model_01"]][["test_01"]],
  study2[["results"]][["model_01"]][["test_01"]]
)

expect_equal_xl(
  studyCombined[["enrichments"]][["model_03"]][["annotation_02"]][["test_02"]],
  study2[["enrichments"]][["model_03"]][["annotation_02"]][["test_02"]]
)

# Combine studies with the same modelIDs but different columns in each table.
# Things get weird because data frames are technically lists, so the unique
# columns are maintained by utils::modifyList()
study1 <- OmicNavigator:::testStudy("study1")
study2 <- OmicNavigator:::testStudy("study2")
study1[["results"]][["model_01"]][["test_01"]][["new"]] <- 1
study2[["results"]][["model_01"]][["test_01"]][1, 2] <- 1

studyCombined <- combineStudies(study1, study2)

expect_equal_xl(
  studyCombined[["results"]][["model_01"]][["test_01"]],
  cbind(
    study2[["results"]][["model_01"]][["test_01"]],
    new = study1[["results"]][["model_01"]][["test_01"]][["new"]]
  )
)
