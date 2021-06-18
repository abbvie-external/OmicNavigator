# Test app endpoints

# Setup ------------------------------------------------------------------------

source("tinytestSettings.R")
using(ttdo)

library(OmicNavigator)

testStudyName <- "ABC"
testStudyObj <- OmicNavigator:::testStudy(name = testStudyName, version = "0.3")
testStudyObj <- addPlots(testStudyObj, OmicNavigator:::testPlots())
testModelName <- names(testStudyObj[["models"]])[1]
testTestName <- names(testStudyObj[["tests"]][[1]])[1]
testAnnotationName <- names(testStudyObj[["annotations"]])[1]
testTermName <- names(testStudyObj[["annotations"]][[testAnnotationName]][["terms"]])[1]

# Add a report file
tmpReport <- tempfile(fileext = ".html")
writeLines("<p>example</p>", tmpReport)
testStudyObj <- addReports(testStudyObj, list(model_02 = tmpReport))

tmplib <- tempfile()
dir.create(tmplib)
libOrig <- .libPaths()
.libPaths(c(tmplib, libOrig))
suppressMessages(installStudy(testStudyObj))

# listStudies ------------------------------------------------------------------

studies <- listStudies(libraries = tmplib)

expect_identical_xl(
  length(studies),
  1L
)

expect_identical_xl(
  studies[[1]][["name"]],
  testStudyName
)

expect_identical_xl(
  names(studies[[1]]),
  c("name", "package", "results", "enrichments", "plots")
)

expect_identical_xl(
  studies[[1]][["package"]][["OmicNavigatorVersion"]],
  as.character(utils::packageVersion("OmicNavigator"))
)

expect_identical_xl(
  c("Package", "Title", "Version", "Maintainer", "Description", "OmicNavigatorVersion",
    names(OmicNavigator:::testStudyMeta()), "Imports", "Built", "description"),
  names(studies[[1]][["package"]]),
  info = "listStudies() returns DESCRIPTION"
)

expect_identical_xl(
  vapply(studies[[1]][["results"]], function(x) x[["modelID"]], character(1)),
  names(getModels(testStudyObj))
)

expect_identical_xl(
  vapply(studies[[1]][["results"]][[1]][["tests"]],
         function(x) x[["testID"]], character(1)),
  names(getTests(testStudyObj, modelID = testModelName))
)

expect_identical_xl(
  vapply(studies[[1]][["enrichments"]], function(x) x[["modelID"]], character(1)),
  names(getModels(testStudyObj))
)

expect_identical_xl(
  vapply(studies[[1]][["enrichments"]][[1]][["annotations"]],
         function(x) x[["annotationID"]], character(1)),
  names(getEnrichments(testStudyObj, modelID = testModelName))
)

expect_identical_xl(
  vapply(studies[[1]][["plots"]], function(x) x[["modelID"]], character(1)),
  names(getModels(testStudyObj))
)

expect_identical_xl(
  vapply(studies[[1]][["plots"]][[1]][["plots"]],
         function(x) x[["plotID"]], character(1)),
  names(getPlots(testStudyObj, modelID = testModelName))
)

expect_identical_xl(
  vapply(studies[[1]][["plots"]][[1]][["plots"]],
         function(x) x[["plotType"]], character(1)),
  c("singleFeature", "multiFeature")
)

# If there are no OmicNavigator study packages installed, return an empty list.
expect_identical_xl(
  listStudies(libraries = tempfile()),
  list()
)

# getResultsTable --------------------------------------------------------------

resultsTable <- getResultsTable(testStudyName, testModelName, testTestName)

expect_identical_xl(
  class(resultsTable),
  "data.frame"
)

features <- getFeatures(testStudyObj, testModelName)
expect_true_xl(all(colnames(features) %in% colnames(resultsTable)))

results <- getResults(testStudyObj, testModelName, testTestName)
expect_true_xl(all(colnames(results) %in% colnames(resultsTable)))

expect_identical_xl(
  resultsTable[, 1],
  results[, 1],
  info = "getResultsTable() doesn't change order of featureIDs"
)

expect_error_xl(
  getResultsTable(1),
  "missing"
)

expect_equal_xl(
  resultsTable,
  getResultsTable(testStudyObj, testModelName, testTestName)
)

# getEnrichmentsTable ----------------------------------------------------------

enrichmentsTable <- getEnrichmentsTable(testStudyName, testModelName, testAnnotationName)

expect_identical_xl(
  class(enrichmentsTable),
  "data.frame"
)

expect_true_xl(all(names(getTests(testStudyName, testModelName)) %in% colnames(enrichmentsTable)))

expect_error_xl(
  getEnrichmentsTable(1),
  "missing"
)

expect_equal_xl(
  enrichmentsTable,
  getEnrichmentsTable(testStudyObj, testModelName, testAnnotationName)
)

# getEnrichmentsNetwork --------------------------------------------------------

enrichmentsNetwork <- getEnrichmentsNetwork(
  testStudyName,
  testModelName,
  testAnnotationName
)

expect_identical_xl(
  class(enrichmentsNetwork),
  "list"
)

expect_identical_xl(
  names(enrichmentsNetwork),
  c("tests", "nodes", "links")
)

expect_identical_xl(
  class(enrichmentsNetwork[["tests"]]),
  "character"
)

expect_identical_xl(
  class(enrichmentsNetwork[["nodes"]]),
  "data.frame"
)

expect_identical_xl(
  class(enrichmentsNetwork[["links"]]),
  "data.frame"
)

expect_identical_xl(
  enrichmentsNetwork[["tests"]],
  names(getTests(testStudyName, testModelName))
)

expect_identical_xl(
  dim(enrichmentsNetwork[["nodes"]]),
  as.integer(c(50, 6))
)

# These tests require the use of set.seed() for the randomly-generated numbers
# to match. The default algorithm for random number generation was changed in R
# 3.6.0, so the tests below only pass for R >= 3.6.0
if (getRversion() >= "3.6.0") {

  node1 <- structure(
    list(
      id = 1L,
      termID = "term_01",
      description = "Description of term_01",
      geneSetSize = 18L,
      nominal = list(c(0.03, 0.03)),
      adjusted = list(c(0.05, 0.05))),
    row.names = 1L,
    class = "data.frame"
  )

  expect_identical_xl(
    enrichmentsNetwork[["nodes"]][1, ],
    node1
  )

  expect_identical_xl(
    dim(enrichmentsNetwork[["links"]]),
    as.integer(c(1020, 6))
  )

  link1 <- structure(
    list(
      id = 1L,
      source = 1L,
      target = 3L,
      overlapSize = 6L,
      overlap = 0.333333333333333,
      jaccard = 0.166666666666667),
    row.names = 1L,
    class = "data.frame"
  )

  expect_equal_xl(
    enrichmentsNetwork[["links"]][1, ],
    link1
  )

}

expect_message_xl(
  getEnrichmentsNetwork(testStudyObj, testModelName, testAnnotationName),
  "No overlaps available"
)

expect_error_xl(
  getEnrichmentsNetwork(1),
  "missing"
)

# getMetaFeaturesTable ---------------------------------------------------------

metaFeaturesTable <- getMetaFeaturesTable(
  testStudyName,
  testModelName,
  "feature_0042"
)

expect_identical_xl(
  class(metaFeaturesTable),
  "data.frame"
)

expect_identical_xl(
  dim(metaFeaturesTable),
  c(3L, 5L)
)

# Confirm that even numeric-looking columns are returned as character
expect_identical_xl(
  unique(vapply(metaFeaturesTable, class, FUN.VALUE = character(1), USE.NAMES = FALSE)),
  "character"
)

expect_message_xl(
  getMetaFeaturesTable(
    testStudyName,
    testModelName,
    "non-existent"
  ),
  "No metaFeatures found for featureID \"non-existent\""
)

# getBarcodeData ---------------------------------------------------------------

barcodeData <- getBarcodeData(
  testStudyName,
  testModelName,
  testTestName,
  testAnnotationName,
  testTermName
)

expect_identical_xl(
  names(barcodeData),
  c("data", "highest", "labelStat", "labelLow", "labelHigh")
)

expect_identical_xl(
  colnames(barcodeData[["data"]]),
  c("featureID", "featureEnrichment", "featureDisplay", "statistic", "logFoldChange")
)

expect_identical_xl(
  barcodeData[["data"]][["statistic"]],
  sort(barcodeData[["data"]][["statistic"]], decreasing = TRUE),
  info = "Barcode results should be ordered by statistic column"
)

expect_equal_xl(
  barcodeData[["highest"]],
  ceiling(max(abs(barcodeData[["data"]][, "statistic"])))
)

barcodeData <- getBarcodeData(
  testStudyName,
  "model_03",
  testTestName,
  testAnnotationName,
  testTermName
)

expect_identical_xl(
  barcodeData[["labelStat"]],
  "Effect size",
  info = "Confirm model-specific barcode data returned"
)

# getReportLink ----------------------------------------------------------------

expect_identical_xl(
  getReportLink(testStudyName, testModelName),
  getReports(testStudyObj, modelID = testModelName)
)

expect_identical_xl(
  getReportLink(testStudyName, "model_02"),
  sprintf("%s%s/OmicNavigatorReports/model_02/report.html",
          OmicNavigator:::getPrefix(), testStudyName)
)

# getNodeFeatures --------------------------------------------------------------

annotation <- getAnnotations(testStudyName, testAnnotationName)

expect_identical_xl(
  getNodeFeatures(testStudyName, testAnnotationName, testTermName),
  sort(annotation[["terms"]][[testTermName]])
)

expect_identical_xl(
  getNodeFeatures(testStudyObj, testAnnotationName, testTermName),
  sort(annotation[["terms"]][[testTermName]])
)

expect_message_xl(
  getNodeFeatures(testStudyName, testAnnotationName, "non-existent-term"),
  "non-existent-term"
)

expect_message_xl(
  getNodeFeatures(testStudyName, "non-existent-annotation", testTermName),
  "non-existent-annotation"
)

# getLinkFeatures --------------------------------------------------------------

expect_identical_xl(
  getLinkFeatures(testStudyName, testAnnotationName, testTermName, "term_03"),
  sort(intersect(annotation[["terms"]][[testTermName]],
                 annotation[["terms"]][["term_03"]]))
)

expect_identical_xl(
  getLinkFeatures(testStudyObj, testAnnotationName, testTermName, "term_03"),
  sort(intersect(annotation[["terms"]][[testTermName]],
                 annotation[["terms"]][["term_03"]]))
)

# getPackageVersion ------------------------------------------------------------

expect_identical_xl(
  class(getPackageVersion()),
  "character"
)

# getFavicons ------------------------------------------------------------------

if (at_home()) {
  # Only run these tests locally with tinytest::test_all(). Skip when running
  # tinytest::test_package() (called by R CMD check). faviconPlease is unable to
  # obtain the favicons from the EBI sites when run on Jenkins.

  expect_identical_xl(
    getFavicons(getResultsLinkouts(testStudyName)),
    list(
      default = list(
        customID = c(
          "https://ensembl.org/i/ensembl-favicon.png",
          "https://www.targetvalidation.org/favicon.png"
        ),
        featureVar01 = c(
          "https://www.ncbi.nlm.nih.gov/favicon.ico"
        )
      ),
      model_03 = list(
        featureVar02 = c(
          "https://www.ncbi.nlm.nih.gov/favicon.ico"
        )
      )
    )
  )

  expect_identical_xl(
    getFavicons(getEnrichmentsLinkouts(testStudyName)),
    list(
      annotation_01 = c(
        "http://amigo.geneontology.org/static/images/go-logo-favicon.ico",
        "https://www.ebi.ac.uk/favicon.ico"
      ),
      annotation_03 = c(
        "https://reactome.org//templates/favourite/favicon.ico"
      )
    )
  )

}

expect_identical_xl(
  getFavicons("https://reactome.org/content/detail/"),
  "https://reactome.org//templates/favourite/favicon.ico"
)

expect_identical_xl(
  getFavicons(list()),
  list()
)

# Teardown ---------------------------------------------------------------------

unlink(tmplib, recursive = TRUE, force = TRUE)
.libPaths(libOrig)
