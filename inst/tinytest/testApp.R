# Test app endpoints

# Setup ------------------------------------------------------------------------

source("tinytestSettings.R")
using(ttdo)

library(OmicNavigator)

testStudyName <- "ABC"
testStudyObj <- OmicNavigator:::testStudy(
  name = testStudyName,
  description = "A test study package for testApp.R",
  maintainer = "testApp.R",
  maintainerEmail = "testApp@R",
  version = "0.3"
)
testStudyObj <- addPlots(testStudyObj, OmicNavigator:::testPlots())
testModelName <- names(testStudyObj[["models"]])[1]
testTestName <- names(testStudyObj[["tests"]][[1]])[1]
testAnnotationName <- names(testStudyObj[["annotations"]])[1]
testTermName <- names(testStudyObj[["annotations"]][[testAnnotationName]][["terms"]])[1]

# Add a report file
tmpReport <- tempfile(fileext = ".html")
writeLines("<p>example</p>", tmpReport)
testStudyObj <- addReports(testStudyObj, list(model_02 = tmpReport))

# Create annotation that uses secondary featureID
secondaryFeatureIDName <- "secondaryID"
secondaryID <- getFeatures(testStudyObj)[[1]][[secondaryFeatureIDName]]
secondaryIDterms <- replicate(
  n = 10,
  sample(x = secondaryID, size = sample(5:25, size = 1, replace = TRUE)),
  simplify = FALSE
)
names(secondaryIDterms) <- sprintf("term_%02d", seq_along(secondaryIDterms))
secondaryIDanno <- list(
  annotation_04 = list(
    description = "Annotation that uses a secondary featureID",
    featureID = secondaryFeatureIDName,
    terms = secondaryIDterms
  )
)
testStudyObj <- addAnnotations(testStudyObj, secondaryIDanno)
# just copy subset of enrichments from another annotation
secondaryIDenrich <- getEnrichments(
  study = testStudyObj,
  modelID = "model_01",
  annotationID = "annotation_01",
  testID = "test_01"
)[1:10, ]
secondaryIDenrich <- list(
  model_01 = list(
    annotation_04 = list(
      test_01 = secondaryIDenrich
    )
  )
)
testStudyObj <- addEnrichments(testStudyObj, secondaryIDenrich)

tmplib <- tempfile()
dir.create(tmplib)
libOrig <- .libPaths()
.libPaths(c(tmplib, libOrig))
suppressMessages(installStudy(testStudyObj))

# getStudyMeta -----------------------------------------------------------------

studyMeta <- getStudyMeta(testStudyName, libraries = tmplib)

expect_false_xl(
  OmicNavigator:::studyToPkg(testStudyName) %in% loadedNamespaces()
)

expect_identical_xl(
  studyMeta[["description"]],
  testStudyObj[["description"]]
)
expect_identical_xl(
  studyMeta[["version"]],
  testStudyObj[["version"]]
)

expect_identical_xl(
  studyMeta[["maintainer"]],
  testStudyObj[["maintainer"]]
)

expect_identical_xl(
  studyMeta[["maintainerEmail"]],
  testStudyObj[["maintainerEmail"]]
)

expect_identical_xl(
  studyMeta[["studyMeta"]],
  c(list(OmicNavigatorVersion = getPackageVersion()), testStudyObj[["studyMeta"]])
)

expect_error_xl(
  getStudyMeta("missing"),
  "The package ONstudymissing is not installed"
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

# getResultsTable (filtered by annotationID/termID) ----------------------------

resultsTableTerm <- getResultsTable(testStudyName, testModelName, testTestName,
                                    testAnnotationName, testTermName)
termFeatures <- getNodeFeatures(testStudyObj, testAnnotationName, testTermName)

expect_identical_xl(
  class(resultsTableTerm),
  "data.frame"
)

expect_identical_xl(
  sort(resultsTableTerm[[1]]),
  sort(termFeatures)
)

# Annotation with alternative featureID
resultsTableTerm <- getResultsTable(testStudyName, testModelName, testTestName,
                                    "annotation_04", testTermName)
termFeatures <- getNodeFeatures(testStudyObj, "annotation_04", testTermName)
secondaryFeatureIDcol <- which(colnames(resultsTableTerm) == secondaryFeatureIDName)

expect_identical_xl(
  class(resultsTableTerm),
  "data.frame"
)

expect_identical_xl(
  sort(resultsTableTerm[[secondaryFeatureIDcol]]),
  sort(termFeatures),
  info = "Pull results subset from an annotation term that uses an alternative featureID"
)

expect_equal_xl(
  nrow(resultsTableTerm),
  length(termFeatures)
)

# getResultsTable (minimal study filtered by annotationID/termID) --------------

# Needs to support a minimal study with no annotations terms. Should return an
# empty table.

minimalStudyObj <- OmicNavigator:::testStudyMinimal()

minimalResultsTable <- getResultsTable(
  study = minimalStudyObj,
  modelID = "model_01",
  testID = "test_01",
  annotationID = "annotation_01",
  termID = "term_01"
)

expect_identical_xl(minimalResultsTable, data.frame())

# Minimal study with features but no terms. Still return empty data frame.

minimalStudyObjFeatures <- addFeatures(
  study = minimalStudyObj,
  features = OmicNavigator:::testFeatures()
)

minimalResultsTableFeatures <- getResultsTable(
  study = minimalStudyObjFeatures,
  modelID = "model_01",
  testID = "test_01",
  annotationID = "annotation_01",
  termID = "term_01"
)

expect_identical_xl(minimalResultsTableFeatures, data.frame())

# Minimal study with terms but no features. Should return filtered results.

minimalStudyObjTerms <- addAnnotations(
  study = minimalStudyObj,
  annotations = OmicNavigator:::testAnnotations()
)

minimalResultsTableTerms <- getResultsTable(
  study = minimalStudyObjTerms,
  modelID = "model_01",
  testID = "test_01",
  annotationID = "annotation_01",
  termID = "term_01"
)

expect_identical_xl(
  sort(minimalResultsTableTerms[[1]]),
  sort(getNodeFeatures(
    study = minimalStudyObjTerms,
    annotationID = "annotation_01",
    termID = "term_01"
  ))
)

# getEnrichmentsTable ----------------------------------------------------------

enrichmentsTable <- getEnrichmentsTable(
  study = testStudyName,
  modelID = testModelName,
  annotationID = testAnnotationName
)

expect_identical_xl(
  class(enrichmentsTable),
  "data.frame"
)

expect_true_xl(
  all(names(getTests(testStudyName, testModelName)) %in% colnames(enrichmentsTable))
)

expect_error_xl(
  getEnrichmentsTable(1),
  "missing"
)

expect_equal_xl(
  enrichmentsTable,
  getEnrichmentsTable(testStudyObj, testModelName, testAnnotationName)
)

perTestenrichments <- getEnrichments(
  study = testStudyObj,
  modelID = testModelName,
  annotationID = testAnnotationName,
  testID = testTestName
)

expect_equal_xl(
  enrichmentsTable[[testTestName]],
  perTestenrichments[["nominal"]]
)

enrichmentsTableAdj <- getEnrichmentsTable(
  study = testStudyName,
  modelID = testModelName,
  annotationID = testAnnotationName,
  type = "adjusted"
)

expect_equal_xl(
  enrichmentsTableAdj[[testTestName]],
  perTestenrichments[["adjusted"]]
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
  c("data", "highest", "lowest", "labelStat", "labelLow", "labelHigh")
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

# Confirm that you can use alternative featureID for annotation terms
barcodeData <- getBarcodeData(
  testStudyName,
  testModelName,
  testTestName,
  "annotation_04",
  testTermName
)

expect_identical_xl(
  sort(barcodeData[["data"]][["featureEnrichment"]]),
  sort(getNodeFeatures(testStudyObj, "annotation_04", testTermName)),
  info = "Pull results subset from an annotation term that uses an alternative featureID"
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
  # Only run getFavicons() tests "at home". Accessing internet resources is too
  # prone to spurious errors.

  expect_identical_xl(
    getFavicons(getResultsLinkouts(testStudyName)),
    list(
      default = list(
        customID = c(
          "https://static.ensembl.org/i/ensembl-favicon.png",
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
        "https://amigo.geneontology.org/static/images/go-logo-favicon.ico",
        "https://ebi.emblstatic.net/web_guidelines/EBI-Framework/v1.4/images/logos/EMBL-EBI/favicons/favicon.ico"
        # "https://www.ebi.ac.uk/favicon.ico"
        # The direct favicon.ico link still works, but faviconLink() find this
        # new alternative first
      ),
      annotation_03 = c(
        "https://reactome.org//templates/favourite/favicon.ico"
      )
    )
  )

  expect_identical_xl(
    getFavicons("https://reactome.org/content/detail/"),
    "https://reactome.org//templates/favourite/favicon.ico"
  )

}

expect_identical_xl(
  getFavicons(list()),
  list()
)

# Teardown ---------------------------------------------------------------------

unlink(tmplib, recursive = TRUE, force = TRUE)
.libPaths(libOrig)
