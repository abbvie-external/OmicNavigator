# Test app endpoints for dropdown menus

# Setup ------------------------------------------------------------------------

source("tinytestSettings.R")
using(ttdo)

library(OmicNavigator)

tmplib <- tempfile()
dir.create(tmplib)

tmplibSpace <- tempfile("a space")
dir.create(tmplibSpace)

# Minimal study ----------------------------------------------------------------

testStudyMinObj <- OmicNavigator:::testStudyMinimal()
testStudyMinName <- testStudyMinObj$name
suppressMessages(installStudy(testStudyMinObj, library = tmplib))

expect_equal_xl(
  getResultsModels(
    study = testStudyMinName,
    libraries = tmplib
  ),
  list(
    model_01 = "model_01",
    model_02 = "model_02",
    model_03 = "model_03"
  )
)

expect_equal_xl(
  getResultsTests(
    study = testStudyMinName,
    modelID = "model_01",
    libraries = tmplib
  ),
  list(
    test_01 = "test_01",
    test_02 = "test_02"
  )
)

expect_equal_xl(
  getEnrichmentsModels(
    study = testStudyMinName,
    libraries = tmplib
  ),
  list(
    model_01 = "model_01",
    model_02 = "model_02",
    model_03 = "model_03"
  )
)

expect_equal_xl(
  getEnrichmentsAnnotations(
    study = testStudyMinName,
    modelID = "model_01",
    libraries = tmplib
  ),
  list(
    annotation_01 = "annotation_01",
    annotation_02 = "annotation_02",
    annotation_03 = "annotation_03"
  )
)

# Full study -------------------------------------------------------------------

testStudyName <- "testDropdowns"
testStudyObj <- OmicNavigator:::testStudy(name = testStudyName)
suppressMessages(installStudy(testStudyObj, library = tmplib))

expect_equal_xl(
  getResultsModels(
    study = testStudyName,
    libraries = tmplib
  ),
  list(
    model_01 = "Model 1",
    model_02 = "Model 2",
    model_03 = "Model 3"
  )
)

expect_equal_xl(
  getResultsTests(
    study = testStudyName,
    modelID = "model_01",
    libraries = tmplib
  ),
  list(
    test_01 = "test 1",
    test_02 = "test 2"
  )
)

expect_equal_xl(
  getEnrichmentsModels(
    study = testStudyName,
    libraries = tmplib
  ),
  list(
    model_01 = "Model 1",
    model_02 = "Model 2",
    model_03 = "Model 3"
  )
)

expect_equal_xl(
  getEnrichmentsAnnotations(
    study = testStudyName,
    modelID = "model_01",
    libraries = tmplib
  ),
  list(
    annotation_01 = "Terms from annotation_01",
    annotation_02 = "Terms from annotation_02",
    annotation_03 = "Terms from annotation_03"
  )
)

# Empty study ------------------------------------------------------------------

emptyStudyName <- "empty"
emptyStudyObj <- createStudy(name = emptyStudyName)
suppressWarnings(suppressMessages(installStudy(emptyStudyObj, library = tmplib)))

expect_identical_xl(
  getResultsModels(
    study = emptyStudyName,
    libraries = tmplib
  ),
  list()
)

expect_identical_xl(
  getResultsTests(
    study = emptyStudyName,
    modelID = "model_01",
    libraries = tmplib
  ),
  list()
)

expect_identical_xl(
  getEnrichmentsModels(
    study = emptyStudyName,
    libraries = tmplib
  ),
  list()
)

expect_identical_xl(
  getEnrichmentsAnnotations(
    study = emptyStudyName,
    modelID = "model_01",
    libraries = tmplib
  ),
  list()
)

# Edge cases -------------------------------------------------------------------

# Only one model has tooltip
edgeCaseName <- "oneModelTooltip"
edgeCaseObj <- OmicNavigator:::testStudy(edgeCaseName)
edgeCaseObj[["models"]][2:3] <- NULL
suppressMessages(installStudy(edgeCaseObj, library = tmplib))

expect_equal_xl(
  getResultsModels(
    study = edgeCaseName,
    libraries = tmplib
  ),
  list(
    model_01 = "Model 1",
    model_02 = "model_02",
    model_03 = "model_03"
  )
)

expect_equal_xl(
  getEnrichmentsModels(
    study = edgeCaseName,
    libraries = tmplib
  ),
  list(
    model_01 = "Model 1",
    model_02 = "model_02",
    model_03 = "model_03"
  )
)

# Only one test has tooltip
edgeCaseName <- "oneTestTooltip"
edgeCaseObj <- OmicNavigator:::testStudy(edgeCaseName)
edgeCaseObj[["tests"]][["default"]][2:3] <- NULL
suppressMessages(installStudy(edgeCaseObj, library = tmplib))

expect_equal_xl(
  getResultsTests(
    study = edgeCaseName,
    modelID = "model_01",
    libraries = tmplib
  ),
  list(
    test_01 = "test 1",
    test_02 = "test_02"
  )
)

# Only one annotation has tooltip
edgeCaseName <- "oneAnnotationTooltip"
edgeCaseObj <- OmicNavigator:::testStudy(edgeCaseName)
edgeCaseObj[["annotations"]][2:3] <- NULL
suppressMessages(installStudy(edgeCaseObj, library = tmplib))

expect_equal_xl(
  getEnrichmentsAnnotations(
    study = edgeCaseName,
    modelID = "model_01",
    libraries = tmplib
  ),
  list(
    annotation_01 = "Terms from annotation_01",
    annotation_02 = "annotation_02",
    annotation_03 = "annotation_03"
  )
)

# Results and Enrichments have mismatched Models
edgeCaseName <- "mismatchedModels"
edgeCaseObj <- createStudy(
  name = edgeCaseName,
  results = OmicNavigator:::testResults(),
  enrichments = OmicNavigator:::testEnrichments(),
  models = OmicNavigator:::testModels()
)
edgeCaseObj <- addModels(edgeCaseObj, list(model_04 = "Model 4"))
names(edgeCaseObj[["enrichments"]]) <- c("model_01", "model_02", "model_04")
suppressMessages(installStudy(edgeCaseObj, library = tmplib))

expect_equal_xl(
  getResultsModels(
    study = edgeCaseName,
    libraries = tmplib
  ),
  list(
    model_01 = "Model 1",
    model_02 = "Model 2",
    model_03 = "Model 3"
  )
)

expect_equal_xl(
  getEnrichmentsModels(
    study = edgeCaseName,
    libraries = tmplib
  ),
  list(
    model_01 = "Model 1",
    model_02 = "Model 2",
    model_04 = "Model 4"
  )
)

# different tests per model
edgeCaseName <- "diffTestsPerModel"
edgeCaseObj <- createStudy(
  name = edgeCaseName,
  results = OmicNavigator:::testResults()
)
names(edgeCaseObj[["results"]][["model_02"]]) <- c("test_03", "test_04")
names(edgeCaseObj[["results"]][["model_03"]]) <- c("test_05", "test_06")
edgeCaseObj <- addTests(
  study = edgeCaseObj,
  tests = list(
    model_01 = list(test_01 = "test 1", test_02 = "test 2"),
    model_02 = list(test_03 = "test 3", test_04 = "test 4"),
    model_03 = list(test_05 = "test 5", test_06 = "test 6")
  )
)
suppressMessages(installStudy(edgeCaseObj, library = tmplib))

expect_equal_xl(
  getResultsTests(
    study = edgeCaseName,
    modelID = "model_01",
    libraries = tmplib
  ),
  list(
    test_01 = "test 1",
    test_02 = "test 2"
  )
)

expect_equal_xl(
  getResultsTests(
    study = edgeCaseName,
    modelID = "model_02",
    libraries = tmplib
  ),
  list(
    test_03 = "test 3",
    test_04 = "test 4"
  )
)

expect_equal_xl(
  getResultsTests(
    study = edgeCaseName,
    modelID = "model_03",
    libraries = tmplib
  ),
  list(
    test_05 = "test 5",
    test_06 = "test 6"
  )
)

# different annotations per model
edgeCaseName <- "diffAnnotationsPerModel"
edgeCaseObj <- createStudy(
  name = edgeCaseName,
  enrichments = OmicNavigator:::testEnrichments(nModels = 2, nAnnotations = 2),
  annotations = OmicNavigator:::testAnnotations(n = 2)
)
names(edgeCaseObj[["enrichments"]][["model_02"]]) <- c("annotation_03", "annotation_04")
edgeCaseObj[["annotations"]][["annotation_03"]] <- edgeCaseObj[["annotations"]][["annotation_02"]]
edgeCaseObj[["annotations"]][["annotation_04"]] <- edgeCaseObj[["annotations"]][["annotation_02"]]
edgeCaseObj[["annotations"]][["annotation_03"]][["description"]] <- "Terms from annotation_03"
edgeCaseObj[["annotations"]][["annotation_04"]][["description"]] <- "Terms from annotation_04"
suppressMessages(installStudy(edgeCaseObj, requireValid = FALSE, library = tmplib))

expect_equal_xl(
  getEnrichmentsAnnotations(
    study = edgeCaseName,
    modelID = "model_01",
    libraries = tmplib
  ),
  list(
    annotation_01 = "Terms from annotation_01",
    annotation_02 = "Terms from annotation_02"
  )
)

expect_equal_xl(
  getEnrichmentsAnnotations(
    study = edgeCaseName,
    modelID = "model_02",
    libraries = tmplib
  ),
  list(
    annotation_03 = "Terms from annotation_03",
    annotation_04 = "Terms from annotation_04"
  )
)

# Error handling ---------------------------------------------------------------

expect_error_xl(
  getResultsModels(
    study = testStudyObj,
    libraries = tmplib
  ),
  "Only installed study packages are supported"
)

expect_error_xl(
  getResultsTests(
    study = testStudyObj,
    modelID = "model_01",
    libraries = tmplib
  ),
  "Only installed study packages are supported"
)

expect_error_xl(
  getEnrichmentsModels(
    study = testStudyObj,
    libraries = tmplib
  ),
  "Only installed study packages are supported"
)

expect_error_xl(
  getEnrichmentsAnnotations(
    study = testStudyObj,
    modelID = "model_01",
    libraries = tmplib
  ),
  "Only installed study packages are supported"
)

# jq ---------------------------------------------------------------------------

# The results of `getEnrichmentsAnnotations()` should be identical whether or
# not `jq` is used

expect_equal_xl(
  getEnrichmentsAnnotations(
    study = testStudyMinName,
    modelID = "model_01",
    useJqIfAvailable = TRUE,
    libraries = tmplib
  ),
  getEnrichmentsAnnotations(
    study = testStudyMinName,
    modelID = "model_01",
    useJqIfAvailable = FALSE,
    libraries = tmplib
  )
)

expect_equal_xl(
  getEnrichmentsAnnotations(
    study = testStudyName,
    modelID = "model_01",
    useJqIfAvailable = TRUE,
    libraries = tmplib
  ),
  getEnrichmentsAnnotations(
    study = testStudyName,
    modelID = "model_01",
    useJqIfAvailable = FALSE,
    libraries = tmplib
  )
)

expect_identical_xl(
  getEnrichmentsAnnotations(
    study = emptyStudyName,
    modelID = "model_01",
    useJqIfAvailable = TRUE,
    libraries = tmplib
  ),
  getEnrichmentsAnnotations(
    study = emptyStudyName,
    modelID = "model_01",
    useJqIfAvailable = FALSE,
    libraries = tmplib
  )
)

# getAnnotationDisplayJq() can handle a space in the file path

testStudySpaceName <- "testDropdownsSpace"
testStudySpaceObj <- OmicNavigator:::testStudy(name = testStudySpaceName)
suppressMessages(installStudy(testStudySpaceObj, library = tmplibSpace))

expect_equal_xl(
  getEnrichmentsAnnotations(
    study = testStudySpaceName,
    modelID = "model_01",
    useJqIfAvailable = TRUE,
    libraries = tmplibSpace
  ),
  list(
    annotation_01 = "Terms from annotation_01",
    annotation_02 = "Terms from annotation_02",
    annotation_03 = "Terms from annotation_03"
  )
)

# The package option "OmicNavigator.useJqIfAvailable" should be set when the
# package is loaded

expect_true_xl(is.logical(getOption("OmicNavigator.useJqIfAvailable")))

# Teardown ---------------------------------------------------------------------

unlink(tmplib, recursive = TRUE, force = TRUE)
unlink(tmplibSpace, recursive = TRUE, force = TRUE)
