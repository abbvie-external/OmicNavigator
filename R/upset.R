#' Shared parameters for upset functions
#'
#' @name shared-upset
#'
#' @param anchor The primary test to filter from.
#' @param mustTests The tests whose significant values must be included. (The intersection)
#' @param notTests The tests whose significant values will be removed. (The difference)
#' @param sigValue The significance levels for each column.
#' @param operator The operators for each column.
#' @param column The columns to be thresheld.
#' @param type Type of p-value (\code{"nominal"} or \code{"adjusted"})
#' @param tests Restrict UpSet plot to these tests
#'
#' @keywords internal
NULL

#' getResultsIntersection
#'
#' @inheritParams shared-upset
#' @inheritParams shared-get
#'
#' @export
getResultsIntersection <- function(
  study,
  modelID,
  anchor,
  mustTests,
  notTests,
  sigValue,
  operator,
  column
)
{
  results <- getResults(study, modelID = modelID)
  results <- list(results)
  names(results) <- modelID

  intersection <- getInferenceIntersection(
    Inference.Results = results,
    testCategory = modelID,
    anchor = anchor,
    mustTests = mustTests,
    notTests = notTests,
    sigValue = sigValue,
    operator = operator,
    column = column
  )

  features <- getFeatures(study, modelID = modelID)
  if (isEmpty(features)) {
    intersectionTable <- intersection
  } else {
    intersectionTable <- merge(features, intersection, by = 1)
  }
  columnsOrder <- union(c(colnames(features), "Set_Membership"),
                        colnames(intersection))
  intersectionTable <- intersectionTable[, columnsOrder]

  return(intersectionTable)
}

#' Find the Intersection of a list of tests.
#'
#' This function returns a table of results.
#'
#' @param anchor The primary test to filter from.
#' @param testCategory The test category
#' @param notTests The tests whose significant values will be removed. (The difference)
#' @param mustTests The tests whose significant values must be included. (The intersection)
#' @param sigValue The significance levels for each column.
#' @param operator The operators for each column.
#' @param column The columns to be thresheld.
#'
#' Note: The sigValue, operator, and column parameter vectors must be the same length.
#'
#' @return a table
#' @examples
#'  getInferenceIntersection(testCategory = "No Pretreatment Timecourse Differential Phosphorylation", sigValue = c(.05,.01), notTests=c(), anchor = "IKE", mustTests = c("FIN56"), operator = c("<",">"), column = c("adj_P_Val","adj_P_Val"))
#'
#' Notes:
#'   * Source: ***REMOVED***/blob/7560460792780289eb45eb18567d8904a0f0d40d/R/getInferenceIntersection.R
#' Changes made:
#'   * Added Inference.Results as first argument
#'   * Changed `id` to be set to the name of the first column
#'   * Changed `<=` and `>=` to `<` and `>`, respectively, to match app UI
#'   * Reordered `anchor` test to start of tests using `union()`
#' @noRd
getInferenceIntersection <- function(Inference.Results, testCategory, anchor, mustTests, notTests, sigValue, operator=c("<"), column=c("adj_P_Val")) {

  #Initializing the master data frame to the anchor
  rv <- Inference.Results[[testCategory]][[anchor]]

  if (length(sigValue) != length(operator) || length(column) != length(operator)) {
    stop("The arguments sigValue, column, and operator must be the same length")
  }

  #Instantiate the set membership column. This will be filled out as you go. If the protein site is significant
  #in the study, then it will concatenate the study to the set membership column.
  rv$Set_Membership <- anchor
  tests <- names(Inference.Results[[testCategory]])
  tests <- union(anchor, tests)

  #Calculate Intersection
  for(i in 1:length(tests)){
    temp = Inference.Results[[testCategory]][[tests[i]]]
    # if("id" %in%  colnames(temp)){id = "id"}
    # else{id = "id_mult"}
    id <- colnames(temp)[1]
    fTemp = data.frame(temp[,id])
    fTemp = cbind(fTemp, 1)
    fTemp[,2] = as.numeric(fTemp[,2])
    for(k in 1:length(operator)){
      sigCol = c(temp[,column[k]])
      if(operator[k] == "<"){sigCol <- as.numeric(sigCol < sigValue[k])}
      else if(operator[k] == ">"){sigCol <- as.numeric(sigCol > sigValue[k])}
      else if(operator[k] == "|>|"){sigCol <- as.numeric(abs(sigCol) > sigValue[k])}
      else if(operator[k] == "|<|"){sigCol <- as.numeric(abs(sigCol) < sigValue[k])}
      fTemp[,2] = as.integer(fTemp[,2] & sigCol)
    }
    fTemp = fTemp[fTemp[,2] == 1,]
    temp = temp[temp[,id] %in% fTemp[,1],]
    if(tests[i] %in% mustTests || tests[i] == anchor){
      rv = rv[rv[,id] %in% temp[,id],]
      if(nrow(rv) == 0){return(rv)}
      if(tests[i] != anchor){rv$Set_Membership <- paste(rv$Set_Membership, tests[i], sep = " , ")}
    }
    else if(tests[i] %in% notTests){
      rv = rv[!rv[,id] %in% temp[,id],]
      if(nrow(rv) == 0){return(rv)}
    }
    else{rv[which(is.element(rv$id,temp[,id])),"Set_Membership"] <- paste(rv[which(is.element(rv$id,temp[,id])),"Set_Membership"],tests[i], sep=" , ")}
  }
  #Returning the master data frame.
  return (rv)
}

#' getEnrichmentsIntersection
#'
#' @inheritParams shared-upset
#' @inheritParams shared-get
#'
#' @export
getEnrichmentsIntersection <- function(
  study,
  modelID,
  annotationID,
  mustTests,
  notTests,
  sigValue,
  operator,
  type
)
{
  if (type == "nominal") {
    Enrichment.Results <- formatEnrichmentResults(study, modelID, annotationID, type)
    Enrichment.Results.Adjusted <- NULL
  } else if (type == "adjusted") {
    Enrichment.Results <- NULL
    Enrichment.Results.Adjusted <- formatEnrichmentResults(study, modelID, annotationID, type)
  } else {
    stop(sprintf("Argument `type` must be \"nominal\" or \"adjusted\", not \"%s\"",
                 type))
  }

  intersection <- getEnrichmentIntersection(
    Enrichment.Results = Enrichment.Results,
    Enrichment.Results.Adjusted = Enrichment.Results.Adjusted,
    testCategory = modelID,
    mustTests = mustTests,
    notTests = notTests,
    sigValue = sigValue,
    annotation = annotationID,
    operator = operator,
    pValType = type
  )

  return(intersection)
}

#' Find the Intersection of a list of tests.
#'
#' @param testCategory The test category
#' @param notTests The tests whose significant values will be removed. (The difference)
#' @param mustTests The tests whose significant values must be included. (The intersection)
#' @param sigValue The significance levels
#' @param annotation The annotation
#' @param operator The operators
#' @param pValType nominal or adjusted
#' Note: The sigValue and operator parameter vectors must be the same length.
#'
#' @return a table
#' @examples
#'  getEnrichmentIntersection("No Pretreatment Timecourse Differential Phosphorylation", sigValue = c(.05),notTests=c(),annotation="GOSLIM",mustTests = c("IKE","FIN56"))
#'
#' Notes:
#'   * Source: ***REMOVED***/blob/7560460792780289eb45eb18567d8904a0f0d40d/R/getEnrichmentIntersection.R
#' Changes made:
#'   * Added arguments Enrichment.Results and Enrichment.Results.Adjusted
#'   * Changed `<=` and `>=` to `<` and `>`, respectively, to match app UI
#'
#' @noRd
getEnrichmentIntersection <- function(Enrichment.Results, Enrichment.Results.Adjusted, testCategory, mustTests, notTests, sigValue, annotation, operator=c("<"), pValType="nominal") {

  if (length(sigValue) != length(operator)) {
    stop("The arguments sigValue and operator must be the same length")
  }

  #Generate Master Data Frame
  if(pValType == "nominal"){
    rv <- Enrichment.Results[[testCategory]][[annotation]]
    data <- Enrichment.Results[[testCategory]][[annotation]]
    tests <- names(Enrichment.Results[[testCategory]][[annotation]])
  }else{
    rv <- Enrichment.Results.Adjusted[[testCategory]][[annotation]]
    data <- Enrichment.Results.Adjusted[[testCategory]][[annotation]]
    tests <- names(Enrichment.Results.Adjusted[[testCategory]][[annotation]])
  }

  #Calculate Intersection
  for(i in 2:length(tests)){
    filteredColumn = data[,c(1,i)]
    for(k in 1:length(operator)){
      if(tests[i] %in% mustTests || tests[i] %in% notTests){
        if(operator[k] == "<"){filteredColumn <- filteredColumn[filteredColumn[[tests[i]]] < sigValue[k],]}
        else if(operator[k] == ">"){filteredColumn <- filteredColumn[filteredColumn[[tests[i]]] > sigValue[k],]}
      }
    }
    if(tests[i] %in% mustTests){rv = rv[rv[,1] %in% filteredColumn[,1],]}
    else if(tests[i] %in% notTests){rv = rv[!rv[,1] %in% filteredColumn[,1],]}
    if(length(row.names(rv)) == 0){
      return(rv)
    }
  }

  #Returning the master data frame.
  return (rv)
}

#' getResultsUpset
#'
#' @inheritParams shared-upset
#' @inheritParams shared-get
#'
#' @export
getResultsUpset <- function(
  study,
  modelID,
  sigValue,
  operator,
  column
)
{
  if (!requireNamespace("UpSetR", quietly = TRUE)) {
    stop("Install the package \"UpSetR\" to run getResultsUpset()")
  }

  results <- getResults(study, modelID = modelID)
  results <- list(results)
  names(results) <- modelID

  resultsUpset <- InferenceUpsetPlot(
    Inference.Results = results,
    testCategory = modelID,
    sigValue = sigValue,
    operator = operator,
    column = column
  )

  return(resultsUpset)
}

#' Creates a static Upset plot
#'
#' @param testCategory The test category
#' @param sigValue The significance levels for each column.
#' @param operator The operators for each column.
#' @param column The columns to be thresheld.
#' Note: The sigValue, operator, and column parameter vectors must be the same length.
#'
#' @return An SVG
#' @examples
#'  InferenceUpsetPlot(testCategory = "No Pretreatment Timecourse Differential Phosphorylation", sigValue = c(.05), operator = c("<"), column = c("adj_P_Val"))
#'
#' Notes:
#'   * Source: ***REMOVED***/blob/7560460792780289eb45eb18567d8904a0f0d40d/R/InferenceUpsetPlot.R
#' Changes made:
#'   * Added Inference.Results as first argument
#'   * Changed `id` to be set to the name of the first column
#'   * Changed `<=` and `>=` to `<` and `>`, respectively, to match app UI
#'   * Set `newpage = FALSE` when printing to avoid blank page
#'
#' @noRd
InferenceUpsetPlot <- function(Inference.Results, testCategory, sigValue, operator=c("<"), column= c("adj_P_Val")) {

  if (length(sigValue) != length(operator) || length(column) != length(operator)) {
    stop("The arguments sigValue, column, and operator must be the same length")
  }

  #Create list of variables from parameter strings
  tests <- names(Inference.Results[[testCategory]])
  testsUsed <- tests
  # if("id_mult" %in% colnames(Inference.Results[[testCategory]][[tests[1]]])){id <- "id_mult"}
  # else{id <- "id"}
  id <- colnames(Inference.Results[[testCategory]][[tests[1]]])[1]

  #Create the master data frame
  data <- data.frame(matrix(ncol=length(tests)+1, nrow=length(Inference.Results[[testCategory]][[tests[1]]][[id]])))
  colnames(data) <- c("Identifier",tests)
  data[,1] = as.character(Inference.Results[[testCategory]][[tests[1]]][[id]])
  data = data[order(data[,1]),]

  for(i in 1:length(tests)){
    mat <- Inference.Results[[testCategory]][[tests[i]]][[id]]
    for(k in 1:length(operator)){
      sigCol <- as.numeric(Inference.Results[[testCategory]][[tests[i]]][[column[k]]])
      if(operator[k] == "<"){sigCol <- as.numeric(sigCol < sigValue[k])}
      else if(operator[k] == ">"){sigCol <- as.numeric(sigCol > sigValue[k])}
      else if(operator[k] == "|<|"){sigCol <- as.numeric(abs(sigCol) < sigValue[k])}
      else if(operator[k] == "|>|"){sigCol <- as.numeric(abs(sigCol) > sigValue[k])}
      if(sum(sigCol)==0){testsUsed <- testsUsed[testsUsed != tests[i]]}
      mat <- cbind.data.frame(mat, sigCol)
      if(k > 1){
        mat[,2] = as.integer(mat[,2] & mat[,3])
        mat = mat[,-3]
      }
    }
    #This function assumes that all the tests have the same id_mults; therefore, we can order them so they are in the same order and match up when lined up.
    #Each file must contain the same exact id_mults for this to work.
    mat <- mat[order(mat[,1]),]
    data[,i+1] <- as.numeric(mat[,2])
  }
  if(length(testsUsed) <= 1){return (NULL)}

  #Create the upset plot.
  rv <- UpSetR::upset(data,sets = testsUsed, sets.bar.color = "#56B4E9",order.by = "freq", empty.intersections = "on")
  #rv <- upset(data,point.size=1.1, line.size=0.4,sets = testsUsed, sets.bar.color = "#56B4E9",order.by = "freq", empty.intersections = "on")
  print(rv, newpage = FALSE)
  invisible();
}

#' getEnrichmentsUpset
#'
#' @inheritParams shared-upset
#' @inheritParams shared-get
#'
#' @export
getEnrichmentsUpset <- function(
  study,
  modelID,
  annotationID,
  sigValue,
  operator,
  type,
  tests = NULL
)
{
  if (!requireNamespace("UpSetR", quietly = TRUE)) {
    stop("Install the package \"UpSetR\" to run getEnrichmentsUpset()")
  }

  if (type == "nominal") {
    Enrichment.Results <- formatEnrichmentResults(study, modelID, annotationID, type)
    Enrichment.Results.Adjusted <- NULL
  } else if (type == "adjusted") {
    Enrichment.Results <- NULL
    Enrichment.Results.Adjusted <- formatEnrichmentResults(study, modelID, annotationID, type)
  } else {
    stop(sprintf("Argument `type` must be \"nominal\" or \"adjusted\", not \"%s\"",
                 type))
  }

  enrichmentsUpset <- EnrichmentUpsetPlot(
    Enrichment.Results = Enrichment.Results,
    Enrichment.Results.Adjusted = Enrichment.Results.Adjusted,
    testCategory = modelID,
    annotation = annotationID,
    sigValue = sigValue,
    operator = operator,
    pValType = type,
    tests = tests
  )

  return(enrichmentsUpset)
}

#' Creates a static Upset plot
#'
#' @param testCategory The test category
#' @param sigValue The significance values
#' @param annotation The annotation
#' @param operator The operators
#' @param pValType nominal or adjusted
#' @param tests The tests
#' Note: The sigValue and operator parameter vectors must be the same length.
#'
#' @return An SVG
#' @examples
#'  EnrichmentUpsetPlot("No Pretreatment Timecourse Differential Phosphorylation", annotation="GOSLIM",c(.05))
#'
#' Notes:
#'   * Source: ***REMOVED***/blob/7560460792780289eb45eb18567d8904a0f0d40d/R/EnrichmentUpsetPlot.R
#' Changes made:
#'   * Added arguments Enrichment.Results and Enrichment.Results.Adjusted
#'   * Passed `na.rm = TRUE` to sum() to handle missing values
#'   * Changed `<=` and `>=` to `<` and `>`, respectively, to match app UI
#'   * Added tests parameter and respective subset functionality
#'   * Removed colsUsed variable and replaced with tests
#'   * Set `newpage = FALSE` when printing to avoid blank page
#'
#' @noRd
EnrichmentUpsetPlot <- function(Enrichment.Results, Enrichment.Results.Adjusted, testCategory, annotation, sigValue, operator=c("<"), pValType="nominal", tests=NULL) {

  if (length(sigValue) != length(operator)) {
    stop("The arguments sigValue and operator must be the same length")
  }
  if(!is.null(tests) && length(tests) < 2){
    stop("UpSet plot requires two or more tests to subset")
  }

  if(pValType=="nominal"){
    Identifier <- Enrichment.Results[[testCategory]][[annotation]][,1]
    data <- Enrichment.Results[[testCategory]][[annotation]][,names(Enrichment.Results[[testCategory]][[annotation]])]
  }else{
    Identifier <- Enrichment.Results.Adjusted[[testCategory]][[annotation]][,1]
    data <- Enrichment.Results.Adjusted[[testCategory]][[annotation]][,names(Enrichment.Results.Adjusted[[testCategory]][[annotation]])]
  }
  if(is.null(tests)){
    tests = colnames(data)
  }

  for(i in 1:ncol(data)){
    if(colnames(data)[i]%in%tests){
      temp = data.frame(data[,1])
      temp$newCol = 1
      for(j in 1:length(operator)){
        sigCol = data[,i]
        if(operator[j]=="<"){sigCol <- as.numeric(sigCol < sigValue[j])}
        else if(operator[j]==">"){sigCol <- as.numeric(sigCol > sigValue[j])}
        temp = cbind(temp, sigCol)
        temp[,2] = as.integer(temp[,2] & temp[,3])
        temp = temp[,-3]
      }
      if(sum(temp[,2], na.rm = TRUE) == 0){tests <- tests[tests != colnames(data)[i]]}
      data[,i] = temp[,2]
    }
  }
  if(length(tests) < 2){
    stop("Not enough significant elements to create upset plot")
  }

  data <- cbind(Identifier, data)

  #Create the upset plot.
  rv <- UpSetR::upset(data, sets = tests, sets.bar.color = "#56B4E9",order.by = "freq", empty.intersections = "on")

  print(rv, newpage = FALSE)
  invisible();
}

# Format enrichment table as if it was in PhosphoProt Enrichment.Results objects
formatEnrichmentResults <- function(study, modelID, annotationID, type) {
  enrichmentsTable <- getEnrichmentsTable(study, modelID, annotationID, type)
  Enrichment.Results <- vector("list", 1)
  names(Enrichment.Results) <- modelID
  Enrichment.Results[[1]] <- vector("list", 1)
  names(Enrichment.Results[[1]]) <- annotationID
  Enrichment.Results[[1]][[1]] <- enrichmentsTable
  return(Enrichment.Results)
}

#' getUpsetCols
#'
#' Determine the common columns across all tests of a model that are available
#' for filtering with UpSet.
#'
#' @inheritParams shared-get
#'
#' @return character vector
#'
#' @export
getUpsetCols <- function(
  study,
  modelID
)
{
  results <- getResults(study, modelID = modelID)
  if (isEmpty(results)) return(character())
  colsAll <- lapply(results, function(x) colnames(x[, -1]))
  colsCommon <- Reduce(intersect, colsAll)
  return(colsCommon)
}
