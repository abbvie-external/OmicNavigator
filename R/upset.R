#' getResultsIntersection
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
  column,
  ...
)
{
  results <- getResults(study)

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

  return(intersection)
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
#'   * "<" is actually "<=" and ">" is actually ">="
#' Changes made:
#'   * Added Inference.Results as first argument
#'   * Changed `id` to be set to the name of the first column
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
  tests <- tests[tests != anchor]
  tests <- c(anchor, tests)

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
      if(operator[k] == "<"){sigCol <- as.numeric(sigCol <= sigValue[k])}
      else if(operator[k] == ">"){sigCol <- as.numeric(sigCol >= sigValue[k])}
      else if(operator[k] == "|>|"){sigCol <- as.numeric(abs(sigCol) >= sigValue[k])}
      else if(operator[k] == "|<|"){sigCol <- as.numeric(abs(sigCol) <= sigValue[k])}
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
