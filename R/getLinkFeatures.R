#' getLinkFeatures
#' @examples
#'   getLinkFeatures("***REMOVED***", "GO_BIOLOGICAL_PROCESS", "GO:0000027", "GO:0000184")
#'   getLinkFeatures("***REMOVED***", "GO_BIOLOGICAL_PROCESS", "GO:0000027", "GO:0006364")
#' @importFrom dplyr "%>%"
#' @importFrom rlang "!!"
#' @export
getLinkFeatures <- function(study, database, node1, node2) {

  node1_features <- getNodeFeatures(study, database, node1)
  node2_features <- getNodeFeatures(study, database, node2)

  overlap_features <- intersect(node1_features, node2_features)

  return(overlap_features)
}
