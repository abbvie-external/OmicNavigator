#' getNodeFeatures
#' @examples
#'   getNodeFeatures("***REMOVED***", "GO_BIOLOGICAL_PROCESS", "GO:0000002")
#'   getNodeFeatures("***REMOVED***", "GOSLIM", "GO:0140014")
#' @importFrom dplyr "%>%"
#' @importFrom rlang "!!"
#' @export
getNodeFeatures <- function(study, database, node) {
  pkg <- paste0("PAstudy", study)
  db <- system.file("PhosphoApp/PhosphoApp.sqlite", package = pkg,
                    mustWork = TRUE)

  con <- DBI::dbConnect(RSQLite::SQLite(), db)
  on.exit(DBI::dbDisconnect(con))

  terms <- dplyr::tbl(con, "terms") %>%
    dplyr::filter(annotation == !! database,
                  key == !! node) %>%
    dplyr::collect()

  if (nrow(terms) == 0) stop("Invalid node ID: ", node)

  node_features <- terms$id

  return(node_features)
}
