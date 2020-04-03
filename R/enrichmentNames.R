#' @importFrom dplyr "%>%"
#' @export
enrichmentNames <- function(study) {
  pkg <- paste0("PAstudy", study)
  db <- system.file("PhosphoApp/PhosphoApp.sqlite", package = pkg,
                    mustWork = TRUE)

  con <- DBI::dbConnect(RSQLite::SQLite(), db)
  on.exit(DBI::dbDisconnect(con))

  query <- dplyr::tbl(con, "EnrichmentResults") %>%
    dplyr::distinct(modelID, annotation) %>%
    dplyr::collect()

  result <- list()
  for (modelID in unique(query$modelID)) {
    result[[modelID]] <- query$annotation[query$modelID == modelID]
  }

  return(result)
}
