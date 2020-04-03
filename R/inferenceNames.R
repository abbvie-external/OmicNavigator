#' @importFrom dplyr "%>%"
#' @export
inferenceNames <- function(study) {
  pkg <- paste0("PAstudy", study)
  db <- system.file("PhosphoApp/PhosphoApp.sqlite", package = pkg,
                    mustWork = TRUE)

  con <- DBI::dbConnect(RSQLite::SQLite(), db)
  on.exit(DBI::dbDisconnect(con))

  query <- dplyr::tbl(con, "InferenceResults") %>%
    dplyr::distinct(modelID, InferenceContrast) %>%
    dplyr::collect()

  result <- list()
  for (modelID in unique(query$modelID)) {
    result[[modelID]] <- query$InferenceContrast[query$modelID == modelID]
  }

  return(result)
}
