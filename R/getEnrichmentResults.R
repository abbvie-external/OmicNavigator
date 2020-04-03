#' getEnrichmentResults
#' @examples
#'   getEnrichmentResults("No Pretreatment Timecourse Differential Phosphorylation",
#'                        "GO_BIOLOGICAL_PROCESS",
#'                        "***REMOVED***",
#'                        type = "nominal")
#'   getEnrichmentResults("No Pretreatment Timecourse Differential Phosphorylation",
#'                        "GO_BIOLOGICAL_PROCESS",
#'                        "***REMOVED***",
#'                        type = "adjusted")
#' @importFrom dplyr "%>%"
#' @importFrom rlang "!!"
#' @export
getEnrichmentResults <- function(model, database, study, type = "nominal") {
  pkg <- paste0("PAstudy", study)
  db <- system.file("PhosphoApp/PhosphoApp.sqlite", package = pkg,
                    mustWork = TRUE)

  con <- DBI::dbConnect(RSQLite::SQLite(), db)
  on.exit(DBI::dbDisconnect(con))

  if (type == "nominal") {
    enrichment_col <- "p_val"
  } else if (type == "adjusted") {
    enrichment_col <- "p_val_adj"
  } else {
    stop("Invalid input for argument `type`: ", type)
  }

  terms <- dplyr::tbl(con, "terms") %>%
    dplyr::filter(annotation == !! database) %>%
    dplyr::select(key, description) %>%
    dplyr::distinct()

  query <- dplyr::tbl(con, "EnrichmentResults") %>%
    dplyr::filter(modelID == !! model,
                  annotation == !! database) %>%
    dplyr::select(key, contrast, !! enrichment_col) %>%
    dplyr::left_join(terms, by = "key") %>%
    dplyr::collect()

  result <- query %>%
    tidyr::pivot_wider(names_from = "contrast",
                       values_from = !! enrichment_col) %>%
    as.data.frame()

  return(result)
}
