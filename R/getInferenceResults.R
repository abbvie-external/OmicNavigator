#' @importFrom dplyr "%>%"
#' @importFrom rlang "!!"
#' @export
getInferenceResults <- function(testCategory, test, study) {
  pkg <- paste0("PAstudy", study)
  db <- system.file("PhosphoApp/PhosphoApp.sqlite", package = pkg,
                    mustWork = TRUE)

  con <- DBI::dbConnect(RSQLite::SQLite(), db)
  on.exit(DBI::dbDisconnect(con))

  features_min <- dplyr::tbl(con, "features") %>%
    dplyr::select(featureID, Protein_Site, hgnc_symbol) %>%
    dplyr::rename(Protein = hgnc_symbol)

  query <- dplyr::tbl(con, "InferenceResults") %>%
    dplyr::filter(modelID == !! testCategory,
                  InferenceContrast == !! test) %>%
    dplyr::select(-modelID, -InferenceContrast) %>%
    dplyr::left_join(features_min, by = "featureID") %>%
    dplyr::select_if(is_available) %>%
    dplyr::rename(id_mult = featureID) %>%
    dplyr::collect()

  result <- as.data.frame(query)

  return(result)
}
