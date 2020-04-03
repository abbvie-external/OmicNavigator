#' sitedata
#' #' @examples
#' sitedata("6433_1", "***REMOVED***")
#' @importFrom dplyr "%>%"
#' @importFrom rlang "!!"
#' @export
sitedata <- function(featureID, study) {
  pkg <- paste0("PAstudy", study)
  db <- system.file("PhosphoApp/PhosphoApp.sqlite", package = pkg,
                    mustWork = TRUE)

  con <- DBI::dbConnect(RSQLite::SQLite(), db)
  on.exit(DBI::dbDisconnect(con))

  features <- dplyr::tbl(con, "features") %>%
    dplyr::select(Protein_Site,
                  Positions.within.proteins,
                  ccds,
                  Evidence.IDs,
                  featureID,
                  multiplicity) %>%
    dplyr::filter(featureID == !! featureID)

  evidence <- dplyr::tbl(con, "evidence") %>%
    dplyr::select(evidenceID, Phospho..STY., Modifications, Modified.sequence,
                  Type, Experiment, Intensity, Proteins,	Leading.proteins,
                  Leading.razor.protein)

  link <- dplyr::tbl(con, "features_link_evidence")

  combined <- features %>%
    dplyr::inner_join(link, by = "featureID") %>%
    dplyr::inner_join(evidence, by = "evidenceID") %>%
    dplyr::collect()

  multiplicity <- unique(combined$multiplicity)
  stopifnot(length(multiplicity) == 1)
  if (multiplicity == 1 || multiplicity == 2) {
    combined <- combined %>% dplyr::filter(Phospho..STY. == multiplicity)
  }
  if(multiplicity == 3) {
    combined <- combined %>% dplyr::filter(Phospho..STY. >= multiplicity)
  }


  aggregated <- combined %>%
    dplyr::group_by(Modified.sequence, Protein_Site, Positions.within.proteins, ccds) %>%
    dplyr::summarise(Phosphates = paste(unique(Phospho..STY.), collapse = ";"),
                     Proteins = paste(unique(Proteins), collapse = ";"),
                     LeadingProteins = paste(unique(Leading.proteins), collapse = ";"),
                     LeadingRazorProtein = paste(unique(Leading.razor.protein), collapse = ";"),
                     Modifications = paste(unique(Modifications), collapse = ";"),
                     Experiments = paste(Experiment, collapse = ";"),
                     Observations = length(evidenceID),
                     ObservationTypes = paste(Type, collapse = ";"),
                     MedianIntensity = median(Intensity, na.rm = T))

  result <- aggregated %>%
    dplyr::select(Protein_Site,
                  CCDS = ccds,
                  PositionsWithinProteins = Positions.within.proteins,
                  Modified_sequence = Modified.sequence,
                  Phosphates,
                  Proteins,
                  LeadingProteins,
                  LeadingRazorProtein,
                  Modifications,
                  Experiments,
                  Observations,
                  ObservationTypes,
                  MedianIntensity) %>%
    as.data.frame(stringsAsFactors = FALSE)

  return(result)
}
