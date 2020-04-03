library(magrittr)
library(OmicAnalyzer)

# enrichmentNames --------------------------------------------------------------

# ***REMOVED***/blob/master/R/EnrichmentNames.R
# https://***REMOVED***/IProt/PhosphoProt/blob/master/R/EnrichmentNames.R
# Note the spelling difference

enrichment_names_1 <- PhosphoProt::EnrichmentNames("***REMOVED***plots")
enrichment_names_2 <- enrichmentNames("***REMOVED***")

stopifnot(identical(
  enrichment_names_1[sort(names(enrichment_names_1))],
  enrichment_names_2[sort(names(enrichment_names_2))]
))

# getEnrichmentNetwork ---------------------------------------------------------

study <- "***REMOVED***"
model <- "No Pretreatment Timecourse Differential Phosphorylation"
database <- "GO_BIOLOGICAL_PROCESS"

enrichment_network <- getEnrichmentNetwork(study, model, database)
enrichment_network_min <- enrichment_network
enrichment_network_min$nodes <- enrichment_network_min$nodes[1:5, ]
enrichment_network_min$links <- enrichment_network_min$links[1:5, ]
enrichment_network_json_file <- file.path(tempdir(), "network.json")
# enrichment_network_json_file <- "network.json"
jsonlite::write_json(enrichment_network_min, enrichment_network_json_file,
                     pretty = TRUE)

# getEnrichmentResults ---------------------------------------------------------

# ***REMOVED***/blob/master/R/getEnrichmentResults.R
# https://***REMOVED***/IProt/PhosphoProt/blob/master/R/getEnrichmentResults.R

model <- "No Pretreatment Timecourse Differential Phosphorylation"
database <- "GO_BIOLOGICAL_PROCESS"

enrichment_results_1 <- PhosphoProt::getEnrichmentResults(
  model = model, database = database, "***REMOVED***plots", type = "nominal") %>%
  dplyr::arrange(Description)

enrichment_results_2 <- getEnrichmentResults(
  model = model, database = database, "***REMOVED***", type = "nominal") %>%
  dplyr::select(-key) %>%
  dplyr::rename(Description = description) %>%
  dplyr::arrange(Description)

stopifnot(identical(enrichment_results_1, enrichment_results_2))

enrichment_results_adj_1 <- PhosphoProt::getEnrichmentResults(
  model = model, database = database, "***REMOVED***plots", type = "adjusted") %>%
  dplyr::arrange(Description)

enrichment_results_adj_2 <- getEnrichmentResults(
  model = model, database = database, "***REMOVED***", type = "adjusted") %>%
  dplyr::select(-key) %>%
  dplyr::rename(Description = description) %>%
  dplyr::arrange(Description)

stopifnot(identical(enrichment_results_adj_1, enrichment_results_adj_2))

# getStudies -------------------------------------------------------------------

stopifnot(identical(getStudies(), "***REMOVED***"))

# getDatabases -----------------------------------------------------------------

# https://***REMOVED***/IProt/PhosphoProt/issues/33

databases_1 <- jsonlite::fromJSON(PhosphoProt::getDatabases("***REMOVED***plots", "GOSLIM")) %>%
  dplyr::select(key = Key, description = Description, id = hgnc_symbol) %>%
  dplyr::distinct() %>%
  dplyr::arrange(key, description, id)
databases_2 <- getDatabases("***REMOVED***", "GOSLIM") %>%
  dplyr::arrange(key, description, id)

stopifnot(
  identical(dim(databases_1), dim(databases_2)),
  identical(setdiff(colnames(databases_1), colnames(databases_2)), character(0)),
  identical(databases_1$key, databases_2$key),
  identical(databases_1, databases_2)
)

# getInferenceResults ----------------------------------------------------------

# ***REMOVED***/blob/master/R/getInferenceResults.R
# https://***REMOVED***/IProt/PhosphoProt/blob/master/R/getInferenceResults.R

inference_results_1 <- PhosphoProt::getInferenceResults("No Pretreatment Timecourse Differential Phosphorylation",
                                                        "RSL3 VS FIN56",
                                                        "***REMOVED***plots") %>%
  dplyr::arrange(id_mult)
inference_results_2 <- getInferenceResults("No Pretreatment Timecourse Differential Phosphorylation",
                                           "RSL3 VS FIN56",
                                           "***REMOVED***") %>%
  dplyr::arrange(id_mult)

stopifnot(
  identical(inference_results_1$id_mult, inference_results_2$id_mult),
  all.equal(inference_results_1$F, inference_results_2$F),
  all.equal(inference_results_1$P_Value, inference_results_2$P_Value),
  all.equal(inference_results_1$adj_P_Val, inference_results_2$adj_P_Val)
)

# For some reason the Protein and Protein_Site are different in Inference.Results,
# which provides the results for PhoshoProt, and SiteTableCombined, which is what
# I used to construct the features database for phoshonaut.
# stopifnot(
#   identical(inference_results_1$Protein_Site, inference_results_2$Protein_Site),
#   identical(inference_results_1$Protein, inference_results_2$Protein)
# )

# inferenceNames ---------------------------------------------------------------

# ***REMOVED***/blob/master/R/InferenceNames.R
# https://***REMOVED***/IProt/PhosphoProt/blob/master/R/inferenceNames.R

inference_names_1 <- PhosphoProt::inferenceNames("***REMOVED***plots")
inference_names_2 <- inferenceNames("***REMOVED***")

stopifnot(identical(
  inference_names_1[sort(names(inference_names_1))],
  inference_names_2[sort(names(inference_names_2))]
))

# sitedata ---------------------------------------------------------------------

# ***REMOVED***/blob/master/R/sitedata.R
# https://***REMOVED***/IProt/PhosphoProt/blob/master/R/sitedata.R

sitedata_1 <- PhosphoProt::sitedata("6433_1", "***REMOVED***plots") %>%
  dplyr::mutate_if(is.factor, as.character)
sitedata_2 <- sitedata("6433_1", "***REMOVED***")

stopifnot(
  identical(dim(sitedata_1), dim(sitedata_2)),
  identical(colnames(sitedata_1), colnames(sitedata_2)),
  identical(sitedata_1, sitedata_2)
)
