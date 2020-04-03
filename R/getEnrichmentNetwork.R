#' getEnrichmentNetwork
#' @examples
#'   getEnrichmentNetwork("***REMOVED***",
#'                        "No Pretreatment Timecourse Differential Phosphorylation",
#'                        "GO_BIOLOGICAL_PROCESS")
#' @importFrom dplyr "%>%"
#' @importFrom rlang "!!"
#' @export
getEnrichmentNetwork <- function(study, model, database) {
  pkg <- paste0("PAstudy", study)
  db <- system.file("PhosphoApp/PhosphoApp.sqlite", package = pkg,
                    mustWork = TRUE)

  con <- DBI::dbConnect(RSQLite::SQLite(), db)
  on.exit(DBI::dbDisconnect(con))

  result <- list()
  result$study <- study
  result$model <- model
  result$database <- database

  # 1. Nodes: Obtain enrichment results for given study, model, and database
  # Note: a lot of this first step will need to be refactored with
  # getEnrichmentResults().
  terms <- dplyr::tbl(con, "terms") %>%
    dplyr::filter(annotation == !! database) %>%
    dplyr::group_by(key, description) %>%
    dplyr::count(name = "geneSetSize")

  enrichmentResults <- dplyr::tbl(con, "enrichmentResults") %>%
    dplyr::filter(annotation == !! database,
                  modelID == !! model) %>%
    dplyr::select(-annotation, -modelID)

  nodes_long <- enrichmentResults %>%
    dplyr::left_join(terms, by = "key") %>%
    dplyr::arrange(contrast) %>%
    dplyr::collect()

  result$tests <- unique(nodes_long$contrast)

  nodes <- nodes_long %>%
    tidyr::pivot_wider(names_from = contrast,
                       values_from = c(p_val, p_val_adj)) %>%
    dplyr::arrange(key) %>%
    dplyr::mutate(id = seq_len(dplyr::n()))

  # Need a way to combine multiple columns into a numeric vector. tidyr::unite()
  # converts them to a single character element. Also can't use tidyselect
  # operations inside of dplyr::mutate(). Thus just using a for loop.
  nodes$PValue <- NA
  nodes$fdrQValue <- NA
  pval_cols <- grep("p_val_", colnames(nodes))
  pval_adj_cols <- grep("p_val_adj_", colnames(nodes))
  pval_cols <- setdiff(pval_cols, pval_adj_cols)
  for (i in seq_len(nrow(nodes))) {
    nodes$PValue[i] <- list(as.numeric(nodes[i, pval_cols]))
    nodes$fdrQValue[i] <- list(as.numeric(nodes[i, pval_adj_cols]))
  }

  nodes <- nodes %>%
    dplyr::select(id, key, description, geneSetSize, PValue, fdrQValue) %>%
    as.data.frame(nodes)

  result$nodes <- nodes

  # 2. Links: Overlap and jaccard coefficients
  links <- dplyr::tbl(con, "overlaps") %>%
    dplyr::filter(annotation ==  !! database) %>%
    dplyr::select(-annotation) %>%
    dplyr::arrange(term1, term2) %>%
    dplyr::rename(source = term1, target = term2) %>%
    dplyr::collect() %>%
    dplyr::mutate(id = seq_len(dplyr::n())) %>%
    dplyr::select(id, dplyr::everything()) %>%
    as.data.frame()

  # Use node IDs with links
  links$source <- match(links$source, nodes$key)
  links$target <- match(links$target, nodes$key)

  result$links <- links

  # 3. Clusters: Clusters the nodes

  return(result)
}
