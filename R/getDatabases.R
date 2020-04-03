#' getDatabases
#' @examples
#'   getDatabases("***REMOVED***", "GOSLIM")[1:3, ]
#'   getDatabases("***REMOVED***", "msig_C6")[1:3, ]
#'   getDatabases("***REMOVED***", "PSP")[1:3, ]
#' @importFrom dplyr "%>%"
#' @importFrom rlang "!!"
#' @export
getDatabases <- function(study, database) {
  pkg <- paste0("PAstudy", study)
  db <- system.file("PhosphoApp/PhosphoApp.sqlite", package = pkg,
                    mustWork = TRUE)

  con <- DBI::dbConnect(RSQLite::SQLite(), db)
  on.exit(DBI::dbDisconnect(con))

  query <- dplyr::tbl(con, "terms") %>%
    dplyr::filter(annotation == !! database) %>%
    dplyr::select(-annotation) %>%
    dplyr::collect()

  result <- as.data.frame(query)
  return(result)
}
