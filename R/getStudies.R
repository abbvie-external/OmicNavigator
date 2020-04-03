#' Return list of phosphoproteomics studies
#'
#' @param libraries Character vector of library directories to search for study
#'   packages. If \code{NULL}, uses \code{.libPaths}.
#' @return character vector of PhoshoApp study packages
#' @examples
#'  getStudies()
#' @export
getStudies <- function(libraries = NULL) {
  pkgs <- rownames(utils::installed.packages(lib.loc = libraries))
  pkgs_pa <- grep("^PAstudy", pkgs, value = TRUE)
  studies <- sub("^PAstudy", "", pkgs_pa)

  return(studies)
}
