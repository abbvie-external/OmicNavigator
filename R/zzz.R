# The minimum version of OmicAnalyzer that is still compatible with the current
# version. If a study package is created with a version less than this minimum
# version, then listStudies() will throw a warning.
minVersionCompatible <- "0.3.0"

utils::globalVariables(".")

#' @importFrom dbplyr as.sql
NULL

#' Shared parameters for add functions
#'
#' @name shared-add
#'
#' @param study An OmicAnalyzer study created with \code{\link{createStudy}}
#' @param overwrite Overwrite existing value
#'
#' @keywords internal
NULL
