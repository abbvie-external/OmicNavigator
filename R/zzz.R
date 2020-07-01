# The minimum version of OmicAnalyzer that is still compatible with the current
# version. If a study package is created with a version less than this minimum
# version, then listStudies() will throw a warning.
minVersionCompatible <- "0.5.0"

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

#' Shared parameters for get functions
#'
#' @name shared-get
#'
#' @param study An OmicAnalyzer study. Either an object of class \code{oaStudy}
#'   or the name of an installed study package.
#' @param modelID Filter by modelID
#' @param testID Filter by testID
#' @param annotationID Filter by annotation
#' @param termID Filter by termID
#' @param featureID Filter by featureID
#' @param plotID Filter by plotID
#'
#' @keywords internal
NULL
