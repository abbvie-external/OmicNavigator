# The minimum version of OmicAnalyzer that is still compatible with the current
# version. If a study package is created with a version less than this minimum
# version, then listStudies() will throw a warning.
minVersionCompatible <- "0.5.0"

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
#' @param study An OmicAnalyzer study. Either an object of class \code{oaStudy},
#'   or the name of an installed study package.
#' @param modelID Filter by modelID
#' @param testID Filter by testID
#' @param annotationID Filter by annotation
#' @param termID Filter by termID
#' @param featureID Filter by featureID
#' @param plotID Filter by plotID
#' @param ... Currently unused but required for potential future arguments. See
#'   the section
#'   \href{https://cran.r-project.org/doc/manuals/R-exts.html#Generic-functions-and-methods}{Generic
#'   functions and methods} in
#'   \href{https://cran.r-project.org/doc/manuals/R-exts.html}{Writing R
#'   Extensions}.
#'
#' @keywords internal
NULL

#' Shared parameters for upset functions
#'
#' @name shared-upset
#'
#' @param anchor The primary test to filter from.
#' @param mustTests The tests whose significant values must be included. (The intersection)
#' @param notTests The tests whose significant values will be removed. (The difference)
#' @param sigValue The significance levels for each column.
#' @param operator The operators for each column.
#' @param column The columns to be thresheld.
#' @param type Type of p-value (\code{"nominal"} or \code{"adjusted"})
#' @param tests Restrict UpSet plot to these tests
#'
#' @keywords internal
NULL
