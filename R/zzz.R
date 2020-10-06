# The minimum version of OmicAnalyzer that is still compatible with the current
# version. If a study package is created with a version less than this minimum
# version, then listStudies() will throw a warning.
minVersionCompatible <- "0.13.0"

# The default prefix added to OmicAnalyzer study packages. Used by .onLoad()
# below and by getPrefix() in utility.R. The default is overriden by setting
# the package option OmicAnalyzer.prefix.
OmicAnalyzerPrefix <- "OAstudy"

# Configure OmicAnalzyer options when package is loaded
.onLoad <- function(libname, pkgname) {
  # Default options
  oaOptions <- list(
    OmicAnalyzer.prefix = OmicAnalyzerPrefix
  )

  # Only set defaults for OmicAnalyzer options that have not been set by user
  for (i in seq_along(oaOptions)) {
    optionName <- names(oaOptions)[i]
    optionValue <- oaOptions[i]
    optionCurrent <- getOption(optionName)
    if (is.null(optionCurrent)) {
      options(oaOptions[i])
    }
  }

  return(NULL)
}

#' OmicAnalyzer
#'
#' Package options to control package-wide behavior are described below.
#'
#' The default prefix for OmicAnalyzer study packages is "OAstudy". If you
#' would prefer to use a different prefix, you can change the package option
#' \code{OmicAnalyzer.prefix}. For example, to use the prefix "OmicAnalyzerStudy",
#' you could add the following line to your \code{.Rprofile} file.
#'
#' \preformatted{
#' options(OmicAnalyzer.prefix = "OmicAnalyzerStudy")
#' }
#'
#' @docType package
#' @name OmicAnalyzer
NULL
