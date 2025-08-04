# The minimum version of OmicNavigator that is still compatible with the current
# version. If a study package is created with a version less than this minimum
# version, then listStudies() will throw a warning.
minVersionCompatible <- "0.24.0"

# The default prefix added to OmicNavigator study packages. Used by .onLoad()
# below and by getPrefix() in utility.R. The default is overriden by setting
# the package option OmicNavigator.prefix.
OmicNavigatorPrefix <- "ONstudy"

# The R package is meant to be used with a specific version of the app. If a
# user has an older or newer version installed, send a warning.
versionAppPinned <- "2.0.0"

# The extra packages required to run the app
appPackages <- c(
  "faviconPlease",
  "opencpu",
  "UpSetR"
)

# Configure OmicNavigator options when package is loaded
.onLoad <- function(libname, pkgname) {
  # Default options
  onOptions <- list(
    OmicNavigator.prefix = OmicNavigatorPrefix
  )

  # Only set defaults for OmicNavigator options that have not been set by user
  for (i in seq_along(onOptions)) {
    optionName <- names(onOptions)[i]
    optionValue <- onOptions[i]
    optionCurrent <- getOption(optionName)
    if (is.null(optionCurrent)) {
      options(onOptions[i])
    }
  }

  return(NULL)
}

# Report versions of package and app when package is first attached
.onAttach <- function(libname, pkgname) {
  versionPackage <- utils::packageVersion("OmicNavigator")
  versionPackageMessage <- sprintf("OmicNavigator R package version: %s",
                                   versionPackage)
  packageStartupMessage(versionPackageMessage)

  appDir <- system.file("www", package = "OmicNavigator")
  manifestFile <- file.path(appDir, "manifest.json")
  if (file.exists(manifestFile)) {
    manifest <- jsonlite::read_json(manifestFile)
    versionApp <- manifest[["version"]]
  } else {
    versionApp <- NA_character_
  }
  if (is.na(versionApp)) {
    packageStartupMessage("The app is not installed. Install it with installApp()")
  } else {
    versionAppMessage <- paste("OmicNavigator app version:", versionApp)
    packageStartupMessage(versionAppMessage)
    if (versionApp != versionAppPinned) {
      warning("Expected app version: ", versionAppPinned, call. = FALSE)
    }
  }
}

#' OmicNavigator
#'
#' Package options to control package-wide behavior are described below.
#'
#' The default prefix for OmicNavigator study packages is "ONstudy". If you
#' would prefer to use a different prefix, you can change the package option
#' \code{OmicNavigator.prefix}. For example, to use the prefix "OmicNavigatorStudy",
#' you could add the following line to your \code{.Rprofile} file.
#'
#' \preformatted{
#' options(OmicNavigator.prefix = "OmicNavigatorStudy")
#' }
#'
"_PACKAGE"

# Make the internal package functions "data.table aware"
# https://rdatatable.gitlab.io/data.table/articles/datatable-importing.html#data-table-in-imports-but-nothing-imported-1
.datatable.aware <- TRUE

# Avoid NOTE from `R CMD check`: no visible binding for global variable '.data'
.data <- NULL
# Used by {ggplot2} for example plots in tests.R. The official guidance is to
# import rlang::.data into the NAMESPACE, but it's not worth adding {rlang} to
# Imports for this minor test usage.
# https://ggplot2.tidyverse.org/articles/ggplot2-in-packages.html#using-aes-and-vars-in-a-package-function
