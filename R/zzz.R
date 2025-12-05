# The minimum version of OmicNavigator that is still compatible with the current
# version. If a study package is created with a version less than this minimum
# version, then getStudyMeta() throws a warning.
checkMinVersionCompatible <- function(study, studyPackageVersion, minVersionCompatible = "0.24.0") {
  studyPackageVersion <- as.package_version(studyPackageVersion)
  minVersionCompatible <- as.package_version(minVersionCompatible)
  if (studyPackageVersion < minVersionCompatible) {
    warning(
      "OmicNavigator version incompatibility\n",
      sprintf("Study \"%s\" was created with version %s\n",
              study, studyPackageVersion),
      sprintf("OmicNavigator version %s is currently installed\n",
              packageVersion("OmicNavigator")),
      sprintf("It requires study packages to be created with a minimum OmicNavigator version of %s\n",
              minVersionCompatible),
      sprintf("Reinstall the study to avoid any potential issues\n"),
      immediate. = TRUE
    )
  }
}

# The default prefix added to OmicNavigator study packages. Used by .onLoad()
# below and by getPrefix() in utility.R. The default is overriden by setting
# the package option OmicNavigator.prefix.
OmicNavigatorPrefix <- "ONstudy"

# The R package is meant to be used with a specific version of the app. If a
# user has an older or newer version installed, send a warning.
versionAppPinned <- "2.2.8"

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
  versionPackage <- packageVersion("OmicNavigator")
  versionPackageMessage <- sprintf("OmicNavigator R package version: %s",
                                   versionPackage)
  packageStartupMessage(versionPackageMessage)

  appDir <- system.file("www", package = "OmicNavigator")
  manifestFile <- file.path(appDir, "manifest.json")
  if (file.exists(manifestFile)) {
    manifest <- read_json(manifestFile)
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
#' @importFrom data.table ":=" "%chin%" .N as.data.table chmatch data.table
#'   dcast.data.table fread fwrite melt merge.data.table setcolorder setDF setDT
#'   setnames setorderv
#' @importFrom graphics barplot par plot text
#' @importFrom jsonlite read_json write_json
#' @importFrom stats median pnorm prcomp rnorm runif setNames
#' @importFrom tools file_ext
#' @importFrom utils capture.output download.file getFromNamespace head
#'   install.packages installed.packages modifyList packageDescription
#'   packageVersion remove.packages unzip
#'
"_PACKAGE"

# Avoid NOTE from `R CMD check`: no visible binding for global variable '.data'
.data <- NULL
# Used by {ggplot2} for example plots in tests.R. The official guidance is to
# import rlang::.data into the NAMESPACE, but it's not worth adding {rlang} to
# Imports for this minor test usage.
# https://ggplot2.tidyverse.org/articles/ggplot2-in-packages.html#using-aes-and-vars-in-a-package-function
