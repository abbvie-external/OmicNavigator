#' Start app on local machine
#'
#' After you have installed at least one OmicNavigator study package with
#' \code{\link{installStudy}}, you can explore the results in the app. The
#' function \code{startApp} starts a local instance of the app running on your
#' current machine. It will automatically open the app in your default browser.
#' For the best experience, use Google Chrome. From the dropdown menu, you will
#' be able to select from any of the studies you have installed on your machine.
#' When you are finished, you can stop the web server by returning to the R
#' console and pressing the Esc key (Windows) or Ctrl-C (Linux, macOS).
#'
#' Note that the app can't be run from within RStudio Server.
#'
#' The app requires some additional R packages to run. If you receive an error
#' about a missing package, please install it with
#' \code{\link[utils]{install.packages}}. To ensure you have all the extra
#' packages installed, you can run the command below:
#'
#' \preformatted{
#' install.packages(c("faviconPlease", "opencpu", "UpSetR"))
#' }
#'
#' @inheritParams opencpu::ocpu_start_app
#'
#' @export
startApp <- function(...) {
  for (appPkg in appPackages) {
    if (!requireNamespace(appPkg, quietly = TRUE)) {
      stop(
        sprintf("Install the package \"%s\" to run the app locally", appPkg)
      )
    }
  }

  www <- system.file("www/", package = "OmicNavigator")
  if (identical(www, "") || !dir.exists(www)) {
    stop("The app is not installed with the OmicNavigator package.\n",
         "Install it with installApp()\n")
  }

  opencpu::ocpu_start_app("OmicNavigator", ...)
}

#' Install the OmicNavigator web app
#'
#' In order to run the OmicNavigator web app on your local machine, the app must
#' be installed in the \code{www/} subdirectory of the R package. If you
#' installed the release tarball from the GitHub Releases page, then you already
#' have the app installed. If you installed directly from GitHub with
#' \code{install_github}, or if you want to use a different version of the app,
#' you can manually download and install the app.
#'
#' @param version Version of the web app to install, e.g. \code{"1.0.0"}
#' @param overwrite Should an existing installation of the app be overwritten?
#' @param ... Passed to \code{\link[utils]{download.file}}. If the download
#'   fails, you may need to adjust the download settings for your operating
#'   system. For example, to download with \code{wget}, pass the argument
#'   \code{method = "wget"}.
#' @inheritParams base::system.file
#'
#' @export
installApp <- function(version = NULL, overwrite = FALSE, lib.loc = NULL, ...) {

  if (is.null(version)) {
    version <- versionAppPinned
  }

  installDir <- system.file(package = "OmicNavigator", lib.loc = lib.loc)
  if (!dir.exists(installDir)) {
    stop("Unable to find installation directory. Was the OmicNavigator R package installed?")
  }

  dirAppFinal <- file.path(installDir, "www")
  if (dir.exists(dirAppFinal) && !overwrite) {
    stop("The app is already installed. Set overwrite=TRUE to replace it.")
  }

  zipurl <- paste0(
    "https://github.com/",
    "abbvie-external/",
    "OmicNavigatorWebApp",
    "/releases/download/v",
    version,
    "/build.zip"
  )
  zipfile <- file.path(installDir, "build.zip")

  message("Installation plan:")
  message("  Version: ", version)
  message("  URL: ", zipurl)
  message("  Installing to ", dirAppFinal)

  utils::download.file(url = zipurl, destfile = zipfile, quiet = TRUE, ...)
  utils::unzip(zipfile, exdir = installDir)
  unlink(zipfile)

  dirAppTmp <- file.path(installDir, "build")
  if (overwrite) unlink(dirAppFinal, recursive = TRUE, force = TRUE)
  dir.create(dirAppFinal, showWarnings = FALSE)
  file.copy(from = file.path(dirAppTmp, "."), to = dirAppFinal, recursive = TRUE)
  unlink(dirAppTmp, recursive = TRUE, force = TRUE)

  message("Success!")
  return(invisible(dirAppFinal))
}
