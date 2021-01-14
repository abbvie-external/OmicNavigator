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
    if (interactive()) on.exit(openReleasesPage(), add = TRUE)
    stop("The app is not installed with the OmicNavigator package.\n",
         "Make sure you install the release tarball of OmicNavigator.\n",
         "Go to ***REMOVED***/releases/\n",
         "and download the asset named OmicNavigator_x.x.x.tar.gz\n",
         "where x.x.x is the package version.\n")
  }

  opencpu::ocpu_start_app("OmicNavigator", ...)
}

openReleasesPage <- function() {
  url <- "***REMOVED***/releases/"
  answer <- readline(
    "Would you like R to open the releases page in your browser? (y/N) "
  )
  if (tolower(answer) == "y") utils::browseURL(url)
}
