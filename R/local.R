#' Start app on local machine
#'
#' After you have installed at least one OmicAnalyzer study package with
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
#' @inheritParams opencpu::ocpu_start_app
#'
#' @export
startApp <- function(...) {
  if (!requireNamespace("opencpu", quietly = TRUE)) {
    stop("Install the package \"opencpu\" to run the app locally")
  }

  www <- system.file("www/", package = "OmicAnalyzer")
  if (identical(www, "") || !dir.exists(www)) {
    if (interactive()) on.exit(openReleasesPage(), add = TRUE)
    stop("The app is not installed with the OmicAnalyzer package.\n",
         "Make sure you install the release tarball of OmicAnalyzer.\n",
         "Go to https://***REMOVED***/***REMOVED***/OmicAnalyzer/releases/\n",
         "and download the asset named OmicAnalyzer_x.x.x.tar.gz\n",
         "where x.x.x is the package version.\n")
  }

  opencpu::ocpu_start_app("OmicAnalyzer", ...)
}

openReleasesPage <- function() {
  url <- "https://***REMOVED***/***REMOVED***/OmicAnalyzer/releases/"
  answer <- readline(
    "Would you like R to open the releases page in your browser? (y/N) "
  )
  if (tolower(answer) == "y") utils::browseURL(url)
}
