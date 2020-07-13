#' Start app on local machine
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
