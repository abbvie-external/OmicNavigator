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
    stop("The app is not installed with the OmicAnalyzer package.\n",
         "Make sure you install the release tarball of OmicAnalyzer.")
  }

  opencpu::ocpu_start_app("OmicAnalyzer", ...)
}
