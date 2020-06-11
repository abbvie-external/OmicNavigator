# Functions to run the app on local machine

#' Install the app on your local machine
#'
#' Requires npm. Requires AbbVie VPN connection.
#'
#' Currently doesn't work because ***REMOVED*** requires a username/password even when
#' download the zipfile or cloning the repo via HTTPS, even though the repo
#' is public.
#'
#' @noRd
installApp <- function() {
  npmExecutable <- Sys.which("npm")
  if (is.null(npmExecutable) || identical(npmExecutable, "")) {
    stop("npm is required to install the app locally")
  }

  zipurl <- "https://***REMOVED***/IProt/OmicAnalyzer/archive/master.zip"
  zipfile <- tempfile()
  download.file(
    url = zipurl,
    destfile = zipfile,
    quiet = TRUE
  )
  files <- utils::unzip(zipfile)
  directory <- dirname(files[1])

  system2("npm", args = c("install", directory))
  system2("npm", args = c("run", "build", directory))

  dirBuild <- file.path(directory, "build")
  dirPkg <- find.package("OmicAnalyzer")
  dirPkgApp <- file.path(dirPkg, "inst", "www")
  dir.create(dirPkgApp, showWarnings = FALSE, recursive = TRUE)
  file.copy(dirBuild, dirPkgApp)
}

#' Start app on local machine
#'
#' @noRd
startApp <- function(...) {
  if (!requireNamespace("opencpu", quietly = TRUE)) {
    stop("Install the package \"opencpu\" to run the app locally")
  }

  opencpu::ocpu_start_app("OmicAnalyzer", ...)
}
