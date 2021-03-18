# Install dependencies needed for development only

# Use RSPM to install package binaries
options(HTTPUserAgent = sprintf("R/%s R (%s)", getRversion(), paste(getRversion(), R.version$platform, R.version$arch, R.version$os)))
options(repos = c(CRAN = "https://packagemanager.rstudio.com/cran/__linux__/bionic/latest"))

devPkgs <- c(
  "commonmark", # for utils::news() to create release notes
  "devtools", # for load_all() to run installApp()
  "roxygen2", # to build documentation
  "xml2" # for utils::news() to create release notes
)

for (pkg in devPkgs) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    message("Installing ", pkg)
    install.packages(pkg)
  }
}
