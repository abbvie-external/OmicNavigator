# Install dependencies needed for development only

# Used by GitHub Action workflow release.yml

# Use RSPM to install package binaries (Linux, Windows)
options(HTTPUserAgent = sprintf("R/%s R (%s)", getRversion(), paste(getRversion(), R.version$platform, R.version$arch, R.version$os)))
os <- Sys.info()["sysname"]
if (os == "Linux") {
  options(repos = c(CRAN = "https://packagemanager.rstudio.com/all/__linux__/focal/2022-10-13+Y3JhbiwyOjQ1MjYyMTU7RjQxMTNEM0U"))
}
if (os == "Windows") {
  options(repos = c(CRAN = "https://packagemanager.rstudio.com/all/2022-10-13+Y3JhbiwyOjQ1MjYyMTU7RjQxMTNEM0U"))
}

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
