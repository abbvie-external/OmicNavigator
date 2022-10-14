# Use RSPM to install package binaries (Linux, Windows)
options(HTTPUserAgent = sprintf("R/%s R (%s)", getRversion(), paste(getRversion(), R.version$platform, R.version$arch, R.version$os)))
os <- Sys.info()["sysname"]
if (os == "Linux") {
  options(repos = c(CRAN = "https://packagemanager.rstudio.com/all/__linux__/focal/2022-10-13+Y3JhbiwyOjQ1MjYyMTU7RjQxMTNEM0U"))
}
if (os == "Windows") {
  options(repos = c(CRAN = "https://packagemanager.rstudio.com/all/2022-10-13+Y3JhbiwyOjQ1MjYyMTU7RjQxMTNEM0U"))
}

if (!requireNamespace("remotes", quietly = TRUE)) {
  install.packages("remotes")
}

remotes::install_deps(dependencies = TRUE)
