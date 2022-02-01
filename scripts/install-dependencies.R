# Use RSPM to install package binaries
options(HTTPUserAgent = sprintf("R/%s R (%s)", getRversion(), paste(getRversion(), R.version$platform, R.version$arch, R.version$os)))
options(repos = c(CRAN = "https://packagemanager.rstudio.com/all/__linux__/bionic/2022-01-21+Y3JhbiwyOjQ1MjYyMTU7MTE5RDgxNDQ"))

if (!requireNamespace("remotes", quietly = TRUE)) {
  install.packages("remotes")
}

remotes::install_deps(dependencies = TRUE)
