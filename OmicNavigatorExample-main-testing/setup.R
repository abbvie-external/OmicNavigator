# Installs the required packages from CRAN, Bioconductor, and GitHub.

cran <- c("BiocManager", "ggplot2", "gplots", "remotes", "rmarkdown", "viridis")
for (pkg in cran) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg)
  }
}

bioc <- c("edgeR", "limma", "Mus.musculus")
for (pkg in bioc) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    BiocManager::install(pkg, update = FALSE)
  }
}

github <- c("abbvie-external/OmicNavigator@*release")
remotes::install_github(github, dependencies = TRUE, upgrade = FALSE)
OmicNavigator::installApp()
