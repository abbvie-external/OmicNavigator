<img src="man/figures/omicnavigator-dark-text.png"/>

[![CRAN status](https://www.r-pkg.org/badges/version/OmicNavigator)](https://cran.r-project.org/package=OmicNavigator)
[![R CMD check status from GitHub Actions](https://github.com/abbvie-external/OmicNavigator/workflows/Comprehensive%20test/badge.svg)](https://github.com/abbvie-external/OmicNavigator/actions/workflows/comprehensive.yml)
[![Monthly downloads](https://cranlogs.r-pkg.org/badges/OmicNavigator)](https://r-pkg.org/pkg/OmicNavigator)

# OmicNavigator: Open-Source Software for Omic Data Analysis and Visualization 

Exploring the results of omic analyses via interactive web applications
facilitates cross-disciplinary collaboration and biological discovery.
OmicNavigator is open-source software for the archival and interactive
exploration of results from high-throughput biology experiments. The software
enables omic data analysts (typically bioinformaticians) to create customizable
web applications from the results of their work using only the statistical
programming language, R. OmicNavigator is an R package that contains
web application code, R functions for programmatic access and data deposition,
and a new container for the storage of measurements (e.g. RNAseq read counts),
statistical analyses (differential expression and/or enrichment analysis),
metadata, and custom plotting functions. Studies created with OmicNavigator are
themselves R packages, accessible via a JavaScript dashboard that can be
interrogated on the user’s local machine or deployed online to be explored by
collaborators. The dashboard combines user-defined study results and feature
plots with performant tables and interactive graphics common to bioinformatic
analyses such as scatter, network, volcano, and barcode plots. The tool also
includes dynamic, multi-set filtering across hypothesis tests based on
user-defined thresholds such as statistical significance or effect size.

This repository contains the R package. To see an example of how to create an
OmicNavigator study, check out the repository [OmicNavigatorExample][]. If
you're a developer interested in the web application code, check out the
repository [OmicNavigatorWebApp][].

[OmicNavigatorWebApp]: https://github.com/abbvie-external/OmicNavigatorWebApp
[OmicNavigatorExample]: https://github.com/abbvie-external/OmicNavigatorExample

## Features

* Simple, yet flexible **study-model-test** data model for differential and/or enrichment analyses
* Well documented data-in functions and a novel data container
* _User-defined_ results and feature plots
* Richly featured tables
* Interactive scatter/volcano plots
* Interactive barcode plots for enrichment results
* Dynamic network views of enriched terms
* Multi-set filtering across hypothesis tests

![](./inst/OmicNavigator.gif "GIF of OmicNavigator WebApp Features")

## Quick start

To get a quick sense of how the app works, the quick start instructions below
install the package, create a very minimal study, and then starts the app. It
should automatically open a new tab in your browser. When you're done, press
ctrl+c or Esc to stop running the app.

```R
# Install and load the package
install.packages("OmicNavigator", dependencies = TRUE)
library(OmicNavigator)

# Create a very minimal study with a single results table
quickstart <- createStudy("quickstart")
data("RNAseq123")
head(basal.vs.lp)
resultsTable <- basal.vs.lp[, -2:-3]
quickstart <- addResults(quickstart, list(model = list(test = resultsTable)))
installStudy(quickstart)

# (optional) Install the example study RNAseq123 which demos many of the app's
# available features
install.packages(c("gplots", "viridis"))
tarball <- "https://github.com/abbvie-external/OmicNavigatorExample/releases/latest/download/ONstudyRNAseq123.tar.gz"
install.packages(tarball, repos = NULL)

# Install and start the web app
installApp()
startApp()
```

To learn how to add your own data to the app, please read the User's Guide
attached to the [latest GitHub Release][release-latest]. The more data you add,
the more features are automatically enabled in the app (e.g. the Enrichment
Analysis tab is available once you add the results of an enrichment analysis).

## Installation

OmicNavigator is a [web
app](https://github.com/abbvie-external/OmicNavigatorWebApp) and R code bundled
together as an R package. You can install OmicNavigator multiple ways.

### Full installation in 2 steps (recommended)

This is the quickest and easiest method for you to be able to install
OmicNavigator and run the web app on your local machine.

1. Install the OmicNavigator R package directly from CRAN:

    ```R
    install.packages("OmicNavigator", dependencies = TRUE)
    ```

1. Install the web app:

    ```R
    library(OmicNavigator)
    installApp()
    ```

### Full installation from release tarball

Alternatively, you can install the OmicNavigator release tarball, which already
includes the web app bundled with the R package.

1. Download the tarball from the [latest GitHub Release][release-latest]. It
will look like `OmicNavigator_x.x.x.tar.gz`, where `x.x.x` corresponds to the
version.

    [release-latest]: https://github.com/abbvie-external/OmicNavigator/releases/latest

1. Install the dependencies:

    ```R
    remotes::install_deps("OmicNavigator_x.x.x.tar.gz", dependencies = TRUE)
    ```

1. Install OmicNavigator from the release tarball:

    1) In the R console:

        ```R
        install.packages("OmicNavigator_x.x.x.tar.gz", repos = NULL)
        ```

    1) In the terminal:

        ```sh
        R CMD INSTALL --no-multiarch --with-keep.source OmicNavigator_x.x.x.tar.gz
        ```

### Minimal installation

If you are only using the R package functions, and don't need to run the app,
you can perform a minimal installation. This is useful if you are using
OmicNavigator in a data engineering pipeline or as part of a continuous
integration build.

1. Install the OmicNavigator R package directly from CRAN, without the extra
dependencies required to run the app:

    ```R
    install.packages("OmicNavigator")
    ```

### Install development version from GitHub

If you need access to the latest changes to OmicNavigator, you can install the
development version of the package directly from GitHub.

1. Install the OmicNavigator R package directly from GitHub:

    ```R
    remotes::install_github("abbvie-external/OmicNavigator", dependencies = TRUE)
    ```

1. Install the web app:

    ```R
    library(OmicNavigator)
    installApp()
    ```

### Installation troubleshooting

Some of the R package dependencies require external software libraries to
already be installed on your machine (e.g. the R package curl requires the
system library libcurl). Unfortunately R is unable to install these for you
automatically. If you are using Linux/macOS, you will need to install these
system libraries yourself first.

If you are using Debian/Ubuntu, you can install all of the required system
libraries using the commands below:

```sh
sudo apt update
sudo apt install \
  libcurl4-openssl-dev \
  libssl-dev \
  libprotobuf-dev \
  protobuf-compiler \
  libxml2-dev
```

Here is a table of the known system dependencies that you will need to install.

R package | System library | Operating system | Installation
--------- | -------------- | ---------------- | ------------
curl | libcurl | Debian/Ubuntu | libcurl4-openssl-dev
curl | libcurl | Fedora/CentOS/RHEL | libcurl-devel
curl | libcurl | Solaris | libcurl_dev
openssl | openssl | Debian/Ubuntu | libssl-dev
openssl | openssl | Fedora/CentOS/RHEL | openssl-devel
openssl | openssl | Solaris | libssl_dev
openssl | openssl | macOS (Homebrew) | openssl@1.1
protolite | protobuf | Debian/Ubuntu | libprotobuf-dev
protolite | protobuf | Fedora/CentOS/RHEL | protobuf-devel
protolite | protobuf | Solaris | protobuf_dev
protolite | protobuf | macOS (Homebrew) | protobuf
protolite | protobuf-compiler | Debian/Ubuntu | protobuf-compiler
protolite | protobuf-compiler | Fedora/CentOS/RHEL | protobuf-compiler
xml2 | libxml-2.0 | Debian/Ubuntu | libxml2-dev
xml2 | libxml-2.0 | Fedora/CentOS/RHEL | libxml2-devel
xml2 | libxml-2.0 | Solaris | libxml2_dev

## Acknowledgements

OmicNavigator is deployed as an [OpenCPU](https://www.opencpu.org/ "OpenCPU") app. The OpenCPU system enables embedded scientific computing with R.

OmicNavigator development is influenced by many existing bioinformatics and data visualization tools, including the set analysis visualization tool [UpSet](https://github.com/VCG/upset "UpSet") and the enrichment analysis network visualization tool [EnrichmentMap](https://github.com/BaderLab/EnrichmentMapApp "EnrichmentMap")

## Contributing and Code of Conduct

Please note that the OmicNavigator project is released with guidelines on how to best contribute and a code of conduct. 
You can find these documents in `CONTRIBUTING.md` and `.github/CODE_OF_CONDUCT.md`.
