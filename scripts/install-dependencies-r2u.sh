#!/bin/bash
set -eu

# Install dependencies with APT/r2u

# Imports
imports="\
  r-cran-data.table \
  r-cran-jsonlite \
"

# Suggests
suggests="\
  r-cran-faviconplease \
  r-cran-ggplot2 \
  r-cran-opencpu \
  r-cran-plotly \
  r-cran-tinytest \
  r-cran-ttdo \
  r-cran-upsetr
"

# Dev-only
#
# * graphviz - to create diagrams for vignettes
# * commonmark - for utils::news() to create release notes
# * devtools - for load_all() to run installApp()
# * roxygen2 - to build documentation
# * xml2 - for utils::news() to create release notes
dev="\
  graphviz \
  r-cran-commonmark \
  r-cran-devtools \
  r-cran-roxygen2 \
  r-cran-xml2 \
"

# Install
apt-get install --yes $imports $suggests $dev

# List installed R packages
apt list --installed 'r-*'
