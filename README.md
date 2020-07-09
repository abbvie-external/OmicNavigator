# OmicAnalyzer

The R backend for the OmicAnalyzer app deployed at
***REMOVED***/ocpu/library/OmicAnalyzer/

## Download

Download the tarball from the [releases page][releases] on ***REMOVED***. It will look
like `OmicAnalyzer_x.x.x.tar.gz`, where `x.x.x` corresponds to the version.

[releases]: https://***REMOVED***/***REMOVED***/OmicAnalyzer/releases

You need to download the tarball in order to run the app. If you clone the Git
repository, you will only obtain the R code without the app.

## Installation from the R console

1. Install dependencies:

    ```
    if (!requireNamespace("remotes", quietly = TRUE))
      install.packages("remotes")
    remotes::install_deps("OmicAnalyzer_x.x.x.tar.gz", dependencies = TRUE)
    ```

1. Install OmicAnalyzer:

    ```
    install.packages("OmicAnalyzer_x.x.x.tar.gz", repos = NULL)
    ```

## Installation from the terminal

1. Install dependencies:

    ```
    Rscript -e 'if (!requireNamespace("remotes", quietly = TRUE)) install.packages("remotes")'
    Rscript -e 'remotes::install_deps("OmicAnalyzer_x.x.x.tar.gz", dependencies = TRUE)'
    ```

1. Install OmicAnalyzer:

    ```
    R CMD INSTALL --no-multiarch --with-keep.source OmicAnalyzer_x.x.x.tar.gz
    ```
