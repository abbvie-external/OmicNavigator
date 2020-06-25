# OmicAnalyzer

The R backend for the OmicAnalyzer app deployed at
***REMOVED***

## Installation

1. Download the tarball from the [releases page][releases] on ***REMOVED***. It will
look like `OmicAnalyzer_x.x.x.tar.gz`, where `x.x.x` corresponds to the version.

[releases]: https://***REMOVED***/***REMOVED***/OmicAnalyzer/releases

1. Install dependencies:

    ```
    Rscript -e 'remotes::install_deps("OmicAnalyzer_x.x.x.tar.gz")'
    ```
    
    The above step requires the remotes package. Install it if needed:
    
    ```
    if (!requireNamespace("remotes", quietly = TRUE))
      install.packages("remotes")
    ```

1. Install OmicAnalyzer:

    ```
    R CMD INSTALL --no-multiarch --with-keep.source OmicAnalyzer_x.x.x.tar.gz
    ```
