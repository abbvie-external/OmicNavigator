# OmicAnalyzer

## Installation

1. Download from ***REMOVED***:

    ```
    # HTTPS
    git clone https://***REMOVED***/***REMOVED***/OmicAnalyzer.git
    # SSH
    git clone  git@***REMOVED***:***REMOVED***/OmicAnalyzer.git
    ```

1. Install dependencies:

    ```
    Rscript -e 'remotes::install_deps("OmicAnalyzer")'
    ```
    
    The above step requires the remotes package. Install it if needed:
    
    ```
    if (!requireNamespace("remotes", quietly = TRUE))
      install.packages("remotes")
    ```

1. Install OmicAnalyzer:

    ```
    R CMD INSTALL --no-multiarch --with-keep.source OmicAnalyzer
    ```


Alternatively, you can install directly from ***REMOVED*** if you save a GitHub Personal
Access Token (PAT) in the environment variable `GITHUB_PAT`:

```
remotes::install_github(repo = "***REMOVED***/OmicAnalyzer",
                        host = "***REMOVED***/api/v3")
```
