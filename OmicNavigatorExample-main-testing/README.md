# How to create an OmicNavigator study

This repository contains an example of how to use the [OmicNavigator R
package][on-rpkg] to convert an omic analysis into a study package to be
explored with the [OmicNavigator app][on-app]. For more details, please see the
User's Guide attached to the [latest release][latest].

[on-rpkg]: https://github.com/abbvie-external/OmicNavigator
[on-app]: https://github.com/abbvie-external/OmicNavigatorWebApp
[latest]: https://github.com/abbvie-external/OmicNavigator/releases/latest

**Files:**

* [`setup.R`](./setup.R) - Installs the required R packages for the example

* [`data/`](./data/) - The input RNA-seq counts and MSigDB annotations

* [`analyze.Rmd`](./analyze.Rmd) - Performs a differential expression and enrichment
analysis of the RNA-seq experiment in [`data/`](./data/). It uses limma+voom,
but OmicNavigator is agnostic to how you perform your analysis. If you prefer,
you could use Python or a GUI to perform the analysis, as long as you export the
results. Also, the HTML produced by this Rmd is included as an external report
in the final OmicNavigator study package (note: including a report file is optional).

* [`results/`](./results/) - The analysis results exported by
[`analyze.Rmd`](./analyze.Rmd)

* [`build.R`](./build.R) - Builds the OmicNavigator study package from the
results files that [`analyze.Rmd`](./analyze.Rmd) exported to
[`results/`](./results/). Installs the study package and starts the web app.

## Run the code

Follow the steps below to install the dependencies, perform the analysis, and
create the OmicNavigator study package. Note that the analysis results are
already available in `results/`, so if you want you can skip running
`analysis.Rmd` and go straight to running `build.R`.

1. Install R package dependencies

    ```
    source("setup.R", local = new.env())
    ```

1. Perform the differential expression analysis. This reads the input files in
`data/` and exports the output files to `results/`

    ```
    library(rmarkdown)
    render("analyze.Rmd", output_file = "results/report.html", envir = new.env())
    ```

1. Create and install the OmicNavigator study package. This reads the analysis
results files in `results/`, converts them to an OmicNavigator study package,
installs the package, and starts the app.

    ```
    source("build.R")
    ```

## Acknowledgements

The example limma+voom code was adapted from the Bioconductor workflow
[RNAseq123](https://bioconductor.org/packages/release/workflows/html/RNAseq123.html).

If you use the code, please cite:

> Law CW, Alhamdoosh M, Su S et al. RNA-seq analysis is easy as 1-2-3
> with limma, Glimma and edgeR [version 2; referees: 3 approved].
> F1000Research 2016, 5:1408 (doi: 10.12688/f1000research.9005.2)

If you use the data, please cite:

> Sheridan JM, Ritchie ME, Best SA, et al.: A pooled shRNA screen for
> regulators of primary mammary stem and progenitor cells identifies
> roles for Asap1 and Prox1. BMC Cancer. 2015; 15(1): 221.
