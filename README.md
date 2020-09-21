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
    install.packages("opencpu")
    remotes::install_deps("OmicAnalyzer_x.x.x.tar.gz")
    ```

1. Install OmicAnalyzer:

    ```
    install.packages("OmicAnalyzer_x.x.x.tar.gz", repos = NULL)
    ```

## Installation from the terminal

1. Install dependencies:

    ```
    Rscript -e 'install.packages("opencpu")'
    Rscript -e 'remotes::install_deps("OmicAnalyzer_x.x.x.tar.gz")'
    ```

1. Install OmicAnalyzer:

    ```
    R CMD INSTALL --no-multiarch --with-keep.source OmicAnalyzer_x.x.x.tar.gz
    ```

## Installation troubleshooting

Some of the R package dependencies require external software libraries to
already be installed on your machine (e.g. the R package curl requires the
system library libcurl). Unfortunately R is unable to these for you
automatically. If you are using Linux/macOS, you will need to install these
system libraries yourself first.

If you are using Debian/Ubuntu, you can install all of the required system
libraries using the commands below:

```
sudo apt update
sudo apt install libssl-dev libcurl4-openssl-dev libprotobuf-dev protobuf-compiler
```

Here is a table of the known system dependencies that you will need to install.

R package | System library | Operating system | Installation
--------- | -------------- | ---------------- | ------------
openssl | openssl | Debian/Ubuntu | libssl-dev
openssl | openssl | Fedora/CentOS/RHEL | openssl-devel
openssl | openssl | Solaris | libssl_dev
openssl | openssl | macOS (Homebrew) | openssl@1.1
curl | libcurl | Debian/Ubuntu | libcurl4-openssl-dev
curl | libcurl | Fedora/CentOS/RHEL | libcurl-devel
curl | libcurl | Solaris | libcurl_dev
protolite | protobuf | Debian/Ubuntu | libprotobuf-dev
protolite | protobuf | Fedora/CentOS/RHEL | protobuf-devel
protolite | protobuf | Solaris | protobuf_dev
protolite | protobuf | macOS (Homebrew) | protobuf
protolite | protobuf-compiler | Debian/Ubuntu | protobuf-compiler
protolite | protobuf-compiler | Fedora/CentOS/RHEL | protobuf-compiler
