# OmicNavigator

The R backend for the OmicNavigator app deployed at
***REMOVED***/ocpu/library/OmicNavigator/

## Download

Download the tarball from the [releases page][releases] on ***REMOVED***. It will look
like `OmicNavigator_x.x.x.tar.gz`, where `x.x.x` corresponds to the version.

[releases]: ***REMOVED***/releases

You need to download the tarball in order to run the app. If you clone the Git
repository, you will only obtain the R code without the app.

## Installation from the R console

1. Install dependencies:

    a. Full installation. This is required if you want to run the app from R.

        ```
        remotes::install_deps("OmicNavigator_x.x.x.tar.gz", dependencies = TRUE)
        ```

    a. Minimal installation. If you only need to create an OmicNavigator study,
       you can install many fewer dependencies.

        ```
        remotes::install_deps("OmicNavigator_x.x.x.tar.gz")
        ```

1. Install OmicNavigator:

    ```
    install.packages("OmicNavigator_x.x.x.tar.gz", repos = NULL)
    ```

## Installation from the terminal

1. Install dependencies:

    a. Full installation. This is required if you want to run the app from R.

        ```
        Rscript -e 'remotes::install_deps("OmicNavigator_x.x.x.tar.gz", dependencies = TRUE)'
        ```

    a. Minimal installation. If you only need to create an OmicNavigator study,
       you can install many fewer dependencies.

        ```
        Rscript -e 'remotes::install_deps("OmicNavigator_x.x.x.tar.gz")'
        ```

1. Install OmicNavigator:

    ```
    R CMD INSTALL --no-multiarch --with-keep.source OmicNavigator_x.x.x.tar.gz
    ```

## Installation troubleshooting

Some of the R package dependencies require external software libraries to
already be installed on your machine (e.g. the R package curl requires the
system library libcurl). Unfortunately R is unable to install these for you
automatically. If you are using Linux/macOS, you will need to install these
system libraries yourself first.

If you are using Debian/Ubuntu, you can install all of the required system
libraries using the commands below:

```
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
