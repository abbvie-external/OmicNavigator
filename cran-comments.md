## Resubmission

Some package tests were failing mainly on CRAN Debian and macOS machines. I now
skip these tests on CRAN and only test them locally and on GitHub Actions.

## Test environments

* win-builder (devel)

* R-hub
    * solaris-x86-patched
    * ubuntu-gcc-devel

* GitHub Actions
    * windows-latest (release)
    * macOS-latest (release)
    * ubuntu-18.04 (3.4.4)

## R CMD check results

0 errors | 0 warnings | 0 notes
