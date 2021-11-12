# Contributing instructions

Thanks for your interest in contributing to OmicNavigator! Please follow the
instructions below to prepare your contribution.

* [Branches](#branches)
* [Files and directories](#files-and-directories)
* [How to contribute with Git and GitHub](#how-to-contribute-with-git-and-github)
* [Setup your development environment](#setup-your-development-environment)
* [Updating NEWS.md](#updating-newsmd)
* [Unit tests](#unit-tests)
* [GitHub Actions](#github-actions)
* [Run OmicNavigator with Docker](#run-omicnavigator-with-docker)
* [CRAN submission](#cran-submission)

## Branches

Branch        | Purpose
------------- | -------------
main          | Main development (**default**)
feature/\<x\> | Implements feature \<x\>
bugfix/\<x\>  | Fixes bug \<x\>
dev           | (Maintainer use only) Deploy to dev server to test experimental changes

## Files and directories

* `R/` - R source code files
* `inst/tinytest/` - Test files
* `inst/www/` - Web app
* `scripts/` - Utility scripts for maintaining the package

## How to contribute with Git and GitHub

Please follow the steps below to create and submit your proposed changes to
OmicNavigator. If you are new to Git and/or GitHub, please see the tutorial [A
Quick Introduction to Version Control with Git and GitHub][intro-git-github].

[intro-git-github]: https://doi.org/10.1371/journal.pcbi.1004668

1. Fork the repository to your personal account

1. Clone your fork to your local machine

    ```
    git clone https://github.com/<fork>/OmicNavigator.git
    cd OmicNavigator
    ```

1. Create a new branch. We recommend prefixing the branch name with `feature/`
or `bugfix/` to help classify it, but don't worry about this too much.

    ```
    git checkout -b feature/<x>
    ```

1. Make your edits. See the section below on setting up your development
environment.

1. Add, commit, push, and open a Pull Request against the "main" branch.

    ```
    git push origin feature/<x>
    ```

## Setup your development environment

First install the development only packages:

```
install.packages(c("remotes", "roxygen2"))
```

Second install the required and suggested dependencies:

```
remotes::install_deps(dependencies = TRUE)
```

Third install LaTeX, Make, and Graphviz if you wish to re-build the
vignettes:

```
sudo apt-get install texlive texinfo make graphviz
```

If you’re on Windows or macOS, I recommend using the R package
[tinytex](https://cran.r-project.org/package=tinytex) to install the
minimal [TinyTex](https://yihui.org/tinytex/) distribution.

To install the app for local testing, the easiest method is to install it once
in the source directory, so that the app is always installed whenever you build
the package locally. You can do this by first loading the package with devtools
and then running `installApp()`, which will install the app to `inst/www/`:

```
devtools::load_all()
installApp()
```

## Updating NEWS.md

If your pull request affects the end user experience, please add a bullet to
`NEWS.md`. At minimum include a reference to your pull request. Additional
useful information is your GitHub account and any related Issues.

For example:

```
* Fix bug in `nameOfFunction()` that caused a problem with... (#12, #13, @username)
```

## Unit tests

The unit tests are in `inst/tinytest/`. They use the testing framework
[tinytest][]. Each test file more or less corresponds to a file in `R/`, e.g.
`testAdd.R` tests the functions in `R/add.R`. Unit tests are highly encouraged
but not required for pull requests.

[tinytest]: https://cran.r-project.org/package=tinytest

After you make your changes, you can run all the tests by running the following
in the R console:

```
tinytest::test_all()
```

For quicker feedback, you can run one specific test file. For example, if you
are making changes to the functions in `R/get.R`, you can run the corresponding
tests in `inst/tinytest/testGet.R` with:

```
tinytest::run_test_file("inst/tinytest/testGet.R")
```

If you'd like to write additional tests (highly encouraged!), try to follow the
style of the surrounding code. In general, the tests are structured like below:

```
observed <- functionName(
  arg1,
  arg2
)

expect_identical(
  observed,
  expected
)
```

Some additional testing tips:

* If re-installing the package is taking up too much time, you can quickly load
the latest versions of all the package functions into the R console by running
`devtools::load_all(".")` (or Ctrl-Shift-L in RStudio).

* If the messages returned by OmicNavigator functions are obscuring the test
output too much, you can wrap the tinytest function call with
`suppressMessages()`. This will suppress the messages from the OmicNavigator
functions but still display the test results.

## GitHub Actions

We use [GitHub Actions](https://github.com/features/actions) for CI/CD. The
configuration scripts are in `.github/workflows/`.

* `comprehensive.yml` - Runs `R CMD check` on the R package on Windows, macOS,
and Linux. It is triggered by a push to the "main" branch or a Pull Request
submitted against the "main" branch.

* `release.yml` - Creates a new release. It is triggered by a new tag. It
downloads the app, builds a package tarball, builds the vignettes PDFs, and
uploads the tarball and PDFs as release assets.

* `quick.yml` - Quickly runs the tests and executes the code in the vignettes.
Designed for quick feedback. It is triggered by a push to any branch other than
the "main" branch.

If you wish to skip all automated CI, e.g. you are trying something experimental
that you know will break the tests, you can put "skip" anywhere in the branch
name. Also note that the continuous integrations jobs are only triggered if a
file that affects the behavior of the package has been modified. For example, if
you only edit documentation files like `README.md`, the tests won't be run.

## Run OmicNavigator with Docker

The repository includes a `Dockerfile` to install and run OmicNavigator. This is
convenient if you want to test changes you've made to the R package without
installing the dependencies on your local machine.

```
# Build the image
docker build -t omicnavigator .
# Run the image
docker run --name onapp -t -p 8004:8004 omicnavigator
```

Open the app in your browser at http://localhost:8004/ocpu/library/OmicNavigator/

When you're finished, stop and delete the container:

```
docker stop onapp
docker rm onapp
```

## CRAN submission

Run the following additional tests prior to CRAN submission.

```R
devtools::check_win_devel()
rhub::validate_email()
rhub::check_for_cran(platform = "solaris-x86-patched")
rhub::check_for_cran(platform = "ubuntu-gcc-devel")
```

Then update `cran-comments.md` accordingly, build the tarball, and [submit the
tarball][cran].

[cran]: https://cran.r-project.org/submit.html
