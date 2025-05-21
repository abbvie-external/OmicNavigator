# Contributing instructions

Thanks for your interest in contributing to OmicNavigator! Please follow the
instructions below to prepare your contribution.

* [Branches](#branches)
* [Files and directories](#files-and-directories)
* [How to contribute with Git and GitHub](#how-to-contribute-with-git-and-github)
* [Setup your development environment](#setup-your-development-environment)
* [Updating NEWS.md](#updating-newsmd)
* [Documentation](#documentation)
* [Unit tests](#unit-tests)
* [GitHub Actions](#github-actions)
* [Run OmicNavigator with Docker](#run-omicnavigator-with-docker)
* [Tag a new release](#tag-a-new-release)
* [How to review and merge Pull Requests](#how-to-review-and-merge-pull-requests)
* [How to sync the dev branch](#how-to-sync-the-dev-branch)
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

    ```sh
    git clone https://github.com/<fork>/OmicNavigator.git
    cd OmicNavigator
    ```

1. Create a new branch. We recommend prefixing the branch name with `feature/`
or `bugfix/` to help classify it, but don't worry about this too much.

    ```sh
    git checkout -b feature/<x>
    ```

1. Make your edits. See the section below on setting up your development
environment.

1. Add, commit, push, and open a Pull Request against the "main" branch.

    ```sh
    git push origin feature/<x>
    ```

## Setup your development environment

First install the development only packages:

```R
install.packages(c("devtools", "remotes", "roxygen2"))
```

Second install the required and suggested dependencies:

```R
remotes::install_deps(dependencies = TRUE)
```

Third install LaTeX, Make, and Graphviz if you wish to re-build the
vignettes:

```sh
sudo apt-get install texlive texinfo make graphviz
```

If you’re on Windows or macOS, I recommend using the R package
[tinytex](https://cran.r-project.org/package=tinytex) to install the
minimal [TinyTex](https://yihui.org/tinytex/) distribution.

To install the app for local testing, the easiest method is to install it once
in the source directory, so that the app is always installed whenever you build
the package locally. You can do this by first loading the package with devtools
and then running `installApp()`, which will install the app to `inst/www/`:

```R
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

## Documentation

If you edit the function documentation in the `.R` files, you will need to
regenerate the corresponding `.Rd` files in `man/`.

1. Update the documentation files by running `devtools::document()` in the R
console or pressing Ctrl+Shift+D in RStudio

1. Reinstall the package with `devtools::install()` (Ctrl+Shift+B in RStudio).
Alternatively you could reload the package with `devtools::load_all()`
(Ctrl+Shift+L in RStudio), but note that any links in the documentation won't
work if the package is only loaded

1. Confirm that the function documentation has been updated by running
`?<name-of-function>`. You may have to refresh the help pane if you already had
it open in RStudio

## Unit tests

The unit tests are in `inst/tinytest/`. They use the testing framework
[tinytest][]. Each test file more or less corresponds to a file in `R/`, e.g.
`testAdd.R` tests the functions in `R/add.R`. Unit tests are highly encouraged
but not required for pull requests.

[tinytest]: https://cran.r-project.org/package=tinytest

After you make your changes, you can run all the tests by running the following
in the R console:

```R
tinytest::test_all()
```

For quicker feedback, you can run one specific test file. For example, if you
are making changes to the functions in `R/get.R`, you can run the corresponding
tests in `inst/tinytest/testGet.R` with:

```R
tinytest::run_test_file("inst/tinytest/testGet.R")
```

If you'd like to write additional tests (highly encouraged!), try to follow the
style of the surrounding code. In general, the tests are structured like below:

```R
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

* Note that the tests wrapped in `at_home()` are only executed when running
`tinytest::test_all()` or `tinytest::run_test_file()`. They are skipped when
running `test_package()` (which is what is called by `R CMD check`), and thus
these tests are not run in GitHub Actions or on CRAN servers.

Lastly, if you are making a large contribution, it can be helpful to evaluate
the comprehensiveness of your tests by calculating the test coverage. The covr
package runs the package tests and creates a report that details which lines
were executed by the tests, and most importantly, which lines were never run.
This will highlight any logic in your code that isn't being regularly tested.

```R
install.packages("covr")
library(covr)
cov <- package_coverage()
cov
report(cov)
```

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
name. Also note that the continuous integration jobs are only triggered if a
file that affects the behavior of the package has been modified. For example, if
you only edit documentation files like `README.md`, the tests won't be run.

## Run OmicNavigator with Docker

The repository includes a `Dockerfile` to install and run OmicNavigator. This is
convenient if you want to test changes you've made to the R package without
installing the dependencies on your local machine.

```sh
# Build the image
docker build -t omicnavigator .
# Run the image
docker run --name onapp -t -p 8004:8004 omicnavigator
```

Open the app in your browser at http://localhost:8004/ocpu/library/OmicNavigator/

When you're finished, stop and delete the container:

```sh
docker stop onapp
docker rm onapp
```

## Tag a new release

Follow these steps to tag a new release:

* Bump the version in `DESCRIPTION`. Make sure it only has 3 components
(major.minor.patch)

* Update `NEWS.md`. Manually add the section header `# major.minor.patch`

* Commit the changes

* Tag the release

    ```sh
    git tag -l -n9
    git tag -a v#.#.# -m "v#.#.#"
    ```

* Push the tag to GitHub

    ```sh
    git push origin --tags
    ```

* Monitor the GitHub Actions workflow [Create a release from a
tag][gh-actions-release]. It will create a new release, copy-paste the section
from `NEWS.md` to use as the release notes, build and upload the PDF vignettes,
and build and upload a tarball with the app pre-bundled

[gh-actions-release]: https://github.com/abbvie-external/OmicNavigator/actions/workflows/release.yml

## How to review and merge Pull Requests

Some notes and advice on Pull Requests:

* Pull Requests should be merged into the "main" branch
* The GitHub Actions workflows won't be triggered by PRs from first-time
  contributors to the repository. You'll need to check the edits they made and
  then allow the Actions to run
* Merge commits are disabled. Instead, you will have to decide whether to squash
  or rebase the commits. Squashing combines all the commits into one single
  commit. This is preferred when many small commits have been made in response
  to feedback and failing tests. You'll have the opportunity to edit the commit
  message to better convey the final contribution of the PR. If instead each
  commit in the PR is a standalone change, then you can choose to rebase the
  commits, which rewrites them on top of the "main" branch. Either way the
  submitter will need to delete their feature branch and then pull the latest
  from the official repo into their fork's "main" branch

## How to sync the dev branch

The "dev" branch exists to deploy experimental (and potentially breaking)
changes that need to be coordinated by the backend R package and the frontend
web app. However, it also needs to be kept in sync with the "main" branch.
Otherwise the latest changes to the R package won't be deployed to the dev
server.

Unfortunately, [GitHub does not allow fast-forward merges of Pull
Requests][so-no-fast-forward]. In other words, using the GitHub UI would require
rewriting the commit SHA, and then "main" could no longer be cleanly merged into
"dev".

[so-no-fast-forward]: https://stackoverflow.com/questions/60597400/how-to-do-a-fast-forward-merge-on-github

So instead, a developer with write-access needs to regularly sync "dev" by
running the following locally:

```sh
# in the local clone of their fork
git remote add upstream https://github.com/abbvie-external/OmicNavigator.git
git pull upstream main
git checkout dev
git merge main
git push upstream dev
git checkout main
```

## CRAN submission

Run the following additional tests prior to CRAN submission, and then update
`cran-comments.md` accordingly.

```R
devtools::check_win_devel()
rhub::validate_email()
rhub::check_for_cran(platform = "ubuntu-gcc-devel")
```

Next build the tarball (first delete `inst/www/` if you have the app installed
locally), and [submit the tarball][cran].

[cran]: https://cran.r-project.org/submit.html

```R
rm -r inst/www/
R CMD build .
```

Enter the name and email of the current package maintainer (role `cre` in
`DESCRIPTION`). Copy-paste the contents of `cran-comments.md` into the box
"Optional comment". Finally click "Upload the package".
