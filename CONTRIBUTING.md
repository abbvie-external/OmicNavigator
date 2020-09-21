# Contributing instructions

Thanks for your interest in contributing to OmicAnalyzer!

## Branches

Branch        | Purpose
------------- | -------------
stable        | Contains latest stable version (**default**)
release       | Matches the latest [release](https://***REMOVED***/***REMOVED***/OmicAnalyzer/releases)
dev           | Contains experimental new features
feature-\<x\> | Implements feature \<x\>
bugfix-\<x\>  | Fixes bug \<x\>

Lifecycle of branches:

```
feature-<x>
           \
            \
             dev ---> stable ---> release
                     /
                    /
          bugfix-<x>
```

## How to contribute

> I want to implement a new feature

Work on the dev branch.

```
git clone https://***REMOVED***/<fork>/OmicAnalyzer.git
git checkout dev
git checkout -b feature-<x>
# implement feature, add, commit
git push origin feature-<x>
```

Submit a pull request to the dev branch.

> I want to fix a bug, typo, etc

Work on the stable branch.

```
git clone https://***REMOVED***/<fork>/OmicAnalyzer.git
git checkout stable
git checkout -b bugfix-<x>
# fix bug, add, commit
git push origin bugfix-<x>
```

Submit a pull request to the stable branch.

## Updating NEWS.md

If your pull request affects the end user experience, please add a bullet to
`NEWS.md`. At minimum include a reference to your pull request. Additionally
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

* If the messages returned by OmicAnalyzer functions are obscuring the test
output too much, you can wrap the tinytest function call with
`suppressMessages()`. This will suppress the messages from the OmicAnalyzer
functions but still display the test results.

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

# Files and directories

* `R/` - R source code files
* `inst/tinytest/` - Test files
* `inst/www/` - Web app
* `scripts/` - Utility scripts for maintaining the package
