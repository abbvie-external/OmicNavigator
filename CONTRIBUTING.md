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
