# Backwards compatibility

These study package tarballs were created with older versions of OmicNavigator.

## 2025-12-22 - transformations

Add multiple transformations for assays and metaAssays

```R
library("OmicNavigator")
packageVersion("OmicNavigator")

desc <- sprintf(
  "Test study package with transformations created on %s with OmicNavigator %s",
  Sys.Date(),
  packageVersion("OmicNavigator")
)

x <- OmicNavigator:::testStudy(
  name = sprintf("test%s", format(Sys.Date(), "%Y%m%d")),
  description = desc,
  version = "0.1.0",
  maintainer = "My Name",
  maintainerEmail = "me@domain.com"
)
x <- addPlots(x, OmicNavigator:::testPlots())

assaysDataFrame <- OmicNavigator:::testAssays(n = 1)[[1]]
x <- addAssays(
  x,
  assays = list(
    model_04 = list(
      a1 = assaysDataFrame,
      a2 = assaysDataFrame + 1
    )
  )
)

metaAssaysDataFrame <- OmicNavigator:::testMetaAssays(n = 1)[[1]]
x <- addMetaAssays(
  x,
  metaAssays = list(
    model_04 = list(
      a1 = metaAssaysDataFrame,
      a2 = metaAssaysDataFrame + 1
    )
  )
)

exportStudy(x, type = "tarball", path = "inst/tinytest/backwardsCompatibility/")
```

## 2025-10-15

```R
library("OmicNavigator")
packageVersion("OmicNavigator")

desc <- sprintf(
  "Test study package created on %s with OmicNavigator %s",
  Sys.Date(),
  packageVersion("OmicNavigator")
)

x <- OmicNavigator:::testStudy(
  name = sprintf("test%s", format(Sys.Date(), "%Y%m%d")),
  description = desc,
  version = "0.1.0",
  maintainer = "My Name",
  maintainerEmail = "me@domain.com"
)
x <- addPlots(x, OmicNavigator:::testPlots())
exportStudy(x, type = "tarball", path = "inst/tinytest/backwardsCompatibility/")
```
