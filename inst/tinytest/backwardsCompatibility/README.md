# Backwards compatibility

These study package tarballs were created with older versions of OmicNavigator.

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
exportStudy(x, type = "tarball", path = "inst/backwardsCompatiblity")
```
