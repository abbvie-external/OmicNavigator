#!/usr/bin/env Rscript

# Bump the app version `versionAppPinned` in zzz.R and bump R package version
# in DESCRIPTION and NEWS.md.

args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 1) {
  stop("usage: Rscript bump-app-version.R <app-version>")
}
appVersionNew <- args[1]

if (!file.exists("DESCRIPTION")) {
  stop("bump-app-version.R must be executed in the root directory of the package")
}

# Bump pinned version of web app in R/zzz.R and NEWS.md ------------------------

zzz <- readLines("R/zzz.R")
lineWithPin <- which(startsWith(zzz, "versionAppPinned"))
message("Current pinned version of web app: ", zzz[lineWithPin])
zzz[lineWithPin] <- sprintf("versionAppPinned <- \"%s\"", appVersionNew)
message("New pinned version of web app: ", zzz[lineWithPin])
writeLines(zzz, "R/zzz.R")

news <- readLines("NEWS.md")
bullet <- sprintf("* The release tarball includes version %s of the web app",
                  appVersionNew)
news <- c(bullet, "", news)
# Exported below after adding version header

# Bump R package version in DESCRIPTION and NEWS.md ----------------------------

keep.white <- c("Description", "Authors@R", "Depends", "Imports", "Suggests")
description <- read.dcf("DESCRIPTION", keep.white = keep.white)

pkgVersionCurrent <- description[1, "Version"]
message("Current version of R package: ", pkgVersionCurrent)
pkgVersionCurrent <- as.package_version(pkgVersionCurrent)
# major.minor.patch.dev
pkgVersionComponents <- unclass(pkgVersionCurrent)[["Version"]]
# Bump the patch
pkgVersionComponents[3] <- pkgVersionComponents[3] + 1
# Drop the dev component
pkgVersionNew <- paste(pkgVersionComponents[1:3], collapse = ".")
message("New version of R package: ", pkgVersionNew)
description[1, "Version"] <- pkgVersionNew
write.dcf(description, file = "DESCRIPTION", keep.white = keep.white, indent = 0)

newsHeader <- paste("#", pkgVersionNew)
news <- c(newsHeader, "", news)
writeLines(news, "NEWS.md")

# Commit and tag ---------------------------------------------------------------

git <- c(
  "git add DESCRIPTION NEWS.md R/zzz.R",
  sprintf("git commit -m \"Bump to %s and pin version %s of the web app.\"",
          pkgVersionNew, appVersionNew),
  sprintf("git tag -a v%s -m \"v%s\"", pkgVersionNew, pkgVersionNew),
  "git push",
  "git push --tags"
)
gitScript <- "scripts/commit-tag-push.sh"
writeLines(git, gitScript)

message("\nRun the following Git commands:\n")
message(paste(git, collapse = "\n"))
message(sprintf("\nOr run: bash %s", gitScript))
