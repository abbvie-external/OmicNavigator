#!/bin/bash
set -u

# Automate local build steps for next release.
#
# Usage: bash scripts/release.sh

echo "==== Bundle app"
bash scripts/bundleApp.sh

echo "==== Clean up"
rm -f *tar.gz *pdf
make --silent clean
make --silent diagrams

echo "==== Build & install (no vignettes or manual)"
R CMD build --no-build-vignettes --no-manual . > /dev/null 2>&1
R CMD INSTALL --no-multiarch --with-keep.source OmicNavigator_*.tar.gz 2> /dev/null

tarball=`ls OmicNavigator_*tar.gz`
version="$tarball"
version=${version/OmicNavigator_/}
version=${version%.tar.gz}
echo "Installed version $version"

echo "==== Test app interactively"
Rscript -e 'OmicNavigator::startApp()'

read -p "Continue? (yes/no) " continue

if [ "$continue" != "yes" ]
then
  echo "==== Exit early"
  exit 1
fi

echo "==== Build & install"
rm -f *tar.gz
R CMD build . > /dev/null 2>&1
R CMD INSTALL --no-multiarch --with-keep.source OmicNavigator_*.tar.gz 2> /dev/null

echo "==== Create vignettes to upload as release assets"
make --silent vignettes > /dev/null 2>&1
cp vignettes/OmicNavigatorAPI.pdf OmicNavigatorAPI_${version}.pdf
cp vignettes/OmicNavigatorUsersGuide.pdf OmicNavigatorUsersGuide_${version}.pdf
if [ `uname -r | grep Microsoft` ]
then
  explorer.exe .
fi
