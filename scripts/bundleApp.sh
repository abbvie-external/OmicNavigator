#!/bin/bash
set -eux

# Convenience script for bundling a new version of the app into the package.
# Must be run from the root of the project directory.

# Set this to the path to the downloaded app
appPath="/mnt/c/Users/***REMOVED***"
appName="build"

# Check configuration

if [ ! -f DESCRIPTION ]
then
  echo "bundleApp.sh must be executed in the root directory of the package"
  exit 1
fi

if [ ! -f "$appPath/$appName.zip" ]
then
  echo "This file does not exist: $appPath/$appName.zip"
  exit 1
fi

# Clean up beforehand if necessary

if [ -f  "$appName.zip" ]
then
  rm  "$appName.zip"
fi

if [ -d  "$appName"/ ]
then
  rm -r  "$appName"/
fi

if [ -d inst/www/ ]
then
  rm -r inst/www/
fi

cp "$appPath/$appName.zip" .

unzip "$appName.zip"

mkdir inst/www/

cp -R "$appName"/* inst/www/

rm "$appName.zip"
rm -r "$appName"/
