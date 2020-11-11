#!/bin/bash
set -e

# Convenience script for bundling a new version of the app into the package.
# Must be run from the root of the project directory.
#
# Usage: bash scripts/bundleApp.sh </path/to/build.zip>

# Path to the downloaded app

input="$1"
if [ -z "$input" ]
then
  input="/mnt/c/Users/***REMOVED***/build.zip"
fi
appPath=`dirname "$input"`
appName=`basename "${input%.zip}"`

echo "Bundling the file $appPath/$appName.zip"

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
  echo "Removed previous zipball"
fi

if [ -d  "$appName"/ ]
then
  rm -r  "$appName"/
  echo "Removed previous build directory"
fi

if [ -d inst/www/ ]
then
  rm -r inst/www/
  echo "Removed previous inst/www/"
fi

# Copy zipball and extract

cp "$appPath/$appName.zip" .

unzip -q "$appName.zip"

mkdir inst/www/

cp -R "$appName"/* inst/www/

echo "Copied zipball and extracted to inst/www/"

# Clean up temporary files

rm "$appName.zip"
rm -r "$appName"/

# Report app version

grep -o -E "appVersion:[[:space:]]'[[:digit:]]+\.[[:digit:]]+\.[[:digit:]]+[\.]*[[:digit:]]*'" inst/www/static/js/main.*.chunk.js.map
