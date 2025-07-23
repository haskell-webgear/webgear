#!/usr/bin/env sh

#
# Script that updates all packages for a new version
#

set -eu

if [ "$#" -ne "2" ]; then
    echo "Usage: ./prepare-release.sh <old version> <new version>"
    exit 1
fi

old_version=$1
new_version=$2

year=$(date +%Y)
today=$(date +%Y-%m-%d)

find . -name '*.cabal' -exec sed -E -i \
    -e "s/(version:[[:space:]]+)${old_version}/\1${new_version}/" \
    -e "s/(, webgear-core (\^>=|==))${old_version}/\1${new_version}/" \
    -e "s/(, webgear-server (\^>=|==))${old_version}/\1${new_version}/" \
    -e "s/(copyright:[[:space:]]+[[:digit:]]{4})-[[:digit:]]{4}/\1-${year}/" \
    {} \;

find . -name CHANGELOG.md -exec sed -E -i \
    -e "s/^(## \[Unreleased\])$/\1\n\n## [${new_version}] - ${today}/" \
    -e "s/^(\[Unreleased\]: (https:\/\/github.com\/haskell-webgear\/webgear)\/compare\/v)${old_version}...HEAD$/\1${new_version}...HEAD\n[${new_version}]: \2\/releases\/tag\/v${new_version}/" \
    {} \;

cabal build all
cabal run webgear-benchmarks:benchmarks -- --output ./webgear-benchmarks/criterion-report.html
