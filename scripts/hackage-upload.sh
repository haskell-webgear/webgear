#!/usr/bin/env bash

# This script uploads packages to hackage

set -euo pipefail

HACKAGE_API_KEY=$1

if [[ "$HACKAGE_API_KEY" == "" ]]; then
    echo "Usage: $0 <hackage-api-key>"
    exit 1
fi

upload_package() {
    local package=$(find dist-newstyle/sdist -maxdepth 1 -regex "dist-newstyle/sdist/$1-[0-9.]+\.tar\.gz")
    local doc=$(find dist-newstyle/haddock -maxdepth 1 -regex "dist-newstyle/haddock/webgear-swagger-[0-9.]+-docs\.tar\.gz")
    echo "Uploading package $package"
    curl --verbose \
         --header "Accept: text/plain" \
         --header "Authorization: X-ApiKey $HACKAGE_API_KEY" \
         --form "package=@$package" \
         https://hackage.haskell.org/packages/
    sleep 10
    echo "Uploading doc $doc"
    curl --verbose \
         --request PUT \
         --header "Accept: text/plain" \
         --header "Authorization: X-ApiKey $HACKAGE_API_KEY" \
         --data-binary "@$doc" \
         https://hackage.haskell.org/package/$1/docs
}

cabal sdist all
cabal haddock --haddock-for-hackage --enable-doc --builddir=dist-newstyle/haddock all

upload_package webgear-core
upload_package webgear-server
upload_package webgear-swagger
upload_package webgear-swagger-ui
upload_package webgear-openapi
