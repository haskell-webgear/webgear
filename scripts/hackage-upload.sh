#!/usr/bin/env bash

# This script uploads packages to hackage

set -euo pipefail

HACKAGE_API_KEY=$1

if [[ "$HACKAGE_API_KEY" == "" ]]; then
    echo "Usage: $0 <hackage-api-key>"
    exit 1
fi

upload_package() {
    local package=$(ls dist-newstyle/sdist/$1-*.tar.gz)
    local doc=$(ls dist-newstyle/haddock/$1-*.tar.gz)
    curl --verbose \
         --header "Accept: text/plain" \
         --header "Authorization: X-ApiKey $HACKAGE_API_KEY" \
         --form "package=@$package" \
         https://hackage.haskell.org/packages/
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
