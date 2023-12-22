#!/usr/bin/env bash

# This script uploads packages to hackage

set -euo pipefail

HACKAGE_API_KEY=$1

if [[ "$HACKAGE_API_KEY" == "" ]]; then
    echo "Usage: $0 <hackage-api-key>"
    exit 1
fi

upload_package() {
    local archive=$1
    curl --silent \
         --header "Accept: text/plain" \
         --header "Authorization: X-ApiKey $HACKAGE_API_KEY" \
         --form "package=@$archive" \
         https://hackage.haskell.org/packages/
}

cabal sdist all

upload_package dist-newstyle/sdist/webgear-core-*.tar.gz
upload_package dist-newstyle/sdist/webgear-server-*.tar.gz
upload_package dist-newstyle/sdist/webgear-swagger-*.tar.gz
upload_package dist-newstyle/sdist/webgear-swagger-ui-*.tar.gz
upload_package dist-newstyle/sdist/webgear-openapi-*.tar.gz
