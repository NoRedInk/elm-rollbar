#!/usr/bin/env sh

version_in_elm_package_json=$(cat elm-package.json | jq -M .version)
version_in_internal=$(cat src/Rollbar/Internal.elm | pcregrep -M "version =\n    $version_in_elm_package_json" | grep $version_in_elm_package_json | tr -d '[:space:]')


if [ "${version_in_elm_package_json}" != "${version_in_internal}" ]; then
    echo "The value specified in Rollbar.Internal.version is not the same as the package version in elm-package.json. Please update it before publishing!"
    exit 1
else
    echo "Rollbar.Internal.version matches version in elm-package.json"
    exit 0
fi
