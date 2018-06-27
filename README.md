# elm-rollbar ![Build Status](https://travis-ci.org/NoRedInk/elm-rollbar.svg?branch=master)

Send reports to Rollbar.

### Development

- Before publishing changes you must make sure that the value of `Rollbar.Internal.version` matches the `version` in elm-package.json. The script `scripts/verify-notifier-version.sh` will check this for you and also fail the Travis build.
