#!/usr/bin/env bash

# Regenerate the `pkgs/default.nix` file based on the current
# contents of cabal and stack files, with a hackage snapshot.

set -euo pipefail
cd "$(dirname -- "$(readlink -f -- "${BASH_SOURCE[0]}")")"
exec "$(nix-build --no-out-link ../default.nix -A localLib.iohkNix.stack2nix.regeneratePackages --argstr hackageSnapshot 2018-11-08T09:58:14Z)" default.nix "$@"
