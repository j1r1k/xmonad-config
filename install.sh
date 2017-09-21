#!/bin/sh
set -o errexit -o nounset -o pipefail

(
    cd "$(dirname "$0")"

    stack install --local-bin-path="${HOME}/.local/bin/"
    stack install --local-bin-path="${HOME}/.local/bin/" xmobar
)
