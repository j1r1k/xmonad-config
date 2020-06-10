#!/bin/sh
set -o errexit -o nounset -o pipefail

mkdir -p "${HOME}/.xmonad"

mkpipe() {
    PIPE="${HOME}/.xmonad/xmobar-pipe-${1}"
    if [ ! -p "${PIPE}" ]; then
        rm -f "$PIPE"
        mkfifo "$PIPE"
    fi
}

mkpipe bluetooth
mkpipe audio

(
    cd "$(dirname "$0")"

    stack install --local-bin-path="${HOME}/.local/bin/"
    stack install --local-bin-path="${HOME}/.local/bin/" xmobar
)
