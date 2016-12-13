#!/bin/sh
set -o errexit -o nounset -o pipefail

stack install --local-bin-path="${HOME}/.local/bin/"
