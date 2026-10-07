#!/usr/bin/env bash
# The collection corpus under bench/run.sh: bench/collection/run.sh
# [iterations] [graphix-binary].
here="$(cd "$(dirname "$0")" && pwd)"
exec "$here/../run.sh" "${1:-3}" "${2:-${GRAPHIX:-}}" "$here"
