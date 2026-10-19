#!/bin/bash
# ide-tooling: a script's `#!` first line, which graphix runs, checks and formats
# (parser::shebang_len / without_shebang), is an ERROR node in the tree-sitter grammar.
#
# command: bash design/review-2026-10-19/repro/ide-tooling-shebang.sh [graphix-binary]
# expected: tree-sitter parses the file with no ERROR node
# observed (HEAD 9445a1d21): graphix fmt prints the file; tree-sitter reports
#   (ERROR [0, 0] - [0, 1]) (`#` of the shebang)
set -uo pipefail
REPO="$(cd "$(dirname "$0")/../../.." && pwd)"
G="${1:-graphix}"
W="${TMPDIR:-/tmp}/ide-tooling-shebang.$$"
mkdir -p "$W/tree-sitter-graphix"
trap 'rm -rf "$W"' EXIT
cp -r "$REPO/ide/tree-sitter-graphix/"{grammar.js,src,queries,package.json,tree-sitter.json} "$W/tree-sitter-graphix/"
TS="$REPO/ide/tree-sitter-graphix/node_modules/.bin/tree-sitter"
export TREE_SITTER_LIBDIR="$W/lib"
echo "{\"parser-directories\": [\"$W\"]}" > "$W/config.json"
printf '#!/usr/bin/env graphix\nlet x = 1;\nx\n' > "$W/s.gx"
echo "== graphix fmt"
timeout -s KILL 30 "$G" fmt --stdout "$W/s.gx"
echo "== tree-sitter"
(cd "$W" && timeout -s KILL 60 "$TS" parse --config-path "$W/config.json" --scope source.graphix --quiet "$W/s.gx")
