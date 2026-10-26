#!/bin/bash
# Title: tree-sitter refuses an identifier holding a combining vowel sign,
#        which graphix accepts (Devanagari, Bengali, Tamil, ... identifiers)
# Command: bash design/review-2026-10-26/repro/ide-tooling-indic-identifier.sh
#   (needs ide/tree-sitter-graphix/node_modules: npm install there; GRAPHIX=<graphix binary>)
# Expected: both parse `let कि = 1; कि` (U+0915 + U+093F, a Mc mark that is
#   Alphabetic, so char::is_alphanumeric holds and graphix's ident() takes it)
# Observed: graphix fmt: ok; tree-sitter: (ERROR [0, 7] - [0, 10])
#   grammar.js `identifier` continues with [\p{L}\p{N}_..], which has no marks;
#   [\p{Alphabetic}\p{N}_..] parses it (checked by regenerating in a copy).
set -euo pipefail
REPO="$(cd "$(dirname "$0")/../../.." && pwd)"
G="$REPO/ide/tree-sitter-graphix"
GX="${GRAPHIX:-graphix}"
d=$(mktemp -d)
trap 'rm -rf "$d"' EXIT
export TREE_SITTER_LIBDIR="$d/lib"; mkdir -p "$TREE_SITTER_LIBDIR"
printf 'let कि = 1;\nकि\n' > "$d/u.gx"
echo "{\"parser-directories\": [\"$(dirname "$G")\"]}" > "$d/config.json"
"$GX" fmt --stdout "$d/u.gx" >/dev/null && echo "graphix fmt: ok"
"$G/node_modules/.bin/tree-sitter" parse --config-path "$d/config.json" \
    --scope source.graphix --quiet "$d/u.gx" || true
