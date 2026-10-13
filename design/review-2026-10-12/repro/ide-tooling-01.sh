#!/bin/bash
# ide-tooling-01: primitive types never reach (primitive_type), and an
# annotated let / lambda parameter loses its binding to type_ascription.
# Builds the checked-in grammar in a temp dir (needs `npm install` in
# ide/tree-sitter-graphix for node_modules/.bin/tree-sitter).
# Expected: 4+ `type.builtin` captures (i64, string, string, i64, f64) and
#   local.definition captures for x, f, a, m, c.
# Observed (HEAD 7324207f): zero type.builtin captures; definitions only
#   for f, m, c — x and a are missing.
set -euo pipefail
REPO="$(cd "$(dirname "${BASH_SOURCE[0]}")/../../.." && pwd -P)"
GR="$REPO/ide/tree-sitter-graphix"
TS="$GR/node_modules/.bin/tree-sitter"
tmp="$(mktemp -d)"; trap 'rm -rf "$tmp"' EXIT
mkdir -p "$tmp/g/tree-sitter-graphix"
cp -r "$GR/grammar.js" "$GR/package.json" "$GR/tree-sitter.json" "$GR/queries" "$GR/src" \
    "$tmp/g/tree-sitter-graphix/"
echo "{\"parser-directories\": [\"$tmp/g\"]}" > "$tmp/config.json"
SRC="$REPO/design/review-2026-10-12/repro/ide-tooling-01.gx"
export XDG_CACHE_HOME="$tmp/cache"
"$TS" parse --config-path "$tmp/config.json" --scope source.graphix "$SRC"
echo "--- type.builtin captures:"
"$TS" query --config-path "$tmp/config.json" --scope source.graphix \
    "$GR/queries/highlights.scm" "$SRC" | grep -E 'capture: [0-9]+ - type\.builtin' || echo "(none)"
echo "--- local.definition captures:"
"$TS" query --config-path "$tmp/config.json" --scope source.graphix \
    "$GR/queries/locals.scm" "$SRC" | grep 'local.definition' | sort -u
