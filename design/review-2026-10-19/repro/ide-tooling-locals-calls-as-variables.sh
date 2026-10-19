#!/bin/bash
# ide-tooling: locals.scm makes every call of a let-bound function, and the last
# segment of a qualified path whose name a local also binds, highlight as a variable.
#
# command: bash design/review-2026-10-19/repro/ide-tooling-locals-calls-as-variables.sh
#   (copies the grammar to a scratch dir and runs `tree-sitter highlight`, whose
#   locals handling is the one Helix ports: a @local.reference resolved to a
#   @local.definition takes the definition's highlight)
# expected: `f` in `f(y)` and `map` in `array::map(..)` are @function (#222222)
# observed (HEAD 9445a1d21): both are @variable (#111111), the color of the
#   `let f` / `let map` definitions:
#   ... <span style='color: #333333'>array</span> <span style='color: #111111'>map</span>
#   ... <span style='color: #111111'>f</span> <span style='color: #111111'>y</span>
set -euo pipefail
REPO="$(cd "$(dirname "$0")/../../.." && pwd)"
W="${TMPDIR:-/tmp}/ide-tooling-locals.$$"
mkdir -p "$W"
trap 'rm -rf "$W"' EXIT
mkdir -p "$W/tree-sitter-graphix"
cp -r "$REPO/ide/tree-sitter-graphix/"{grammar.js,src,queries,package.json,tree-sitter.json} "$W/tree-sitter-graphix/"
TS="$REPO/ide/tree-sitter-graphix/node_modules/.bin/tree-sitter"
export TREE_SITTER_LIBDIR="$W/lib"
cat > "$W/config.json" <<EOF
{"parser-directories": ["$W"], "theme": {"variable": "#111111", "function": "#222222", "namespace": "#333333"}}
EOF
printf 'let map = 1;\nlet f = |x| x;\nlet y = array::map([map], f);\nf(y)\n' > "$W/p.gx"
cd "$W"
timeout -s KILL 60 "$TS" highlight --config-path "$W/config.json" --scope source.graphix --html "$W/p.gx" \
    | grep -o "<span[^>]*>[^<]*</span>" | tr '\n' ' '
echo
