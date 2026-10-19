#!/bin/bash
# ide-tooling: the tree-sitter grammar refuses a raw (r"..") or template ("""..""")
# string as a select pattern, which the parser accepts (patternexp.rs string_pattern).
#
# command: bash design/review-2026-10-19/repro/ide-tooling-pattern-raw-template-string.sh [graphix-binary]
# expected: both parsers accept the program (graphix fmt prints it; tree-sitter
#   reports no ERROR node)
# observed (HEAD 9445a1d21): graphix fmt prints the program; tree-sitter parse reports
#   (ERROR [1, 11] - [1, 12]) for the raw pattern and (ERROR [1, 13] - [1, 18]) for
#   the template one.
set -uo pipefail
REPO="$(cd "$(dirname "$0")/../../.." && pwd)"
G="${1:-graphix}"
W="${TMPDIR:-/tmp}/ide-tooling-pat.$$"
mkdir -p "$W/tree-sitter-graphix"
trap 'rm -rf "$W"' EXIT
cp -r "$REPO/ide/tree-sitter-graphix/"{grammar.js,src,queries,package.json,tree-sitter.json} "$W/tree-sitter-graphix/"
TS="$REPO/ide/tree-sitter-graphix/node_modules/.bin/tree-sitter"
export TREE_SITTER_LIBDIR="$W/lib"
echo "{\"parser-directories\": [\"$W\"]}" > "$W/config.json"
printf 'let s = "a";\nselect s { r"a" => 1, _ => 0 }\n' > "$W/raw.gx"
printf 'let s = "a";\nselect s { """a""" => 1, _ => 0 }\n' > "$W/tmpl.gx"
for f in raw tmpl; do
    echo "== $f: graphix fmt"
    timeout -s KILL 30 "$G" fmt --stdout "$W/$f.gx"
    echo "== $f: tree-sitter"
    (cd "$W" && timeout -s KILL 60 "$TS" parse --config-path "$W/config.json" --scope source.graphix --quiet "$W/$f.gx")
done
