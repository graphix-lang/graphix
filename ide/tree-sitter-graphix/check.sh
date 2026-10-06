#!/bin/bash
# Regenerate the parser, parse every tracked .gx/.gxi (and ../netidx's when
# it is checked out beside this repo), and compile each query against the
# grammar. Prints the files that do not parse cleanly and their count; the
# count should only go down. Run from anywhere.
set -euo pipefail
GRAMMAR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd -P)"
REPO="$(cd "$GRAMMAR/../.." && pwd -P)"
TS="$GRAMMAR/node_modules/.bin/tree-sitter"
[ -x "$TS" ] || { echo "run npm install in $GRAMMAR first" >&2; exit 2; }
(cd "$GRAMMAR" && "$TS" generate >/dev/null)
tmp="$(mktemp -d)"
trap 'rm -rf "$tmp"' EXIT
echo "{\"parser-directories\": [\"$(dirname "$GRAMMAR")\"]}" > "$tmp/config.json"
cd "$REPO"
{
    git ls-files '*.gx' '*.gxi'
    if [ -d ../netidx/.git ]; then
        (cd ../netidx && git ls-files '*.gx' '*.gxi' | sed 's|^|../netidx/|')
    fi
} > "$tmp/files"
xargs "$TS" parse --config-path "$tmp/config.json" --scope source.graphix --quiet \
    < "$tmp/files" 2>&1 | grep -E 'ERROR|MISSING' | awk '{print $1}' | sort || true
echo "files with ERROR/MISSING nodes: $(xargs "$TS" parse --config-path "$tmp/config.json" --scope source.graphix --quiet < "$tmp/files" 2>&1 | grep -cE 'ERROR|MISSING' || true) of $(wc -l < "$tmp/files")"
for q in "$GRAMMAR"/queries/*.scm; do
    "$TS" query --config-path "$tmp/config.json" --scope source.graphix "$q" \
        "$REPO/book/src/examples/tui/barchart_basic.gx" >/dev/null 2>&1 \
        || echo "query $(basename "$q") does not compile"
done
