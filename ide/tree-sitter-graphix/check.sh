#!/bin/bash
# Regenerate the parser, parse every tracked .gx/.gxi (and ../netidx's when
# it is checked out beside this repo), and compile each query against the
# grammar. A fuzz finding that bundles several files (`// file-v1: <name>`
# sections) is parsed section by section. A file tree-sitter refuses is a
# grammar bug only when Graphix parses it, which `graphix fmt` decides
# (`$GRAPHIX`, else `graphix` on PATH); those are listed and counted, and
# the count should be zero. Run from anywhere.
set -euo pipefail
GRAMMAR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd -P)"
REPO="$(cd "$GRAMMAR/../.." && pwd -P)"
TS="$GRAMMAR/node_modules/.bin/tree-sitter"
[ -x "$TS" ] || { echo "run npm install in $GRAMMAR first" >&2; exit 2; }
GX="${GRAPHIX:-$(command -v graphix || true)}"
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
} | while read -r f; do
    if grep -q '^// file-v1: ' "$f"; then
        # the main program, then each named file, under a directory
        # named for the bundle so a failure names it
        d="$tmp/bundles/$f"
        mkdir -p "$d"
        awk -v d="$d" 'BEGIN { out = d "/main.gx" }
            /^\/\/ file-v1: / { out = d "/" $3; next }
            { print > out }' "$f"
        find "$d" -type f
    else
        echo "$f"
    fi
done > "$tmp/files"
xargs "$TS" parse --config-path "$tmp/config.json" --scope source.graphix --quiet \
    < "$tmp/files" 2>&1 | grep -E 'ERROR|MISSING' | awk '{print $1}' | sort -u \
    > "$tmp/refused" || true
if [ -z "$GX" ]; then
    echo "no graphix binary (set GRAPHIX): files tree-sitter refuses:"
    sed "s|^$tmp/bundles/||" "$tmp/refused"
    echo "tree-sitter refuses $(wc -l < "$tmp/refused") of $(wc -l < "$tmp/files")"
else
    agreed=0
    disagreed=0
    while read -r f; do
        if "$GX" fmt --stdout "$f" >/dev/null 2>&1; then
            echo "graphix parses, tree-sitter refuses: ${f#"$tmp/bundles/"}"
            disagreed=$((disagreed + 1))
        else
            agreed=$((agreed + 1))
        fi
    done < "$tmp/refused"
    echo "tree-sitter refuses $disagreed file(s) graphix parses, of $(wc -l < "$tmp/files")" \
        "($agreed neither parses)"
fi
for q in "$GRAMMAR"/queries/*.scm; do
    "$TS" query --config-path "$tmp/config.json" --scope source.graphix "$q" \
        "$REPO/book/src/examples/tui/barchart_basic.gx" >/dev/null 2>&1 \
        || echo "query $(basename "$q") does not compile"
done
