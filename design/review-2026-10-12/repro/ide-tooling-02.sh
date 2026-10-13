#!/bin/bash
# ide-tooling-02: programs the compiler's parser accepts that the
# tree-sitter grammar refuses (ERROR nodes), each on its own.
# Expected: every snippet "ts: clean".
# Observed (HEAD 7324207f): every snippet "gx: parses" and "ts: ERROR".
#   1. raw / triple-quoted string literals as select patterns
#      (structure_pattern has no raw_string / triple_string)
#   2. a leading `#!` line (RootFile::load strips it; the grammar has no
#      shebang rule)
#   3. an attribute with an empty argument list (grammar: commaSep1)
#   4. a seq / select whose body `{` touches the head (map_ref's
#      token.immediate('{') wins the lexer; the combine parser backtracks)
# Usage: ide-tooling-02.sh [graphix-binary]
set -uo pipefail
GX="${1:-graphix}"
REPO="$(cd "$(dirname "${BASH_SOURCE[0]}")/../../.." && pwd -P)"
GR="$REPO/ide/tree-sitter-graphix"
TS="$GR/node_modules/.bin/tree-sitter"
tmp="$(mktemp -d)"; trap 'rm -rf "$tmp"' EXIT
mkdir -p "$tmp/g/tree-sitter-graphix"
cp -r "$GR/grammar.js" "$GR/package.json" "$GR/tree-sitter.json" "$GR/queries" "$GR/src" \
    "$tmp/g/tree-sitter-graphix/"
echo "{\"parser-directories\": [\"$tmp/g\"]}" > "$tmp/config.json"
export XDG_CACHE_HOME="$tmp/cache"
i=0
while IFS= read -r snippet; do
    i=$((i + 1)); f="$tmp/s$i.gx"
    printf '%b\n' "$snippet" > "$f"
    "$GX" fmt --stdout "$f" >/dev/null 2>&1 && gx="parses" || gx="REFUSED"
    out="$("$TS" parse --config-path "$tmp/config.json" --scope source.graphix --quiet "$f" 2>&1)"
    if grep -qE 'ERROR|MISSING' <<<"$out"; then ts="ERROR"; else ts="clean"; fi
    printf 'gx: %-8s ts: %-6s %s\n' "$gx" "$ts" "$snippet"
done <<'SNIPPETS'
let s = "a"; select s { r"C:\\x" => 1, _ => 0 }
let s = "a"; select s { """a "q" [b]""" => 1, _ => 0 }
#!/usr/bin/env graphix\nlet x = 1; x
#[parallel()]\nlet f = 1; f
let t = 1; seq t{ t }
let x = 1; select x{ _ => 1 }
SNIPPETS
