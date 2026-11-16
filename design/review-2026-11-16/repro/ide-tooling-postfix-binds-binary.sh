#!/bin/bash
# Title: tree-sitter reads `k ~ x$ + 1` as `((k ~ x)$) + 1`; Graphix reads `k ~ ((x$) + 1)`
#
# Command: bash design/review-2026-11-16/repro/ide-tooling-postfix-binds-binary.sh
#   (works on a copy of ide/tree-sitter-graphix in a temp dir; needs
#   `npm install` already run in ide/tree-sitter-graphix for its CLI)
#
# Expected: the `$` (and `?`) applies to `x` alone, as the parser's
#   postfix chain does (arithexp.rs: arith_term), e.g.
#   (binary_expression R (binary_expression (or_never R) N))
# Observed (HEAD e1be2fbf2):
#   k ~ x$ + 1  -> (binary_expression (or_never (binary_expression R R)) N)
#   a + b? * c  -> (binary_expression (qop (binary_expression R R)) R)
#   a && b$ == c -> (binary_expression (or_never (binary_expression R R)) R)
#   while `graphix fmt` prints `k ~ x$ + 1` back unchanged (no parens), so
#   the program's tree is the other one. Cause: grammar.js `qop` and
#   `or_never` take `choice($._primary_expression, $._arithmetic_expression)`.
#   With `$._primary_expression` alone the same corpus parses with the same
#   refusals and these shapes come out right.
set -euo pipefail
REPO="$(cd "$(dirname "$0")/../../.." && pwd)"
G="$REPO/ide/tree-sitter-graphix"
T="$(mktemp -d)"
trap 'rm -rf "$T"' EXIT
mkdir -p "$T/grammars/tree-sitter-graphix" "$T/lib"
cp -r "$G/grammar.js" "$G/tree-sitter.json" "$G/package.json" "$G/src" "$G/queries" \
    "$T/grammars/tree-sitter-graphix/"
echo "{\"parser-directories\": [\"$T/grammars\"]}" > "$T/config.json"
export TREE_SITTER_LIBDIR="$T/lib"
for e in 'k ~ x$ + 1' 'a + b? * c' 'a && b$ == c'; do
    printf '%s\n' "$e" > "$T/s.gx"
    printf '%-14s ' "$e"
    "$G/node_modules/.bin/tree-sitter" parse --config-path "$T/config.json" \
        --scope source.graphix "$T/s.gx" \
        | sed 's/ \[[0-9]*, [0-9]*\] - \[[0-9]*, [0-9]*\]//g' | tr -s ' \n' ' ' \
        | sed 's/(reference (module_path (identifier)))/R/g; s/(literal (number))/N/g'
    echo
done
