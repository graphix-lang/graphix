#!/bin/sh
# Title: a source file that starts with a UTF-8 byte order mark does not parse
# Command: sh x-formatter-bom-refused.sh [path-to-graphix]
# Expected: the BOM is skipped like other leading whitespace: `graphix fmt`
#   prints "let x = 1;\nx", and `graphix --check` succeeds.
# Observed: "Parse error at line: 1, column: 1 / Unexpected `\u{feff}`" (the
#   offending character is invisible in the message) from both fmt and --check.
#   Windows editors commonly write a BOM.
G=${1:-graphix}
D=$(mktemp -d)
printf '\357\273\277let x =  1;\nx\n' > "$D/bom.gx"
"$G" fmt --stdout "$D/bom.gx"; echo "fmt exit $?"
"$G" --check "$D/bom.gx"; echo "check exit $?"
rm -r "$D"
