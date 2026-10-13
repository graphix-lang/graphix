#!/bin/bash
# shell-10: `graphix fmt --check --stdout` ignores --check: it prints the
# formatted text and exits 0 on an unformatted file.
# Usage: shell-10-fmt-check-stdout.sh /path/to/graphix
# expected: exit 1 (or a refusal of the flag pair); observed: exit 0
G=${1:-graphix}
D=$(mktemp -d)
printf 'let x   =  1;\nx\n' > $D/uf.gx
$G fmt --check --stdout $D/uf.gx; echo "--check --stdout exit $?"
$G fmt --check $D/uf.gx; echo "--check exit $?"
rm -rf "$D"
