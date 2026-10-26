#!/bin/sh
# Title: comments are refused in several lists the formatter lays out one
#        item per line: above a labeled call argument, on a lambda formal,
#        on a map literal entry, inside a `use` tree
# Command: sh design/review-2026-10-26/repro/t-format-resolver-comment-gaps.sh
# Expected (CLAUDE.md "Comments": legal above an expression and a field, at
#   the end of a list item's line after its separator -- an element, an
#   argument -- and below a list's last item): each input parses and formats.
# Observed: every one is a parse error, while the same placement in an
#   array, a positional argument list or a struct literal formats.
G=${G:-/home/eric/tmp/review-2026-10-26/bin/graphix}
d=$(mktemp -d)
printf 'f(\n    // the width\n    #width: 1,\n    2\n)\n' > $d/labeled_arg.gx
printf 'let f = |\n    // the width\n    #width: i64 = 1,\n    x: i64\n| x + width;\nf(2)\n' > $d/lambda_formal.gx
printf 'let f = |#width: i64 = 1, // the width\n    x: i64| x + width;\nf(2)\n' > $d/lambda_formal_eol.gx
printf 'let m = {\n    "a" => 1, // first\n    "b" => 2\n};\nm\n' > $d/map_eol.gx
printf 'use array::{\n    // length\n    len,\n    map\n};\nlen([1])\n' > $d/use_tree.gx
for f in labeled_arg lambda_formal lambda_formal_eol map_eol use_tree; do
  printf '== %s: ' $f; $G fmt --stdout $d/$f.gx >/dev/null 2>$d/err; echo "rc=$?"; head -2 $d/err
done
rm -rf $d
