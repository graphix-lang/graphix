#!/bin/sh
# x-formatter: a comment below a statement list whose last statement ends in `;` is refused
# Command: sh design/review-2026-10-26/repro/x-formatter-below-after-trailing-semi.sh
# Expected: each input formats (the comment printed below the last statement, after its `;`).
# Observed: "formatter bug: the formatted text lost or moved a comment or an attribute", exit 1,
#   for the file, block and seq cases; the comma-list case (array) formats fine.
G=${G:-/home/eric/tmp/review-2026-10-26/bin/graphix}
d=$(mktemp -d)
printf 'let a = 1;\nlet b = 2;\n// more to come\n' > $d/file.gx
printf 'let x = { let a = 1; a;\n// c\n};\nx\n' > $d/block.gx
printf 'seq { let a = 1; a;\n// c\n}\n' > $d/seq.gx
printf 'let a = [1, 2,\n// c\n];\na\n' > $d/array.gx
for f in file block seq array; do
  echo "== $f"; timeout -s KILL 30 $G fmt --stdout $d/$f.gx; echo "exit $?"
done
rm -r $d
