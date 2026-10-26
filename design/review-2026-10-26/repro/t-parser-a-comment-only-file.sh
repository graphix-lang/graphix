#!/bin/sh
# Title: comments with no item before them in a statement list are refused: a .gx file
#        (or module) holding only comments, and the comments below a block's single
#        statement and its trailing `;`
# Command: sh design/review-2026-10-26/repro/t-parser-a-comment-only-file.sh
# Expected: all check (exit 0), like the empty file and like `{ 1; 2;\n// c\n}`
# Observed: "Parse error ... a comment here has no item to describe" for the
#           comment-only script, the module whose m.gx is one comment line, and
#           `{ 1;\n    // below\n}`
B=${B:-/home/eric/tmp/review-2026-10-26/bin/graphix}
D=$(mktemp -d)
printf '' > $D/empty.gx
printf '// just a comment\n' > $D/only.gx
mkdir $D/m; printf 'mod m;\n1\n' > $D/m/main.gx; printf '// todo\n' > $D/m/m.gx
printf 'let y = {\n    1;\n    // below\n};\ny\n' > $D/block1.gx
printf 'let y = {\n    1;\n    2;\n    // below\n};\ny\n' > $D/block2.gx
for f in empty.gx only.gx m/main.gx block1.gx block2.gx; do
  timeout -s KILL 30 $B --check $D/$f > $D/out 2>&1; echo "$f exit $?: $(grep -o 'a comment here has no item to describe' $D/out)"
done
rm -rf $D
