#!/bin/sh
# Title: comments below the last statement of a file, block or seq that ends in `;`
#        are parsed onto the statement before the trailing empty one, and the
#        formatter (which reads them from the list's last item) refuses the file
# Command: sh design/review-2026-10-26/repro/t-parser-a-below-after-trailing-semi.sh
# Expected: each file formats, the comment kept below the last statement, as it is in
#           an array or select that ends in `,` (also run, both format)
# Observed: "formatter bug: the formatted text lost or moved a comment or an attribute"
#           for the file, the block and the seq; the array and select format
B=${B:-/home/eric/tmp/review-2026-10-26/bin/graphix}
D=$(mktemp -d)
printf 'let a = 1;\nlet b = 2;\n// the end\n' > $D/file.gx
printf 'let z = {\n    1;\n    2;\n    // below\n};\nz\n' > $D/block.gx
printf 'seq {\n    1;\n    2;\n    // below\n}\n' > $D/seq.gx
printf '[\n    1,\n    2,\n    // below\n]\n' > $D/array.gx
printf 'let x = 1;\nselect x {\n    1 => 2,\n    _ => 3,\n    // below\n}\n' > $D/select.gx
for f in file block seq array select; do
  timeout -s KILL 30 $B --check $D/$f.gx > /dev/null 2>&1; c=$?
  timeout -s KILL 30 $B fmt --stdout $D/$f.gx > $D/out 2>&1; r=$?
  echo "$f: check exit $c, fmt exit $r: $(grep -o 'formatter bug.*' $D/out)"
done
rm -rf $D
