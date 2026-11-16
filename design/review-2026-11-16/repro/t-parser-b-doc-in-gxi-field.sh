#!/bin/sh
# Title: in a .gxi, a `///` doc above a type definition's struct field (or a
#   use-group item) is refused with the
#   note "`///` is a doc comment, legal only in a .gxi interface file; a .gx
#   file comments with `//`" -- said of a .gxi file. comment_line(i, false)
#   calls note_doc_in_gx wherever docs are not taken, which assumes the file
#   is a .gx.
# Command: sh design/review-2026-11-16/repro/t-parser-b-doc-in-gxi-field.sh
# Expected: a note that says where a doc may stand (above an interface item
#   or a trait method), not that the file is the wrong kind.
# Observed: both print the ".gxi interface file" note against a .gxi.
G=${G:-/home/eric/tmp/review-2026-11-16/bin/graphix}
D=$(mktemp -d)
printf 'type T = {\n    /// the a field\n    a: i64\n};\nval f: fn(x: T) -> i64;\n' > $D/field.gxi
printf 'use array::{\n    /// map it\n    map\n};\nval f: fn(x: i64) -> i64;\n' > $D/use.gxi
for f in field use; do
    echo "== $f.gxi"
    timeout -s KILL 30 $G fmt --stdout $D/$f.gxi 2>&1 | grep -n "note" 
done
rm -rf $D
