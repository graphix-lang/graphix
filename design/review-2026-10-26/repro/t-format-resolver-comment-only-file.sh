#!/bin/sh
# Title: a .gx or .gxi file holding only comments does not parse, and a .gxi
#        takes no comment at the end of an item's line or below its last item
# Command: sh design/review-2026-10-26/repro/t-format-resolver-comment-only-file.sh
# Expected (CLAUDE.md "Comments": legal below a list's last item or a file's;
#   a .gx ending in a comment formats): each input parses (an empty file does).
# Observed: parse errors ("a comment here has no item to describe" for the
#   .gx; "Unexpected end of input" / "Unexpected `/`" for the .gxi).
G=${G:-/home/eric/tmp/review-2026-10-26/bin/graphix}
d=$(mktemp -d)
printf '// a module still to be written\n' > $d/stub.gx
printf '// license header\n' > $d/stub.gxi
printf 'val x: i64; // the x\nval y: i64\n' > $d/eol.gxi
printf 'val x: i64\n// more to come\n' > $d/below.gxi
for f in stub.gx stub.gxi eol.gxi below.gxi; do
  printf '== %s: ' $f; $G fmt --stdout $d/$f >/dev/null 2>$d/err; echo "rc=$?"; head -3 $d/err | tail -2
done
rm -rf $d
