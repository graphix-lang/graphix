#!/bin/sh
# x-formatter: lists outside `commented` take no end-of-line or below comment
# Command: sh design/review-2026-10-26/repro/x-formatter-list-comments-missing.sh
# Expected (CLAUDE.md "Comments": legal at the end of a list item's line after its
#   separator and below a list's last item or a file's): every case parses and formats.
# Observed: each is a parse error ("a comment stands on a line of its own" / "a comment
#   here has no item to describe" / "Unexpected `/`"); the same comment in a block,
#   array, call or select parses.
G=${G:-/home/eric/tmp/review-2026-10-26/bin/graphix}
d=$(mktemp -d)
printf 'val x: i64;\n// more to come\n' > $d/trailing.gxi
printf 'val x: i64; // the x\nval y: i64\n' > $d/eol.gxi
printf 'trait T {\n    val m: fn(self) -> i64\n    // c\n};\n0\n' > $d/trait_below.gx
printf 'type C = Abstract<i64>;\ntrait T { val m: fn(self) -> i64 };\nimpl T for C {\n    let m = |c| c.0; // eol\n};\nT::m(C(1))\n' > $d/impl_eol.gx
printf 'let t = 1;\nlet x = never(t, // e\n1);\nx\n' > $d/never_args.gx
printf 'let a = {"k" => 1, // c\n"j" => 2};\na\n' > $d/map_entry.gx
printf '// nothing here yet\n' > $d/comment_only.gx
for f in trailing.gxi eol.gxi trait_below.gx impl_eol.gx never_args.gx map_entry.gx comment_only.gx; do
  echo "== $f"; timeout -s KILL 30 $G fmt --stdout $d/$f 2>&1 | grep -E 'Parse error|note:|^[^ ]' | head -3
done
rm -r $d
