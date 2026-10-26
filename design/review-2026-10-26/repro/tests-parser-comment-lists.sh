#!/bin/sh
# Title: impl/trait method lists and interface files refuse the end-of-line
#        and below-the-last-item comments every other list accepts.
# Command: sh tests-parser-comment-lists.sh   (G = the graphix binary)
# Expected (CLAUDE.md "Comments": legal at the end of a list item's line
#   after its separator and below a list's last item or a file's): each case
#   formats like the block/array controls do.
# Observed: the controls format; every impl/trait/.gxi case is a parse error,
#   most with no reason note ("Unexpected `/`", "Unexpected `}`").
G=${G:-/home/eric/tmp/review-2026-10-26/bin/graphix}
D=$(mktemp -d)
run() { printf '%s' "$2" > "$D/p.$1"; echo "=== $3"; timeout -s KILL 30 "$G" fmt --stdout "$D/p.$1" 2>&1 | grep -v '^Expected' | head -4; }
run gx '{ let y = 1; // c
y
// below
}' 'control: block eol + below (accepted)'
run gx 'impl Show for C { let show = |c| "c"
// below
}' 'impl: below the last method'
run gx 'impl Show for C { let show = |c| "c";
// below
}' 'impl: below the last method after its ;'
run gx 'impl Show for C { let show = |c| "c"; // eol
let s = |c| "d" }' 'impl: eol after a method'
run gx 'trait T {
  val show: fn(self) -> string
  // below
}' 'trait: below the last method'
run gxi 'val x: i64; // eol
val y: i64' 'interface: eol after an item'
run gxi 'val x: i64;
val y: i64
// below
' 'interface: below the file'"'"'s last item'
run gx 'let x = 1;
x
// below
' 'control: below a .gx file'"'"'s last item (accepted)'
rm -rf "$D"
