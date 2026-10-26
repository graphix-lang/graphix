#!/bin/sh
# Title: interface files, trait bodies and impl bodies refuse end-of-line and below-list comments
# Command: sh t-parser-b-list-comments.sh
# Expected (CLAUDE.md "Comments": legal at the end of a list item's line after its separator
#   and below a list's last item or a file's): every case parses, as the same shapes do in a
#   block, a select or a .gx file.
# Observed: each case is a parse error ("Unexpected `/`").
G=${G:-/home/eric/tmp/review-2026-10-26/bin/graphix}
D=$(mktemp -d)
run() { echo "== $1"; timeout -s KILL 30 "$G" --check "$2" 2>&1 | grep -m1 -A1 "Parse error"; }
mkdir "$D/a" "$D/b"
printf 'let x = 1;\nlet y = 2\n' > "$D/a/m.gx"; cp "$D/a/m.gx" "$D/b/m.gx"
printf 'mod m;\nm::x + m::y\n' > "$D/a/main.gx"; cp "$D/a/main.gx" "$D/b/main.gx"
printf 'val x: i64;\nval y: i64\n// end of the interface\n' > "$D/a/m.gxi"
run "comment below a .gxi file's last item" "$D/a/main.gx"
printf 'val x: i64; // the x\nval y: i64\n' > "$D/b/m.gxi"
run "end-of-line comment after an interface item" "$D/b/main.gx"
printf 'trait Show {\n    val show: fn(self) -> string; // eol\n    val two: fn(self) -> string\n};\n1\n' > "$D/t1.gx"
run "end-of-line comment after a trait method" "$D/t1.gx"
printf 'trait Show {\n    val show: fn(self) -> string\n    // below\n};\n1\n' > "$D/t2.gx"
run "comment below a trait's last method" "$D/t2.gx"
printf 'trait Show { val show: fn(self) -> string; val two: fn(self) -> string };\nimpl Show for i64 {\n    let show = |x| "[x]"; // eol\n    let two = |x| "[x][x]"\n};\nShow::show(1)\n' > "$D/i1.gx"
run "end-of-line comment after an impl method" "$D/i1.gx"
printf 'trait Show { val show: fn(self) -> string };\nimpl Show for i64 {\n    let show = |x| "[x]"\n    // below\n};\nShow::show(1)\n' > "$D/i2.gx"
run "comment below an impl's last method" "$D/i2.gx"
printf '{\n    let show = |x| "[x]"; // eol\n    show(1)\n    // below\n}\n' > "$D/ok.gx"
echo "== control: the same comments in a block"; timeout -s KILL 30 "$G" --check "$D/ok.gx"; echo "rc=$?"
rm -rf "$D"
