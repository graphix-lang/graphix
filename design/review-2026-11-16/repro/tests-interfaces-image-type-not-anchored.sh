#!/bin/sh
# Title: interface_type_anchored_after_destructuring pins nothing about the
#   destructuring anchor: a gxi-only `type` goes ahead of the whole body
#   (resolver::needs_no_value), never "after that let".
# Command: sh design/review-2026-11-16/repro/tests-interfaces-image-type-not-anchored.sh
# Expected (per the fixture's comment): T lands after `let (a, c)`, so a cast
#   to T above that let is refused "declare it before its first use".
# Observed: case 1 prints 3 (T is in force above the destructuring let); case 2
#   (a gxi-only `mod` after `val a`) is what depends on the destructuring
#   anchor: it prints 2, and with `a` declared below the reader (case 3, the
#   anchor missing) it is refused "l::z not defined".
G=${G:-/home/eric/tmp/review-2026-11-16/bin/graphix}
D=$(mktemp -d)
mkdir -p "$D/1" "$D/2/m" "$D/3/m"
cd "$D/1"
printf 'mod m;\nm::a + m::c + m::b\n' > main.gx
printf 'val a: i64;\ntype T = i64;\nval b: T;\nval c: i64\n' > m.gxi
printf 'let b: T = cast<T>(1)$;\nlet (a, c) = (1, 1)\n' > m.gx
echo "== 1 cast to the gxi type above the destructuring let"; timeout -s KILL 8 "$G" --no-cache main.gx
for n in 2 3; do
  cd "$D/$n"
  printf 'mod m;\nm::w\n' > main.gx
  printf 'val a: i64;\nmod l;\nval w: i64;\nval c: i64\n' > m.gxi
  printf 'use super::a;\nlet z = a\n' > m/l.gx
done
printf 'let (a, c) = (1, 1);\nlet w = l::z + 1\n' > "$D/2/m.gx"
printf 'let c = 1;\nlet w = l::z + 1;\nlet a = 1\n' > "$D/3/m.gx"
cd "$D/2"; echo "== 2 gxi-only mod after a val bound by destructuring"; timeout -s KILL 8 "$G" --no-cache main.gx
cd "$D/3"; echo "== 3 the same with the val declared below its reader"; timeout -s KILL 8 "$G" --check main.gx 2>&1 | tail -1
rm -rf "$D"
