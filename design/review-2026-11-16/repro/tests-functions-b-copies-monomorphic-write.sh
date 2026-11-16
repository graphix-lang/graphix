#!/bin/sh
# Title: generic_function_copies_refuse_a_monomorphic_write passes because the
#   first call decides the array's element type, not because the copies'
#   quantifier is held rigid against the write
# Command: sh tests-functions-b-copies-monomorphic-write.sh [path/to/graphix]
# Expected (the fixture's comment): the quantifier stays open after
#   `fs[0]$(u8:1, u8:2)`, so a monomorphic write is refused whatever its type.
# Observed: the refusal says fs is already `Array<fn(x: u8, y: u8) -> u8>`, and
#   the same write at u8 (a monomorphic function) checks.
G=${1:-/home/eric/tmp/review-2026-11-16/bin/graphix}
D=$(mktemp -d)
cat > $D/i64_write.gx <<'GX'
let g = 'q: Number |x, y: 'q| -> 'q x + y; let fs = [g, g]; let a = fs[0]$(u8:1, u8:2); fs <- [|x: i64, y: i64| x * y]; let result = a
GX
cat > $D/u8_write.gx <<'GX'
let g = 'q: Number |x, y: 'q| -> 'q x + y; let fs = [g, g]; let a = fs[0]$(u8:1, u8:2); fs <- [|x: u8, y: u8| x * y]; let result = a
GX
for f in i64_write u8_write; do
  printf '%s: ' $f
  if timeout -s KILL 30 "$G" --no-cache --check $D/$f.gx > $D/$f.out 2>&1; then echo ok
  else echo "refused: $(tail -1 $D/$f.out | sed 's/^ *[0-9]*: //')"; fi
done
rm -rf $D
