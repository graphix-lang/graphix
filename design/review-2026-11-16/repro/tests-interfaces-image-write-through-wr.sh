#!/bin/sh
# Title: write_through_{a_typedef,a_struct_of_functions,an_array_of_functions}_val_refused
#   are refused whatever is written: the write sits in `wr = |x| { m::c <- x; x }`,
#   whose `x` is wr's own variable, so the refusal is "it holds a type variable of
#   the code around the write", given with no call of wr, or a call with a value
#   as general as the val. The narrow write the fixtures pass is never judged.
# Command: sh design/review-2026-11-16/repro/tests-interfaces-image-write-through-wr.sh
# Expected (if the fixtures pinned the narrow write): the general and no-call
#   variants check, and only the narrow argument is refused.
# Observed: all three wr variants refused with "...it holds a type variable of
#   the code around the write"; only the direct writes are refused for being
#   narrow ("and { f: fn(x: i64) -> i64 } is not").
G=${G:-/home/eric/tmp/review-2026-11-16/bin/graphix}
D=$(mktemp -d)
run() {
  mkdir -p "$D/$1"; cd "$D/$1"
  printf '%s\n' "$2" > main.gx; printf '%s\n' "$3" > m.gxi; printf '%s\n' "$4" > m.gx
  echo "== $1"; timeout -s KILL 30 "$G" --check main.gx 2>&1 | tail -1
}
SIG="val c: {f: fn(x: 'a) -> 'a}"; IMP='let c = {f: |x| x}'
run wr-narrow-as-pinned 'mod m; let wr = |x| { m::c <- x; x }; let w = wr({f: |x: i64| -> i64 x + 100}); m::c.f("s")' "$SIG" "$IMP"
run wr-general 'mod m; let wr = |x| { m::c <- x; x }; let w = wr({f: |x| x}); m::c.f("s")' "$SIG" "$IMP"
run wr-never-called 'mod m; let wr = |x| { m::c <- x; x }; m::c.f("s")' "$SIG" "$IMP"
run direct-narrow 'mod m; m::c <- {f: |x: i64| -> i64 x + 100}; m::c.f("s")' "$SIG" "$IMP"
run array-wr-general 'mod m; let wr = |x| { m::v <- x; x }; let w = wr([|x| x]); m::v[0]$("s")' "val v: Array<fn(x: 'a) -> 'a>" 'let v = [|x| x]'
run array-direct-narrow 'mod m; m::v <- [|x: i64| -> i64 x + 100]; m::v[0]$("s")' "val v: Array<fn(x: 'a) -> 'a>" 'let v = [|x| x]'
rm -rf "$D"
