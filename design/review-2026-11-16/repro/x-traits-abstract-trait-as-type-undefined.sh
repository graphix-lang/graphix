#!/bin/sh
# A trait written where a type is read is refused three ways: a bare
# annotation above the trait, a cast target and a type test say
# "undefined type Sh", where the same name inside an annotation
# (`Array<Sh>`), in `never<Sh>()` or in a bare annotation below the trait
# says "trait Sh used as a type ...".
#
# Command: sh design/review-2026-11-16/repro/x-traits-abstract-trait-as-type-undefined.sh
#
# Expected: every case names the trait ("trait Sh used as a type ...").
# Observed: cases 1-3 print "undefined type Sh at line: 1, column: ..";
# cases 4-6 print the trait refusal.
G=/home/eric/tmp/review-2026-11-16/bin/graphix
D=$(mktemp -d)
T='trait Sh { val show: fn(self) -> string }; impl Sh for i64 { let show = |i| "i" };'
i=0
while IFS= read -r prog; do
  i=$((i+1))
  printf '%s\n' "$prog" > "$D/c.gx"
  printf 'case %s: ' "$i"
  timeout -s KILL 60 "$G" --no-cache --check "$D/c.gx" 2>&1 | tail -1 | sed 's/ in file.*//'
done <<CASES
{ let x: Sh = 1; $T x }
$T cast<Sh>(1)
$T let x: Any = 1; select x { Sh as s => 1, _ => 0 }
{ let x: Array<Sh> = [1]; $T x }
$T never<Sh>()
$T let v: Sh = 1; v
CASES
rm -rf "$D"
