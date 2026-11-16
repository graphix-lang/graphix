#!/bin/sh
# A typed read whose written target is a union holding a member data cannot
# make (a reference, a function) is accepted with the target narrowed to one
# other member, or refused as an ambiguity it does not have.
#
# command: sh design/review-2026-11-16/repro/x-typecheck-generics-read-target-union-nondata.sh
#
# expected: all three refused the way `cast<[null, {v: i64, next: &i64}]>(1)`
#   ("can't cast a reference") and a struct target alone are, naming the
#   member that is no data; or 'b taking every member that is data.
# observed:
#   1: check accepts; 'b is `null` alone, so the read of an object answers
#      error:`InvalidCast("can't cast [[\"next\", 5], [\"v\", 1]] to null: no primitive conversion")
#   2: check accepts; 'b is `Nil alone:
#      error:`ParseError("can't cast `Cons(1, `Nil) to `Nil: variant tag mismatch")
#   3: refused: "'b: unbound within Concrete & JsonData & NoError could go to
#      null or { v: i64 } of the union [null, { next: &i64, v: i64 }, { v: i64 }],
#      and nothing has decided which: annotate the value"
B=${GRAPHIX:-/home/eric/tmp/review-2026-11-16/bin/graphix}
D=$(mktemp -d)
cat > $D/1.gx <<'GX'
let y: Result<[null, {v: i64, next: &i64}], [`JsonErr(string), `InvalidCast(string)]> = json::read("{\"v\": 1, \"next\": 5}");
sys::exit(sys::time::after_idle(duration:200.ms, 0));
dbg(y)
GX
cat > $D/2.gx <<'GX'
type L = [`Nil, `Cons(i64, &L)];
let y: Result<L, `ParseError(string)> = str::parse("`Cons(1, `Nil)");
sys::exit(sys::time::after_idle(duration:200.ms, 0));
dbg(y)
GX
cat > $D/3.gx <<'GX'
let y: Result<[null, {v: i64}, {v: i64, next: &i64}], [`JsonErr(string), `InvalidCast(string)]> = json::read("{\"v\": 1}");
y
GX
for i in 1 2 3; do
  echo "== $i"
  timeout -s KILL 30 $B --no-cache -n -i $D/$i.gx 2>&1 | tail -2
done
rm -r $D
