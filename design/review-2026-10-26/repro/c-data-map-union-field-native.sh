#!/bin/sh
# Title: a field read or functional update over a union of same-shape
#        tuples/structs/errors is accepted by the checker and the node-walk
#        (data.rs struct_field_types / tuple_field_types union paths) but
#        never fuses: emit_field_ref_node / emit_struct_with_node accept only a
#        bare Tuple/Struct source type.
# Command: sh design/review-2026-10-26/repro/c-data-map-union-field-native.sh
# Expected: each #[native] body fuses (Jit trace equal to the Interp trace).
# Observed (Jit/InLanguage):
#   t.0  -> emit_clif: accessor source of type [(i64, `V(i64)), (i64, `W)] isn't Tuple
#   s.a  -> emit_clif: accessor source of type [{ a: i64, b: `V(i64) }, { a: i64, b: `W }] isn't Struct
#   {s with a: 7} -> emit_clif: struct-with source isn't a struct
#   e.0  -> emit_clif: accessor source of type [Error<`A(i64)>, Error<`B(string)>] isn't Tuple
F=${GRAPHIX_FUZZ:-/home/eric/tmp/review-2026-10-26/bin/graphix-fuzz}
D=$(mktemp -d)
cat > $D/t.gx <<'GX'
type T = [(i64, `V(i64)), (i64, `W)];
let f = |t: T| #[native] t.0;
let n = 0;
n <- select n < 3 { true => n + 1, false => never() };
let t: T = select n % 2 { 0 => (n, `V(n)), _ => (n, `W) };
f(t)
GX
cat > $D/s.gx <<'GX'
type S = [{a: i64, b: `V(i64)}, {a: i64, b: `W}];
let f = |s: S| #[native] s.a;
let n = 0;
n <- select n < 3 { true => n + 1, false => never() };
let s: S = select n % 2 { 0 => {a: n, b: `V(n)}, _ => {a: n, b: `W} };
f(s)
GX
cat > $D/w.gx <<'GX'
type S = [{a: i64, b: `V(i64)}, {a: i64, b: `W}];
let f = |s: S| #[native] {s with a: 7};
let n = 0;
n <- select n < 3 { true => n + 1, false => never() };
let s: S = select n % 2 { 0 => {a: n, b: `V(n)}, _ => {a: n, b: `W} };
f(s)
GX
cat > $D/e.gx <<'GX'
type E = [Error<`A(i64)>, Error<`B(string)>];
let f = |e: E| #[native] e.0;
let n = 0;
n <- select n < 3 { true => n + 1, false => never() };
let e: E = select n % 2 { 0 => error(`A(n)), _ => error(`B("b[n]")) };
f(e)
GX
for p in t s w e; do
  echo "== $p"
  timeout -s KILL 180 $F run $D/$p.gx 2>&1 | grep -E '^(Interp|Jit)/InLanguage:' | sed 's/.*did not fully fuse to native code: |//'
done
rm -r $D
