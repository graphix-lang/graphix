#!/bin/sh
# c-data-map-01: T(v) with a type parameter nothing decides mints values whose params hold an
# unbound cell; core-trait dispatch (coretraits.rs impl_for: has_unbound) then skips the impl.
# (c-data-map-01.gx is an unrelated finding's repro, hence a shell script.)
# Run: sh design/review-2026-10-12/repro/c-data-map-01.sh [path/to/graphix]
# Program A, expected: (true, "Bx")          observed: (false, "Bx((1, []))")   impl ignored
# Program B (A plus an unrelated map::insert use of e and f, which settles 'a):
#            expected: (1, "Bx")             observed: (1, "Bx")               impl applies
# Inside a function (|n| Bx((n, []))) the impls also apply. Node-walk and JIT agree.
G=${1:-graphix}
D=$(mktemp -d)
cat > "$D/a.gx" <<'GX'
type Bx<'a> = Abstract<(i64, Array<'a>)>;
impl<'a> Eq for Bx<'a> { let eq = |a, b| true };
impl<'a> Display for Bx<'a> { let fmt = |b| "Bx" };
let e = Bx((1, []));
let f = Bx((2, []));
sys::exit(sys::time::after_idle(duration:100.ms, 0));
(e == f, "[e]")
GX
cat > "$D/b.gx" <<'GX'
type Bx<'a> = Abstract<(i64, Array<'a>)>;
impl<'a> Ord for Bx<'a> { let cmp = |a, b| `Equal };
impl<'a> Eq for Bx<'a> { let eq = |a, b| true };
impl<'a> Display for Bx<'a> { let fmt = |b| "Bx" };
let e = Bx((1, []));
let f = Bx((2, []));
let m = map::insert(map::insert({}, e, 1), f, 2);
sys::exit(sys::time::after_idle(duration:100.ms, 0));
(map::len(m), "[e]")
GX
echo "A:"; timeout -s KILL 60 "$G" --no-cache "$D/a.gx"
echo "B:"; timeout -s KILL 60 "$G" --no-cache "$D/b.gx"
rm -rf "$D"
