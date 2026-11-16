#!/bin/sh
# Title: a written type test over a scrutinee that is one variant type
#   (`A([i64, null])) does not lower to a kernel; the same test over a tuple,
#   a struct, or a union of the variant with another member does.
# Command: sh tests-select-c-written-test-over-lone-variant-defuses.sh
# Expected: all four #[native] programs build (no output after "==").
# Observed (HEAD e1be2fbf2):
#   == variant: #[native] expression did not fully fuse to native code:
#        - emit_clif: type predicate `A(i64) not lowerable
#   == variant-in-union: (builds)
#   == tuple:            (builds)
#   == struct:           (builds)
# Cause (read): graphix-compiler/src/fusion/emit/select.rs emit_predicate_test
#   takes the graphix_value_is_a path only where the scrutinee's type is a
#   Type::Set; a lone Type::Variant scrutinee falls to "not lowerable".
G=${GRAPHIX:-/home/eric/tmp/review-2026-11-16/bin/graphix}
D=$(mktemp -d)
cat > $D/variant.gx <<'GX'
let f = |x: `A([i64, null])| #[native] select x { `A(null) => 0, `A(i64) as `A(n) => n };
[f(`A(4)), f(`A(null))]
GX
cat > $D/variant-in-union.gx <<'GX'
let f = |x: [`A([i64, null]), `C]| #[native] select x { `A(null) => 0, `A(i64) as `A(n) => n, `C => 9 };
[f(`A(4)), f(`A(null)), f(`C)]
GX
cat > $D/tuple.gx <<'GX'
let f = |x: ([i64, null], i64)| #[native] select x { (null, _) => 0, (i64, i64) as (n, _) => n };
[f((4, 1)), f((null, 1))]
GX
cat > $D/struct.gx <<'GX'
let f = |x: {a: [i64, null], b: i64}| #[native] select x { {a: null, b: _} => 0, {a: i64, b: i64} as {a, b: _} => a };
[f({a: 4, b: 1}), f({a: null, b: 1})]
GX
for f in variant variant-in-union tuple struct; do
  echo "== $f"
  timeout -s KILL 60 $G --no-cache --expand $D/$f.gx 2>&1 | grep -v '^$' | tail -2 | sed 's/^ *[0-9]*: //'
done
rm -rf $D
