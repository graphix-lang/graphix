#!/bin/sh
# A union of two array types fits a generic `Array<'a>` formal only where the
# formal is a union holding a bare variable.
#
# command: sh design/review-2026-11-16/repro/x-typecheck-generics-union-of-arrays-to-generic-formal.sh
#
# expected: 1-4 all accepted ('a is the union of the element types, as 4 has
#   it and as `let t: Array<[i64, null]> = v` accepts the same value), or all
#   refused naming the annotation.
# observed:
#   1: type mismatch Array<'a: i64> does not contain [Array<i64>, Array<null>]
#   2: type mismatch Array<'a: i64> does not contain [Array<i64>, Array<string>]
#   3: type mismatch [null, Array<'a: i64>] does not contain [Array<i64>, Array<string>]
#   4: accepted
B=${GRAPHIX:-/home/eric/tmp/review-2026-11-16/bin/graphix}
D=$(mktemp -d)
cat > $D/1.gx <<'GX'
let c = true;
let v = select c { true => [1], false => [null] };
array::len(v)
GX
cat > $D/2.gx <<'GX'
let x: [`Ints(Array<i64>), `Strs(Array<string>)] = `Ints([1]);
select x { `Ints(a) | `Strs(a) => array::len(a) }
GX
cat > $D/3.gx <<'GX'
let e: Array<i64> = [1];
let v: [Array<i64>, Array<string>] = e;
let f = |a: [Array<'a>, null]| 0;
f(v)
GX
cat > $D/4.gx <<'GX'
let e: Array<i64> = [1];
let v: [Array<i64>, Array<string>] = e;
let f = |a: ['z, Array<'b>], w: 'z| 0;
f(v, 1.5)
GX
for i in 1 2 3 4; do
  echo "== $i"
  timeout -s KILL 30 $B --no-cache --check $D/$i.gx 2>&1 | tail -1
done
rm -r $D
