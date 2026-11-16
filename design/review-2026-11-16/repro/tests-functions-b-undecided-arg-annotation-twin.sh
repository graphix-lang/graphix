#!/bin/sh
# Title: the "annotate the undecided one" remedy of the undecided-argument refusal has no pin
# Command: sh tests-functions-b-undecided-arg-annotation-twin.sh [path/to/graphix]
# Expected: the refusal names two fixes (pass the widest first, or annotate the
#   undecided one); both twins should be pinned beside it.
# Observed: the annotated twins check, but no fixture holds them (functions.rs pins
#   only the reordered twins, open_argument_takes_the_variable_as_it_stands and
#   nested_open_argument_takes_the_variable_as_it_stands).
G=${1:-/home/eric/tmp/review-2026-11-16/bin/graphix}
D=$(mktemp -d)
cat > $D/refused.gx <<'GX'
let pick = |x: 'a, y: 'a, z: 'a| -> Array<'a> [x, y, z];
let v: [i64, null] = null;
let h = |z| pick(i64:1, z, v);
h(null)
GX
cat > $D/annotated.gx <<'GX'
let pick = |x: 'a, y: 'a, z: 'a| -> Array<'a> [x, y, z];
let v: [i64, null] = null;
let h = |z: [i64, null]| pick(i64:1, z, v);
h(null)
GX
cat > $D/nested_refused.gx <<'GX'
let pick = |x: Array<'a>, y: Array<'a>, z: Array<'a>| -> Array<Array<'a>> [x, y, z];
let v: [i64, null] = null;
let h = |z| pick([i64:1], [z], [v]);
h(null)
GX
cat > $D/nested_annotated.gx <<'GX'
let pick = |x: Array<'a>, y: Array<'a>, z: Array<'a>| -> Array<Array<'a>> [x, y, z];
let v: [i64, null] = null;
let h = |z: [i64, null]| pick([i64:1], [z], [v]);
h(null)
GX
for f in refused annotated nested_refused nested_annotated; do
  printf '%s: ' $f
  if timeout -s KILL 30 "$G" --no-cache --check $D/$f.gx > $D/$f.out 2>&1; then echo ok
  else echo "refused: $(grep -o "an undecided argument before this one met [^,]*" $D/$f.out)"; fi
done
rm -rf $D
