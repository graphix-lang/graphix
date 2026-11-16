#!/bin/sh
# Title: "an element of a collection a call returns is one instance" has no pin
#   that fails if the rule goes: the pins refuse through a callback parameter
#   or an index-read `let`, which are one instance on their own
# Command: sh tests-functions-b-call-result-element-rule-unpinned.sh [path/to/graphix]
# Expected for a pin of the rule: the call-result program is refused while the
#   same reads of a generic holder `[h]` check.
# Observed: the shapes the pins use (functions.rs generic_function_elements_stay_generic:
#   a map callback; collection.rs collection_returned_function_is_one_instance and
#   generics.md: `let g = fs[0]$`) are refused over the generic holder too, with
#   the same message; only direct reads tell the two apart.
G=${1:-/home/eric/tmp/review-2026-11-16/bin/graphix}
D=$(mktemp -d)
H="let h = 'a: Number |x: 'a, y: 'a| -> 'a x + y;"
C="let fs = array::map([1, 2], |r| 'a: Number |x: 'a, y: 'a| -> 'a x + y);"
printf '%s let fs = [h]; array::map(fs, |f| (f(1, 2), f(f32:1.5, f32:2.0)))\n' "$H" > $D/holder_callback.gx
printf '%s let fs = [h]; let g = fs[0]$; (g(1, 2), g(f32:1.5, f32:2.0))\n' "$H" > $D/holder_index_let.gx
printf '%s let fs = [h]; (fs[0]$(1, 2), fs[0]$(f32:1.5, f32:2.0))\n' "$H" > $D/holder_direct.gx
printf '%s (fs[0]$(1, 2), fs[0]$(f32:1.5, f32:2.0))\n' "$C" > $D/call_result_direct.gx
for f in holder_callback holder_index_let holder_direct call_result_direct; do
  printf '%s: ' $f
  if timeout -s KILL 30 "$G" --no-cache --check $D/$f.gx > $D/$f.out 2>&1; then echo ok
  else echo "refused: $(tail -1 $D/$f.out | sed 's/^ *[0-9]*: //')"; fi
done
rm -rf $D
