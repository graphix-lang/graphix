#!/bin/sh
# Title: default_hidden_by_a_with_update is refused by its plain call, not by the update
# Command: sh tests-functions-b-with-update-hides-nothing.sh [path/to/graphix]
# Expected for a pin of "a with-update judges the default a fn type hides":
#   the update itself is refused, whatever the reader does.
# Observed: `{s with h: f}` gives `h` f's own type (fn<'a: Number>(?#d: 'a, x: 'a)),
#   so it hides nothing: the update with no call, and the call that passes #d,
#   both check; the fixture's refusal is the omitted default at `(s2.h)(1.5)`,
#   the same refusal `f(1.5)` gets. Annotating the update does hide #d and is refused.
G=${1:-/home/eric/tmp/review-2026-11-16/bin/graphix}
D=$(mktemp -d)
P="let f = 'a: Number |#d: 'a = 5, x: 'a| x + d; let s = {h: |x: f64| x};"
printf '%s let s2 = {s with h: f}; (s2.h)(1.5)\n' "$P" > $D/fixture.gx
printf '%s let s2 = {s with h: f}; 0\n' "$P" > $D/no_call.gx
printf '%s let s2 = {s with h: f}; (s2.h)(#d: 2.5, 1.5)\n' "$P" > $D/passes_label.gx
printf '%s f(1.5)\n' "$P" > $D/plain_call.gx
printf '%s let s2: {h: fn(x: f64) -> f64} = {s with h: f}; 0\n' "$P" > $D/annotated_update.gx
for f in fixture no_call passes_label plain_call annotated_update; do
  printf '%s: ' $f
  if timeout -s KILL 30 "$G" --no-cache --check $D/$f.gx > $D/$f.out 2>&1; then echo ok
  else echo "refused: $(tail -1 $D/$f.out | sed 's/^ *[0-9]*: //')"; fi
done
rm -rf $D
