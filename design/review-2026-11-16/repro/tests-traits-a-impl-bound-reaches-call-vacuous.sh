#!/bin/sh
# Title: impl_head_bound_reaches_the_call is refused by h's own declared
#   `'a: Number + Show`, not by the impl head's bound reaching the call.
# Command: sh design/review-2026-11-16/repro/tests-traits-a-impl-bound-reaches-call-vacuous.sh
# Expected: the fixture's refusal depends on the impl's bound `'b: Show` being
#   handed to `'a`.
# Observed: (1) the fixture and (2) the same h with no trait call at all give the
#   identical refusal "Array<'_: unbound within Number & Show> does not contain
#   Array<f64>"; (3) h with a bare `'a` (the case where only the impl's bound can
#   refuse) is refused "Array<'_: unbound within Show> does not contain Array<f64>",
#   which no fixture pins.
G=${G:-/home/eric/tmp/review-2026-11-16/bin/graphix}
d=$(mktemp -d)
pre='trait Show { val show: fn(self) -> string };
impl Show for i64 { let show = |x| "i" };
impl<'"'"'b: Show> Show for Array<'"'"'b> { let show = |xs| "arr" };'
printf '%s\n%s\n' "$pre" "let h = 'a: Number + Show |x: Array<'a>| -> string Show::show(x); h([1.5])" > $d/1-fixture.gx
printf '%s\n%s\n' "$pre" "let h = 'a: Number + Show |x: Array<'a>| -> string \"no call\"; h([1.5])" > $d/2-no-trait-call.gx
printf '%s\n%s\n' "$pre" "let h = |x: Array<'a>| -> string Show::show(x); h([1.5])" > $d/3-bare-variable.gx
for f in 1-fixture 2-no-trait-call 3-bare-variable; do
  echo "== $f: $(tail -1 $d/$f.gx)"
  timeout -s KILL 30 $G --no-cache --check $d/$f.gx 2>&1 | tail -1
done
rm -rf $d
