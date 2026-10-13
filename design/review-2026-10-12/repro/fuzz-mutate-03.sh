#!/bin/sh
# fuzz-mutate-03: same-form appends a dead `_ => never()` after a bind
# catch-all, so its mutant is refused at the select even if the
# one-runtime-form rule regressed.
#
# Run: sh design/review-2026-10-12/repro/fuzz-mutate-03.sh <dir-with-graphix-and-graphix-fuzz>
#
# Expected: the lane's ok for same-form#4 should depend on the
#   one-runtime-form rule alone.
# Observed:
#   1. typemorph-one prints `REJECT same-form#4 ok`.
#   2. mutant.gx (what the family builds) is refused "this pattern can't
#      tell string from `TmSame" -- but so is the same mutant without the
#      appended arm (noextra.gx): the arm is not needed for exhaustiveness.
#   3. control.gx is the mutant with the widening replaced by a value of a
#      distinct runtime form (`true`), i.e. what the mutant checks as if the
#      rule had regressed: still refused, "unreachable arm" at the
#      appended `never()`, inside the select's span, which right_site
#      accepts. So the lane would keep printing ok.
B=${1:?bin dir}
T=$(mktemp -d)
cat > $T/base.gx <<'G'
{
  let s: [string, i64] = "a";
  let r = select s { string as x => 1, y => 2 };
  r
}
G
cat > $T/mutant.gx <<'G'
{
  let s: [string, i64] = "a";
  let r = select (select 1 == 1 { true => s, false => `TmSame }) { string as x => 1, y => 2, _ => never() };
  r
}
G
cat > $T/noextra.gx <<'G'
{
  let s: [string, i64] = "a";
  let r = select (select 1 == 1 { true => s, false => `TmSame }) { string as x => 1, y => 2 };
  r
}
G
cat > $T/control.gx <<'G'
{
  let s: [string, i64] = "a";
  let r = select (select 1 == 1 { true => s, false => true }) { string as x => 1, y => 2, _ => never() };
  r
}
G
timeout -s KILL 170 $B/graphix-fuzz typemorph-one $T/out.txt 5000 < $T/base.gx
grep REJECT $T/out.txt
for f in mutant noextra control; do
  echo "== $f"
  timeout -s KILL 60 $B/graphix --check $T/$f.gx 2>&1 | tail -1
done
