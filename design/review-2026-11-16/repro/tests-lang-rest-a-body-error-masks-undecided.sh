#!/bin/sh
# Title: a later arm's pattern that decides an undecided part is reported as an
#        earlier arm's body error, never as the "not decided" refusal that names
#        the binding to annotate (pinned as intended by
#        select_undecided.rs::an_earlier_body_over_a_part_a_later_arm_decides_is_its_error)
# Command: sh design/review-2026-11-16/repro/tests-lang-rest-a-body-error-masks-undecided.sh
# Expected: all three programs refused the same way, at the arm `(`M, _)`:
#   "(`M, _) tests a value of 'x, a type not decided at this select ... annotate it,
#    writing for 'x the type of `x`"
# Observed:
#   a) the fixture: "pattern `J will never match `M, unused match cases" at the inner
#      select over `a` (x was never written `M; only the refused arm decided it)
#   b) the same arm with a body that does not read `a`: the "not decided" refusal
#   c) the user's natural fix of (a), deleting the `J arm the error calls dead:
#      the "not decided" refusal, at last
G=${G:-/home/eric/tmp/review-2026-11-16/bin/graphix}
D=$(mktemp -d)
cat > "$D/a.gx" <<'EOF'
let f = |b: bool, x| select (x, 0) { (a, _) if b => select a { `J => 1, `M => 2 }, (`M, _) => 3, _ => 4 };
f(true, `M)
EOF
cat > "$D/b.gx" <<'EOF'
let f = |b: bool, x| select (x, 0) { (a, _) if b => 1, (`M, _) => 3, _ => 4 };
f(true, `M)
EOF
cat > "$D/c.gx" <<'EOF'
let f = |b: bool, x| select (x, 0) { (a, _) if b => select a { `M => 2 }, (`M, _) => 3, _ => 4 };
f(true, `M)
EOF
for p in a b c; do
  echo "== $p"
  timeout -s KILL 30 "$G" --no-cache --check "$D/$p.gx" 2>&1 | tail -1
done
rm -rf "$D"
