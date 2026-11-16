#!/bin/sh
# Title: wildcard_beside_a_bind_used_as_two_types_refused (select_undecided.rs) and
#        select.md's matching `cases` line do not pin the `_`-stands-as-a-name rule
# Command: sh design/review-2026-11-16/repro/tests-lang-rest-a-wildcard-pin-vacuous.sh
# Expected: the fixture's refusal depends on its `(1, _)` arm.
# Observed:
#   a) the fixture: "type mismatch string does not contain i64"
#   b) the fixture with its `(1, _) => 0` arm deleted: the same refusal, word for
#      word -- it comes from `b` bound at one part and used as i64 and as string,
#      a rule of its own; the `_` takes no part in it
#   c) the refusal select.md documents for the `_` rule (a call whose part is wider
#      than the name's twin, `(2, "a")`): refused, "(i64, i64) does not contain
#      (i64, string)" -- pinned by no test and no `cases` line
G=${G:-/home/eric/tmp/review-2026-11-16/bin/graphix}
D=$(mktemp -d)
cat > "$D/a.gx" <<'EOF'
let f = |s| select s { (1, _) => 0, (2, b) => b + 1, (_, b) => str::len(b) };
f((2, 5))
EOF
cat > "$D/b.gx" <<'EOF'
let f = |s| select s { (2, b) => b + 1, (_, b) => str::len(b) };
f((2, 5))
EOF
cat > "$D/c.gx" <<'EOF'
let f = |s| select s { (1, _) => 0, (_, b) => b + 1 };
f((2, "a"))
EOF
for p in a b c; do
  echo "== $p"
  timeout -s KILL 30 "$G" --no-cache --check "$D/$p.gx" 2>&1 | tail -1
done
rm -rf "$D"
