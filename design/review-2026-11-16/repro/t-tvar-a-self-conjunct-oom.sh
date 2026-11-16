#!/bin/sh
# Title: a cell takes a conjunct that holds the cell itself under a
#        constructor (z within Array<z>) with no occurs check; the definition
#        is accepted and a call's check allocates until it is killed.
#
# Command: sh design/review-2026-11-16/repro/t-tvar-a-self-conjunct-oom.sh
#          (GRAPHIX=/path/to/graphix to pick the binary; run it under a
#          memory cap: the review's wrapper caps a run at 6 GB)
#
# Expected: each definition refused, "cannot infer a finite type here", as
#           `let y = never(); let p = |a, z| g(select c { true => a, false => z }); p(y, [y])`
#           is; no run grows without bound.
# Observed: both definitions check (the first prints its parameter as
#           `'_N: unbound within Array<'_N: …>`), and each call's check was
#           killed at the 6 GB cap in under a second (exit 251).
#           GRAPHIX_DBG_BIND=1 on the first shows the call binding
#           z := Array<e> after the commit of z's conjunct aliased e into z:
#           a cell bound to an array of itself.
G=${GRAPHIX:-/home/eric/tmp/review-2026-11-16/bin/graphix}
D=$(mktemp -d)
trap 'rm -rf "$D"' EXIT
# 1: a Singleton bound hands the union's known member to its open one
#    (Type::require_singleton through tvar::require_bounds)
cat > "$D/singleton_def.gx" <<'GX'
let g = 'a: Singleton |u: 'a| u;
let h = |c, z| g(select c { true => [z], false => z });
1
GX
sed '$d' "$D/singleton_def.gx" > "$D/singleton_call.gx"
echo 'h(true, [])' >> "$D/singleton_call.gx"
# 2: a typedef parameter's bound names another parameter, and one variable
#    is given for both (TypeRef::hold_bounds_of through TVar::add_body_bound)
cat > "$D/typedef_def.gx" <<'GX'
type T<'p: Array<'q>, 'q> = {a: 'p, b: 'q};
let f = |z: 'x| -> T<'x, 'x> {a: z, b: z};
1
GX
sed '$d' "$D/typedef_def.gx" > "$D/typedef_call.gx"
echo 'f([])' >> "$D/typedef_call.gx"
for p in singleton_def singleton_call typedef_def typedef_call; do
    timeout -s KILL 60 "$G" --no-cache -n -i --check "$D/$p.gx" > "$D/out" 2>&1
    echo "== $p: exit $?"
    tail -1 "$D/out" | cut -c1-200
done
