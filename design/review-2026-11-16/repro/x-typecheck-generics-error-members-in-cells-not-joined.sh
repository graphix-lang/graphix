#!/bin/sh
# Two error members of one argument are taken together under Result<'a, 'e>
# only when they are written types: the same value built by a select's arms
# (each error member a bound cell) is refused.
#
# command: sh design/review-2026-11-16/repro/x-typecheck-generics-error-members-in-cells-not-joined.sh
#
# expected: both accepted ('b is [`A, `B]), as design/tvar_constraints.md says
#   of two members one constructor member takes ("in either order of the
#   members").
# observed:
#   1 (arms' literals): type mismatch Result<'a: unbound within NoError, 'b: `B>
#      does not contain [i64, Error<'a1: `B>, Error<'a2: `A>]
#   2 (the same select under a written let type): accepted
B=${GRAPHIX:-/home/eric/tmp/review-2026-11-16/bin/graphix}
D=$(mktemp -d)
cat > $D/1.gx <<'GX'
let c = 0;
let r = select c { 0 => 1, 1 => error(`A), _ => error(`B) };
filter_err(r)
GX
cat > $D/2.gx <<'GX'
let c = 0;
let r: [i64, Error<`A>, Error<`B>] = select c { 0 => 1, 1 => error(`A), _ => error(`B) };
filter_err(r)
GX
for i in 1 2; do
  echo "== $i"
  timeout -s KILL 30 $B --no-cache --check $D/$i.gx 2>&1 | tail -1
done
rm -r $D
