#!/bin/sh
# tests-modules-image-01: an interface val over a compound of a sig variable
# (Array<'a>) hides the Discernible bound the implementation needs: the check
# accepts a call the build then refuses with the compiler-bug message.
# Command: sh tests-modules-image-01.sh /path/to/graphix
# Expected: --check refuses the module ("the implementation requires
#   'a: Discernible", as the trait path says), or refuses the call.
# Observed: --check exits 0; the run fails with
#   "an instance at fn(x: Array<[Array<i64>, (i64, i64)]>, ..) -> bool of a
#    definition typed fn(x: '_N, y: '_N) -> '_N"
# Files used are the ones in tests-modules-image-01/ beside this script.
G=${1:-graphix}
D=$(mktemp -d)
printf "val eq: fn(x: Array<'a>, y: Array<'a>) -> bool\n" > $D/m.gxi
printf 'let eq = |x, y| x == y\n' > $D/m.gx
cat > $D/p.gx <<'GX'
mod m;
let a: Array<[(i64, i64), Array<i64>]> = [(1, 2)];
let b: Array<[(i64, i64), Array<i64>]> = [[1, 2]];
m::eq(a, b)
GX
timeout -s KILL 60 $G --check $D/p.gx; echo "check exit $?"
timeout -s KILL 30 $G --no-cache $D/p.gx; echo "run exit $?"
rm -rf $D
