#!/bin/sh
# Title: a typedef parameter's bound is not handed to the open members of a union
# argument: Result<[i64, 'x], `E> accepts an error in its success part (NoError
# never reaches 'x), while Result<[i64, Error<`Q>], `E> written out is refused;
# and a select over Result<[i64, 'x], `E> is refused for 'x being open beside an
# error although NoError should exempt it.
# Command: sh design/review-2026-10-19/repro/x-kind-markers-noerror-open-member.sh
# Expected: a.gx refused ("NoError does not contain ..."), as b.gx is; c.gx accepted, printing 1
# Observed: a.gx accepted, printing error:`Q; b.gx refused; c.gx refused
#           ("this pattern tests for error among [i64, 'x: unbound] ... annotate it")
d=$(mktemp -d)
G=/home/eric/tmp/review-2026-10-19/bin/graphix
cat > "$d/a.gx" <<'GX'
let r: Result<[i64, 'x], `E> = error(`Q);
sys::exit(sys::time::after_idle(duration:100.ms, 0));
r
GX
cat > "$d/b.gx" <<'GX'
let r: Result<[i64, Error<`Q>], `E> = error(`Q);
r
GX
cat > "$d/c.gx" <<'GX'
let f = |a: 'x| -> i64 { let r: Result<[i64, 'x], `E> = a; select r { error as e => 0, v => 1 } };
sys::exit(sys::time::after_idle(duration:100.ms, 0));
f(1)
GX
for f in a b c; do echo "== $f.gx"; timeout -s KILL 30 $G --no-cache "$d/$f.gx" 2>&1 | tail -1; done
rm -rf "$d"
