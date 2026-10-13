#!/bin/sh
# fuzz-mutate-07: typemorph's open-let guard (permutable, let-inline's `opens`)
# sees only direct names, so a read of an open let through a closure or an
# alias let flips.
# Run: sh design/review-2026-10-12/repro/fuzz-mutate-07.sh  (GRAPHIX_FUZZ=path to graphix-fuzz)
# Expected: "typemorph: no flips" for all three (every base program checks).
# Observed (HEAD 7324207f):
#   closure: TYPEFLIP stmt-permute#2: N: type mismatch iN does not contain [iN, string]
#   alias:   TYPEFLIP stmt-permute#2: N: type mismatch iN does not contain [iN, string]
#   inline:  TYPEFLIP let-inline#2 and stmt-permute#2, same message
# Control: the same programs reading `z` directly print "typemorph: no flips".
F=${GRAPHIX_FUZZ:-graphix-fuzz}
D=$(mktemp -d)
echo '{ let z = never(); let f = |x: i64| z; let b: i64 = f(2); let a: [i64, string] = f(1); (a, b) }' > $D/closure.gx
echo '{ let z = never(); let v = z; let b: i64 = z; let a: [i64, string] = v; (a, b) }' > $D/alias.gx
echo '{ let z = never(); let f = |x: i64| z; let v = { let q: i64 = f(1); q }; let a: [i64, string] = f(2); let b = v; (a, b) }' > $D/inline.gx
echo '{ let z = never(); let b: i64 = z; let a: [i64, string] = z; (a, b) }' > $D/control.gx
for p in closure alias inline control; do
    echo "== $p"
    timeout -s KILL 180 "$F" typemorph $D/$p.gx
done
rm -rf $D
