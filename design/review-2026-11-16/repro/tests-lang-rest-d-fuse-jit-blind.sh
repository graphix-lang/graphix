#!/bin/sh
# FuseExpect::Jit on lang::errors::catch_array_index_fused (and
# qop_handler_deliver) cannot tell whether the handler-ful `?` fuses: the
# fixture with that expression replaced by a stateful `count(a)` still
# fuses a region.
# Command: sh tests-lang-rest-d-fuse-jit-blind.sh
# Expected: the second program fuses nothing, so FuseExpect::Jit pins the `?`.
# Observed: "fused=2" for the fixture as written and "fused=1" with the `?`
#   expression replaced: the assertion (regions > 0) passes either way.
BIN=/home/eric/tmp/review-2026-11-16/bin/graphix-fuzz
D=$(mktemp -d)
cat > $D/a.gx <<'G'
let result = {
    let caught = never();
    catch(e) caught <- e ~ 1;
    let a = [10, 20, 30];
    let v = a[5]? * 2;
    caught
};
result
G
sed 's/let v = a\[5\]? \* 2;/let v = count(a);/' $D/a.gx > $D/b.gx
for f in a b; do timeout -s KILL 120 $BIN run $D/$f.gx 2>&1 | grep "fusion:"; done
rm -rf $D
