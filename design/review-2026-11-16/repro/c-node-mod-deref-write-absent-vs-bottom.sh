#!/bin/sh
# `*r <- v` fired while `r` has no value: the write waits for a reference
# that was never delivered, and is dropped for one that is bottom.
#
# Command: sh design/review-2026-11-16/repro/c-node-mod-deref-write-absent-vs-bottom.sh
#
# Both programs bind `r` to "no value until c = 4, then `&mut x`" and write
# 99 through it at c = 1. A reads the bottom of a select's `never()` arm (a
# FreshBottom was delivered, so the store holds an entry), B the bottom of a
# sample whose trigger has not fired (nothing was delivered, the store holds
# none). design/representable_bottom.md: "a never-fired input, a pending
# async result, a `?`-error and a division by zero are the same thing ...
# there is no separate pending channel".
#
# Expected: one answer for both, whichever the rule is.
# Observed: A keeps x = 0 to the end; B has x = 99 from c = 5.
set -e
BIN=${BIN:-/home/eric/tmp/review-2026-11-16/bin/graphix-fuzz}
D=$(mktemp -d)
cat > "$D/a.gx" <<'GX'
{
  let c = array::iter([0, 1, 2, 3, 4, 5, 6, 7]);
  let x = 0;
  let r = select c { k if k >= 4 => k ~ &mut x, _ => never() };
  *r <- select c { 1 => 99, _ => never() };
  let out = (c, x);
  array::group(out, |k, _| k == 8)
}
GX
cat > "$D/b.gx" <<'GX'
{
  let c = array::iter([0, 1, 2, 3, 4, 5, 6, 7]);
  let x = 0;
  let go = select c { 4 => null, _ => never() };
  let r = go ~ &mut x;
  *r <- select c { 1 => 99, _ => never() };
  let out = (c, x);
  array::group(out, |k, _| k == 8)
}
GX
for p in a b; do
  echo "== $p"
  timeout -s KILL 120 "$BIN" run "$D/$p.gx" 2>&1 | grep '^Interp/InLanguage:'
done
rm -rf "$D"
