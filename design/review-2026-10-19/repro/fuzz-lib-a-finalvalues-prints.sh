#!/bin/sh
# Title: print output is captured only for Exact-tier programs; a program that
#   touches any sys::/http::/json::read marker (FinalValues) is compared with no
#   stdout at all, though Trace::agrees_final already compares prints per epoch.
# Command: sh design/review-2026-10-19/repro/fuzz-lib-a-finalvalues-prints.sh
# Expected: both traces carry stdout=[...] for the println.
# Observed: fvprint's traces have no stdout; exprint's (Exact) do.
BIN=/home/eric/tmp/review-2026-10-19/bin/graphix-fuzz
D=$(mktemp -d)
cat > "$D/fvprint.gx" <<'GX'
{
  let d = sys::fs::is_dir("/");
  println("[d$]");
  d$
}
GX
cat > "$D/exprint.gx" <<'GX'
{
  let d = "/";
  println("[d]");
  d
}
GX
for f in fvprint exprint; do
  echo "== $f"
  timeout -s KILL 120 "$BIN" run "$D/$f.gx" 2>&1 | grep -E '^(Interp|Jit)/InLanguage:'
done
rm -rf "$D"
