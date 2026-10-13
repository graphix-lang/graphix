#!/usr/bin/env bash
# f-mod-lowering-03: with the JIT unavailable, a `#[native]` program fails
# with a diagnostic about the attribute's position.
#
# check_and_fuse_inner (graphix-compiler/src/lib.rs:2389) skips fusion when
# `ctx.fusion.available()` is false, so `check_node_attributes`
# (fusion/mod.rs:1133) never dispatches the attribute; compile_stmt's census
# check (lib.rs:2675) tests only `fusion_on(flags)` and reports every
# undispatched attribute as misplaced.
#
# command: GRAPHIX=<path to graphix> bash design/review-2026-10-12/repro/f-mod-lowering-03.sh
# expected: an error that says fusion is unavailable on this host (or the
#   attribute is waived), not one that blames the attribute's position.
# observed (HEAD 7324207f, debug build):
#   1: attribute in a position the fusion pass cannot check — put the
#      decorated expression in its own statement (or a select arm)
# The same program compiles and prints 41 with a working JIT, and the
# program without `#[native]` prints 41 here too (node-walk fallback).
GRAPHIX=${GRAPHIX:-graphix}
dir=$(mktemp -d)
trap 'rm -rf "$dir"' EXIT
cat > "$dir/nat.gx" <<'GX'
let f = |x: i64| x * 2 + 1;
let r = #[native] f(20);
sys::exit(sys::time::after_idle(duration:100.ms, 0));
r
GX
GRAPHIX_JIT_ARENA=1000000000000000 timeout -s KILL 60 "$GRAPHIX" --no-cache "$dir/nat.gx"
echo "rc=$?"
