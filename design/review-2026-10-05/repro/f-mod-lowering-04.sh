#!/usr/bin/env bash
# f-mod-lowering-04: `#[native]` passes a lambda call whose arguments
# run on the node-walk as fed feeders.
#
# fuse() (graphix-compiler/src/fusion/mod.rs:982-985) runs the attribute
# checks on the FusedKernel that try_fuse_feeding_args built, and
# Native::check (graphix-compiler/src/lib.rs:830) accepts any FusedKernel,
# so a fed argument's blockers (recorded in the fusion stats) are never
# reported. Feeding happens only for an argument that fails builtin
# discovery, so whether a node-walked callee under the attribute is
# reported depends on an unrelated builtin in its arguments (cases 4, 5).
#
# command: GRAPHIX=/path/to/graphix bash design/review-2026-10-05/repro/f-mod-lowering-04.sh
#
# expected (CLAUDE.md: "#[native] asserts zero node-walk residue at a
# source location"): all five refused; each leaves throttle, once or the
# loop h on the node-walk. Under the other reading (a call's arguments are
# inputs, design/tail_calls_are_calls.md G3: f(e) is { let a = e; f(a) }),
# 2, 4 and 5 pass alike. Neither reading gives 4 REFUSED and 5 ACCEPTED.
# observed (HEAD c722befe, debug build):
#   1 #[native] throttle(i64:5)                    REFUSED (throttle has no fast-call entry)
#   2 #[native] f(throttle(i64:5))                 ACCEPTED, prints 6
#   3 #[native] { let a = throttle(i64:5); f(a) }  REFUSED (throttle)
#   4 #[native] f(h(1000, 0, &one))                REFUSED (call site h not discovered)
#   5 #[native] f(h(once(1000), 0, &one))          ACCEPTED, prints 1001; h still node-walks
#     (graphix-fuzz run: "lambda `h` has no kernel: formal `k` has no kernel
#     encoding", "lambda call site `h(once(1000), 0, &one)` not discovered";
#     GRAPHIX_DBG_KERNELS=1 builds a kernel for f only)
# Note: lang::fusion::call_fed_by_node_walked_args (stdlib/graphix-tests/
# src/lang/fusion.rs:1877) relies on case 2's behaviour:
# `#[native] f(10, count(a))` must compile.
set -u
GRAPHIX=${GRAPHIX:-graphix}
dir=$(mktemp -d)
trap 'rm -rf "$dir"' EXIT
export XDG_CACHE_HOME="$dir/cache"
EXIT_LINE='sys::exit(sys::time::after_idle(duration:100.ms, 0));'
DEFS='let f = |x: i64| x + 1;
let one = 1;
let rec h = |n: i64, acc: i64, k: &i64| -> i64 select n { 0 => acc, _ => h(n - 1, acc + *k, k) };'
n=0
case_() {
  n=$((n + 1))
  printf '%s\nlet r = %s;\n%s\nr\n' "$DEFS" "$1" "$EXIT_LINE" > "$dir/c$n.gx"
  out=$(timeout -s KILL 60 "$GRAPHIX" --no-cache "$dir/c$n.gx" 2>&1)
  if grep -q 'did not fully fuse' <<<"$out"; then
    why=$(grep -m1 -- '- ' <<<"$out" | sed 's/^ *- //')
    echo "$n $1  REFUSED ($why)"
  else
    echo "$n $1  ACCEPTED, prints $(tr '\n' ' ' <<<"$out")"
  fi
}
case_ '#[native] throttle(i64:5)'
case_ '#[native] f(throttle(i64:5))'
case_ '#[native] { let a = throttle(i64:5); f(a) }'
case_ '#[native] f(h(1000, 0, &one))'
case_ '#[native] f(h(once(1000), 0, &one))'
