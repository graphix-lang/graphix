#!/bin/sh
# A chain of N distinct statically resolved functions, each calling the
# next, aborts the process with a native stack overflow once fused.
# emit_self_dispatch (fusion/emit/call.rs) puts graphix_stack_check only on
# self-calls ("cross-kernel edges are acyclic"), but an acyclic chain is as
# deep as the program is long, and JIT frames cannot grow through stacker.
# Command: sh c-callsite-cross-kernel-chain-overflow.sh [N]   (default N=400)
# Expected: prints 1 + sum(0..N-1) (79801 for N=400), as --no-fusion does
#   (the node-walk handles N=2000 through ensure_sufficient).
# Observed (N=400, fusion on): "thread 'tokio-rt-worker' has overflowed its
#   stack / fatal runtime error: stack overflow, aborting" (exit 134).
#   N=300 still runs; the threshold depends on frame sizes.
G=${G:-/home/eric/tmp/review-2026-10-19/bin/graphix}
N=${1:-400}
D=$(mktemp -d)
F=$D/chain.gx
i=0; while [ $i -lt $N ]; do echo "let v$i = i64:$i;" >> $F; i=$((i+1)); done
echo "let f$N = |x: i64| -> i64 x;" >> $F
i=$((N-1)); while [ $i -ge 0 ]; do echo "let f$i = |x: i64| -> i64 f$((i+1))(x) + v$i;" >> $F; i=$((i-1)); done
echo 'println("[f0(i64:1)]");' >> $F
echo 'sys::exit(sys::time::after_idle(duration:1.ms, 0))' >> $F
echo "== fusion on";  timeout -s KILL 120 $G --no-cache $F > $D/out 2>&1; echo "exit=$?"; head -3 $D/out
echo "== --no-fusion"; timeout -s KILL 120 $G --no-cache --no-fusion $F 2>&1 | head -3
rm -rf $D
