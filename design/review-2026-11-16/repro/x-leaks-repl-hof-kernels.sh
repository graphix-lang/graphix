#!/bin/sh
# x-leaks: a live definition's instance kernels specialised on a callee that
# was deleted are never forgotten. A REPL input `ap(|x| x + i, 1)` over a
# root `ap` builds an instance of ap whose kernel is cached under ap's own
# LambdaId; dropping the input forgets only the input lambda's id
# (Emission::forget retains by `dead.contains(id)` on the key's LambdaId), so
# every distinct callee leaves its kernel, body, records and code behind.
#
# Run: sh design/review-2026-11-16/repro/x-leaks-repl-hof-kernels.sh
# Expected: RssAnon flat (each input is dropped by the next), as with
# --no-fusion.
# Observed (debug build, HEAD e1be2fbf2), RssAnon at 2000 / 20000 inputs:
#   fusion on:  53752 kB -> 136976 kB (~4.6 KB per input), each batch of 2000
#               slower than the last (2.7 s -> 4.7 s)
#   --no-fusion: 37820 kB -> 38032 kB, 0.65 s per batch throughout
# Through GXHandle::compile (x-leaks-registry-counts.rs, hof_calls), 3000
# inputs: FusionStats::retained 176 -> 20076, jit_committed 86016 -> 3989504.
R=design/review-2026-11-16/repro/x-leaks-repl.py
T=${TMPDIR:-/tmp}/x-leaks-$$
mkdir -p $T
printf 'ONCE:let ap = |f: fn(x: i64) -> i64, x: i64| f(x)\nap(|x| x + {i}, 1)\n' > $T/hof.tmpl
cp $T/hof.tmpl $T/hof_nf.tmpl
timeout -s KILL 130 python3 $R 120 $T/hof.tmpl 10 2000 | grep batch
timeout -s KILL 130 python3 $R 120 $T/hof_nf.tmpl 10 2000 --no-fusion | grep batch
rm -rf $T
