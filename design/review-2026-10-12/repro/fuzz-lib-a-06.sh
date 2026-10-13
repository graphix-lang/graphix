#!/bin/sh
# fuzz-lib-a-06: Oracle::settle's nondeterminism drop is silent.
# (fuzz-lib-a-06.gx in this directory belongs to a different note; this
# finding's repro is this script.)
#
# The subject is fuzz-lib-a-01.gx: an Exact-tier program (no rand, no
# clock) whose trace carries a reference's raw BindId, so every run differs.
# The engine pair disagrees, the reference's rerun disagrees with itself,
# and settle returns Settled::Drop without printing anything, unlike the
# CONTAINED / JIT ASSERTION / SLOW drops, which eprintln the program.
#
# Command (run from this directory):
#   XDG_CACHE_HOME=$(mktemp -d) timeout -s KILL 180 graphix-fuzz check fuzz-lib-a-01.gx
# Expected: a NONDETERMINISTIC line with the reference and its rerun (the
#   "(see above)" the UNSURE line promises), or a finding for an Exact-tier program.
# Observed (HEAD 7324207f), the whole output, stdout and stderr:
#   UNSURE — no divergence, but a retry, slowness, containment or nondeterminism decided it (see above)
# `graphix-fuzz run fuzz-lib-a-01.gx` shows why: every run's trace differs in the u64
#   Interp/InLanguage: Trace([0:[u64:4611686018427389929, i64:5]])
#   Jit/InLanguage:    Trace([0:[u64:4611686018427389933, i64:5]])
dir=$(dirname "$0")
XDG_CACHE_HOME=$(mktemp -d) timeout -s KILL 180 graphix-fuzz check "$dir/fuzz-lib-a-01.gx"
