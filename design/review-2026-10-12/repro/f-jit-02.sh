#!/bin/sh
# f-jit-02: a link batch has no size bound; a batch larger than the arena
# panics after a futile retry.
#
# Program: f-jit-02-batch-exceeds-arena.gx, 400 top-level statements, each one
# a fused region (about 1.4MB of code in all).
# Command: GRAPHIX=<graphix binary> sh f-jit-02.sh
#
# Expected: every run exits 0. LINK_BATCH (256 regions) should bound a batch,
# and a full arena rotates to a fresh generation.
# Observed:
#   1.25MB arena, parallel fusion (default): panic at jit.rs install_or_panic,
#     "the JIT could not link emitted code: the JIT code arena is exhausted",
#     then "Error: loading initial modules / channel closed", exit 1.
#     fuse_each joins all 400 tasks before it checks LINK_BATCH, so the whole
#     program is one batch, and the retry in a fresh generation of the same
#     size fails too.
#   1.25MB arena, GRAPHIX_FUSE_SERIAL=1: exit 0. try_fuse links every 256
#     regions, so each batch fits a fresh arena. The serial/parallel A/B gives
#     different results.
#   1.5MB arena, parallel: exit 0.
G=${GRAPHIX:-graphix}
D=$(dirname "$0")
P="$D/f-jit-02-batch-exceeds-arena.gx"
GRAPHIX_JIT_ARENA=1310720 timeout -s KILL 120 $G --no-cache "$P"; echo "1.25MB arena, parallel: exit $?"
GRAPHIX_FUSE_SERIAL=1 GRAPHIX_JIT_ARENA=1310720 timeout -s KILL 120 $G --no-cache "$P"; echo "1.25MB arena, serial: exit $?"
GRAPHIX_JIT_ARENA=1572864 timeout -s KILL 120 $G --no-cache "$P"; echo "1.5MB arena, parallel: exit $?"
