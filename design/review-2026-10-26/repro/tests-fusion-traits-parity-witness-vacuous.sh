#!/bin/sh
# Title: fusion_parity's witness 00 no longer tells a fused compile from a node-walked one
# Command: sh tests-fusion-traits-parity-witness-vacuous.sh
# Expected: the witness exists to show that attempting fusion rewrites a shared
#   type cell: its header records the node-walk refusal as
#   "string does not contain '_6070: [i64, '_6068: _]" and the fused one as
#   "string does not contain '_9158: i64".
# Observed (HEAD e6ffc6eed): both modes, and the check alone, refuse with
#   "type mismatch string does not contain i64": the select's type is already
#   i64 before fusion runs, so an in-place normalize by fusion (the hazard
#   try_freeze_for_abi_normalized documents) would leave the text unchanged and
#   lang::fusion_parity::refusals_identical_across_fusion would still pass.
G=/home/eric/tmp/review-2026-10-26/bin/graphix
W=/home/eric/proj/graphix/graphix-fuzz/findings/fusion-mutates-tvars-aug2026/00_check_diagnostic_type_differs.gx
for flags in "--no-cache --no-fusion" "--no-cache" "--check"; do
  echo "== $flags"
  timeout -s KILL 30 $G $flags $W 2>&1 | tail -1
done
