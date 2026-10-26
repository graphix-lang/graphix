#!/bin/bash
# Helper for the fuzz-mutate repros: run one typemorph subject through the
# `typemorph-one` child and print its verdict lines (BASE, FLIP/MUTANT,
# PROBE, REJECT). Usage: fuzz-mutate-run-typemorph-one.sh <subject.gx>
out=$(mktemp)
GRAPHIX_FUZZ_SANDBOXED=1 timeout -s KILL 170 \
  /home/eric/tmp/review-2026-10-26/bin/graphix-fuzz typemorph-one "$out" 20000 \
  < "$1" > /dev/null 2>&1
cat "$out"; rm -f "$out"
