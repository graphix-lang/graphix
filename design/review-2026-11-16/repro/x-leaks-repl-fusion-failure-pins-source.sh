#!/bin/sh
# x-leaks: FusionStats::failed keeps every fusion failure of every compile the
# context runs, each with its FusionSource's Arc<Origin>, so a REPL input (or
# an embedder's compile) holding an unfusable lambda keeps its whole source
# text for the session after the input is dropped.
#
# Each input carries a 4000-byte string and a lambda whose body writes a
# binding (a fusion blocker). The control is the same input with a pure
# lambda (it fuses, nothing is recorded).
#
# Run: sh design/review-2026-11-16/repro/x-leaks-repl-fusion-failure-pins-source.sh
# Expected: RssAnon flat in both runs (each input is dropped by the next).
# Observed (debug build, HEAD e1be2fbf2), RssAnon after 500 / 5000 inputs:
#   failing lambda: 42388 kB -> 68040 kB (~5.7 KB per input: the text)
#   pure lambda:    38860 kB -> 42248 kB, oscillating 39.6..42.4 MB (flat)
R=design/review-2026-11-16/repro/x-leaks-repl.py
T=${TMPDIR:-/tmp}/x-leaks-$$
mkdir -p $T
PAD=$(python3 -c "print('x'*4000)")
echo "{{ let pad = \"$PAD\"; let f = |k: i64| {{ let s = 0; s <- k; s + 1 }}; f({i}) }}" > $T/fail.tmpl
echo "{{ let pad = \"$PAD\"; let f = |k: i64| k + 1; f({i}) }}" > $T/pure.tmpl
timeout -s KILL 130 python3 $R 120 $T/fail.tmpl 10 500 | grep batch
timeout -s KILL 130 python3 $R 120 $T/pure.tmpl 10 500 | grep batch
rm -rf $T
