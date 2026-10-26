#!/bin/sh
# Title: a warm image start runs its first cycle before the fork calibration lands, so
# Auto forks nothing there; the mandelbrot example reports ~6x its cold time on every
# run after the first.
# Command: sh design/review-2026-10-26/repro/examples-gui-warm-first-cycle-serial.sh
# Expected: the warm run's first-cycle kernel loop forks as the cold run's does
#   ("PAR kernel loop: 7500 slots, ..." and a time near the cold one).
# Observed (debug build, 2026-10-26):
#   cold: PAR calibrated ...; PAR kernel loop: 7500 slots, 16 probed, 441 ranges;
#         computed 7500 pixels in 0.0025s
#   warm: PAR calibrated ...; (no kernel loop line) computed 7500 pixels in 0.0162s
#   GRAPHIX_PAR=off: 0.0143s (the warm figure is the serial one)
#   warm with GRAPHIX_PAR_T=20000 (calibration fixed): 0.0045s
G=/home/eric/tmp/review-2026-10-26/bin/graphix
P=$(dirname "$0")/examples-gui-mandelbrot-headless.gx
C=$(mktemp -d)
echo cold; GRAPHIX_DBG_PAR=1 XDG_CACHE_HOME=$C timeout -s KILL 120 $G $P 2>&1 | head -3
echo warm; GRAPHIX_DBG_PAR=1 XDG_CACHE_HOME=$C timeout -s KILL 120 $G $P 2>&1 | head -3
echo off; GRAPHIX_PAR=off timeout -s KILL 120 $G --no-cache $P 2>&1 | head -1
rm -rf "$C"
