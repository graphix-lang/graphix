#!/bin/sh
# GRAPHIX_PROFILE=1 charges a compile task's time to the phase open on the
# thread that ran it as well as adding the task's own costs at the join, at
# every nesting level: Typecheck1's self_ns comes out at seconds while its
# interval (wall) is tens of milliseconds. HEAD (debug build):
#   phase=Typecheck1 ... self_ns=11223262385 total_ns=68278448
#   interval=Typecheck1 ... duration_ns=68278448
G=${GRAPHIX:-graphix}
D=$(dirname "$0")
GRAPHIX_PROFILE=1 timeout -s KILL 120 "$G" --no-cache --no-fusion \
  "$D/x-perf-compile-02-plan-blocks-cubic.gx" 2>&1 \
  | grep -E 'phase=Typecheck1|interval=Typecheck1' | sort -t= -k5 -n | tail -2
