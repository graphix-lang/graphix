#!/bin/bash
# Per-cycle cost scaling probes for review key x-perf-runtime.
# usage: x-perf-runtime.sh GRAPHIX_BIN prog.gx size...   (FLAGS=--no-fusion to node-walk)
# Each .gx starts with `let size = N;` and prints (…, elapsed) for a fixed cycle count;
# the generators (*.py) take the size as argv[1] (05 also takes s|d) and print a program.
B=$1; f=$2; shift 2
for s in "$@"; do
  tmp=$(mktemp --suffix=.gx)
  case $f in
    *.py) python3 $f $s ${ARG:-} > $tmp ;;
    *) sed "s/let size = [0-9]*/let size = $s/" $f > $tmp ;;
  esac
  echo -n "size=$s "
  timeout -s KILL 170 $B --no-cache $FLAGS $tmp 2>&1 | tail -1
  rm -f $tmp
done
