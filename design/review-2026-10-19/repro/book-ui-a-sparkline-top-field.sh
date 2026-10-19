#!/bin/sh
# Title: sparkline.md's top|awk pipeline publishes the "ni" (nice) column, not CPU usage
# Command: sh design/review-2026-10-19/repro/book-ui-a-sparkline-top-field.sh
# Expected: the published value tracks CPU usage (e.g. 100 - idle, or the "us" column)
# Observed (2026-10-19, CPU 91% busy):
#   %Cpu(s): 91.2 us,  6.6 sy,  0.0 ni,  1.6 id, ...
#   /local/metrics/cpu|f64|0.0
# Field 6 of "%Cpu(s): <us> us, <sy> sy, <ni> ni, <id> id, ..." is the nice percentage.
timeout -s KILL 20 top -b -n 1 | grep Cpu
timeout -s KILL 20 top -b -n 1 | grep --line-buffered Cpu | \
  awk '{ printf("/local/metrics/cpu|f64|%s\n", $6); fflush() }'
