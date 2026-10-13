#!/bin/sh
# Count how often graphix prints the value a program produced before its
# sys::exit fired. Expect 20/20; HEAD prints it in ~12/20.
G=${GRAPHIX:-graphix}
D=$(dirname "$0")
c=0
for i in $(seq 1 20); do
  o=$(timeout -s KILL 30 "$G" --no-cache "$D/x-perf-compile-05-exit-drops-output.gx")
  [ "$o" = "100" ] && c=$((c+1))
done
echo "printed $c/20"
