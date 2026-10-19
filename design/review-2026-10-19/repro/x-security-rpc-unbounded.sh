#!/bin/sh
# Title: sys::net::rpc holds an instance for every call f has not answered, with no bound
# Command: sh x-security-rpc-unbounded.sh   (from this directory; GRAPHIX names the binary)
# Expected: RSS bounded. Observed: ~3.5 MB/s growth while the program writes 1000
# calls/s to the rpc path (the writes carry no reply channel: nobody waits for them).
G=${GRAPHIX:-/home/eric/tmp/review-2026-10-19/bin/graphix}
D=$(dirname "$0")
( timeout -s KILL 35 "$G" --no-cache "$D/x-security-rpc-unbounded.gx" >/dev/null 2>&1 & )
sleep 3
pid=$(pgrep -n -f "no-cache $D/x-security-rpc-unbounded.gx")
for i in 1 2 3 4 5 6; do grep VmRSS /proc/$pid/status; sleep 5; done
