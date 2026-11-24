#!/bin/sh
# Run the language test binary one module at a time with the check by bounds
# on, each under its own memory cap, so a test that runs away is found.
# BOUNDS_MODE=all checks the packages by bounds too.
T=$1; OUT=$2; shift 2
for m in "$@"; do
  f=$OUT/$(echo "$m" | tr ':' '_').txt
  systemd-run --user --wait --collect -q -p MemoryMax=12G -p MemorySwapMax=0 -p RuntimeMaxSec=240 \
    -E GRAPHIX_BOUNDS=${BOUNDS_MODE:-1} -E GRAPHIX_BOUNDS_BLESS=$OUT/bless.txt /bin/sh -c "$T '$m::' > $f 2>&1; echo rc=\$? >> $f"
  echo "$m $(tail -1 $f) $(grep -c '\.\.\. ok' $f) ok $(grep -c 'FAILED' $f) failed" >> $OUT/summary.txt
done
echo done >> $OUT/summary.txt
