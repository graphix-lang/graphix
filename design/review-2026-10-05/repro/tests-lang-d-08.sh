#!/usr/bin/env bash
# tests-lang-d-08: six place/deref fixtures in
# stdlib/graphix-tests/src/lang/byref.rs order their phases by one-shot
# wall-clock timers 30-40 ms apart, so they fail when the test's runtime
# thread is not run across that gap.
#
# The run loop (graphix-rt/src/gx.rs `run`) drains every finished task
# into one `do_cycle`. When nothing polls between the first timer's
# deadline and the second's, both fire in the same cycle and the second
# phase reads or writes before the first phase's `<-` has landed.
# Affected: place_bottom_key, place_removed_element, place_through_deref,
# place_through_bottom_deref (20/50 ms), deref_moved_to_undelivered and
# place_payload (20/60 ms), in all four modes.
#
# This runs the already built graphix-tests binary under CFS bandwidth
# throttling (CPUQuota=200% with 16 test threads): once the quota is
# spent every thread of the cgroup waits for the next 100 ms period,
# the stall a CPU-limited container or VM, or swapping, imposes.
#
# command: cargo test -p graphix-tests --no-run   (once, to build)
#          bash design/review-2026-10-05/repro/tests-lang-d-08.sh [TEST_BINARY]
#          (SCOPE_PROPS adds systemd-run arguments, e.g. a memory cap)
#
# expected: test result: ok. 96 passed; 0 failed (as it is unthrottled)
#
# observed (HEAD c722befe, dev profile, 16-core box), 6 runs: FAILED,
# 16 to 23 of the 24 test functions of the six fixtures each run, every
# one `assertion failed: pred(Ok(&v))` on the value a merged cycle gives:
#   place_through_deref        [i64:10, i64:10]           (expects [20, 20])
#   place_removed_element      [[i64:20, i64:7], i64:20]  (expects obs null)
#   place_payload              [[i64:5, i64:1], i64:5, i64:1, i64:5, i64:1]
#   place_through_bottom_deref [[i64:10, i64:20], i64:10, [i64:99, i64:20]]
#   place_bottom_key           [[i64:10, i64:20], [i64:99, i64:20], ...]
# No other byref fixture fails: their gaps are 70 ms or more, or their
# result does not depend on the order. With the default thread count
# (with or without 32 busy loops sharing the cgroup, two suites at once,
# or a CPU quota) 0 failures in 35 runs of lang::byref and in one
# shuffled run of lang::.
#
# The same wrong values come from the CLI on the fixture programs
# unmodified (`graphix --no-cache [--no-fusion] prog.gx`, both engines)
# when the process is SIGSTOPped from 5 ms to 70 ms after its first
# timer is armed; a stop from 25 ms to 45 ms, which covers neither
# deadline alone, leaves the answer right.
set -u
BIN=${1:-$(find "${CARGO_TARGET_DIR:-$HOME/tmp/target}/debug/deps" -maxdepth 1 \
  -name 'graphix_tests-*' -type f -executable -printf '%T@ %p\n' 2>/dev/null |
  sort -rn | head -1 | cut -d' ' -f2-)}
if [ -z "$BIN" ] || [ ! -x "$BIN" ]; then
  echo "no graphix-tests binary: cargo test -p graphix-tests --no-run" >&2
  exit 2
fi
cd "$(dirname "$0")/../../../stdlib/graphix-tests" || exit 2
# shellcheck disable=SC2086
systemd-run --user --scope --quiet --collect ${SCOPE_PROPS:-} \
  -p CPUQuota=200% -E RUST_TEST_THREADS=16 -- "$BIN" lang::byref 2>&1 |
  awk '/^---- /{name=$2} /^\[|^i64|^-?[0-9]/{if (name) print name ": " $0}
       /^test result/{print}'
