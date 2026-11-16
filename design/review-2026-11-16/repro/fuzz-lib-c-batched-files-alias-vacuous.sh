#!/bin/bash
# Title: batch_files_test::batched_files_do_not_alias cannot fail: both of its
#        subjects are refused alone (`mod m0;` finds no module), and the test
#        counts a refusal as an agreement (`v != BatchVerdict::Other`).
# Command: bash design/review-2026-11-16/repro/fuzz-lib-c-batched-files-alias-vacuous.sh
# Expected: the test's two subjects check as AGREE (trace) alone, so a
#           batch child that aliased m0.gx across them would turn one into
#           a refusal the test could see.
# Observed (HEAD e1be2fbf2):
#   == a (the test's subject a)       AGREE (reject)   module test::m0 could not be found
#   == b (the test's subject b)       AGREE (reject)
#   == b over a's m0 (the aliasing)   AGREE (reject)   -- indistinguishable
#   == a2/b2 without `mod m0;`        AGREE (trace)
#   == b2 over a's m0 (aliasing)      AGREE (reject)   -- what a Ran-requiring test would see
BIN=${GRAPHIX_FUZZ:-/home/eric/tmp/review-2026-11-16/bin/graphix-fuzz}
D=$(mktemp -d)
printf '{ mod m0; m0::k0 }\n// file-v1: m0.gx\nlet k0 = i64:1\n' > $D/a.gx
printf '{ mod m0; str::len(m0::k0) }\n// file-v1: m0.gx\nlet k0 = "xy"\n' > $D/b.gx
printf '{ mod m0; str::len(m0::k0) }\n// file-v1: m0.gx\nlet k0 = i64:1\n' > $D/b_aliased.gx
printf 'm0::k0\n// file-v1: m0.gx\nlet k0 = i64:1\n' > $D/a2.gx
printf 'str::len(m0::k0)\n// file-v1: m0.gx\nlet k0 = "xy"\n' > $D/b2.gx
printf 'str::len(m0::k0)\n// file-v1: m0.gx\nlet k0 = i64:1\n' > $D/b2_aliased.gx
for f in a b b_aliased a2 b2 b2_aliased; do
  echo "== $f"
  GRAPHIX_FUZZ_SESSIONS=0 GRAPHIX_FUZZ_FORK=0 timeout -s KILL 170 $BIN check $D/$f.gx 2>&1 | tail -1
done
timeout -s KILL 120 $BIN run $D/a.gx 2>&1 | head -1 | cut -c1-200
rm -rf $D
