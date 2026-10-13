#!/bin/sh
# fuzz-lib-a-01: oracle_tier substring markers exclude every read_all/write_all
# program and any sys program calling error().
# The fire-count markers ("all(", "or(", ...) in graphix_fuzz::oracle_tier are
# raw substrings: "all(" matches `read_all(`, "or(" matches `error(`.
# Command: sh fuzz-lib-a-01.sh /path/to/graphix-fuzz
# Expected: all three print AGREE (FinalValues comparison).
# Observed: read_all -> EXCLUDED; metadata -> AGREE; metadata + error(..) -> EXCLUDED.
F=${1:-graphix-fuzz}
D=$(mktemp -d)
printf '%s\n' 'let r = sys::fs::read_all("/nonexistent-review-path");' \
  'select r { error as _ => 0, _ => 1 }' > $D/read_all.gx
printf '%s\n' 'let r = sys::fs::metadata("/nonexistent-review-path");' \
  'select r { error as _ => 0, _ => 1 }' > $D/metadata.gx
printf '%s\n' 'let r = sys::fs::metadata("/nonexistent-review-path");' \
  'let e = error(`Boom);' \
  'select r { error as _ => 0, _ => 1 }' > $D/metadata_error.gx
for p in read_all metadata metadata_error; do
  echo "== $p"; timeout -s KILL 180 "$F" check $D/$p.gx 2>&1 | tail -1
done
rm -rf $D
