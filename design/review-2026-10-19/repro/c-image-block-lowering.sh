#!/bin/sh
# Title: a warm start re-lowers the seq of a function defined in a block
#   (`{ .. }`), so its run-time instances check instead of substituting.
#   The image relocates the block ids in a lowering key's scope text
#   (image/defs.rs lexical_relocating_encode) but keeps the definition's own
#   scope text verbatim (scope_encode), so the warm key `/#do<new>/#fn<new>`
#   never matches the lookup `/#do<old>/#fn<new>`.
# Command: sh c-image-block-lowering.sh   (needs the review graphix binary)
# Expected: f's instances (line 9) check in about the same time warm as cold
#   (~35us here), as they do for the same program at top level.
# Observed: cold ~31-38us, warm ~120-225us (the lowered seq's nodes check
#   themselves); the top-level copy is ~38us both ways. The cargo tests
#   c-image-block-fn-lowerings.rs and c-image-chained-lowerings.rs show the
#   duplicate lowering directly through EnvStats::lowerings_len.
G=${GRAPHIX:-/home/eric/tmp/review-2026-10-19/bin/graphix}
D=$(mktemp -d)
cp "$(dirname "$0")/c-image-block-lowering.gx" "$D/b.gx"
for run in cold warm; do
  echo "== $run"
  XDG_CACHE_HOME=$D/c GRAPHIX_PROFILE=1 GRAPHIX_PROFILE_INSTANCES=1 \
    timeout -s KILL 30 "$G" "$D/b.gx" 2>&1 | grep '^INSTANCE' \
    | grep 'line: 9, column: 25' | grep 'signature=1' \
    | sed 's/.*check_ns=\([0-9]*\).*/check_ns=\1/'
done
rm -rf "$D"
