#!/bin/sh
# fuzz-lib-b-06: Schedule::parse drops a `// callable-v1:` header that sits
# above its own header, and the minimizer (also mutate_wrapper and
# typemorph_subject) renders the callable line first.
#
# Command (from the repo root; GF is the graphix-fuzz binary):
#   GF=graphix-fuzz sh design/review-2026-10-05/repro/fuzz-lib-b-06.sh
#
# One program, written twice: the schedule line first, then the callable
# line first (the order CallSpec::render documents and minimize's `reattach`
# emits). A dispatch writes 7 into `seen` and the verdict then settles on
# `TwinDiverged, so any run that dispatches is a twin finding by
# construction; no compiler bug is involved.
#
# Expected: both orders report the twin DIVERGENCE, and the minimizer's
# output (budget 1: no reduction at all) still diverges.
# Observed:
#   schedule first: DIVERGENCE — twin invariant violated (...)
#   callable first: AGREE — interp and jit, no cache, cold and warm produce the same result
#   minimize, schedule first, budget 1: no divergence to minimize (program agrees)
# `$GF run` on the callable-first file still lists the Dispatch routes, but
# every trace stops after the schedule epoch: nothing is dispatched.
GF=${GF:-graphix-fuzz}
dir=$(mktemp -d)
trap 'rm -rf "$dir"' EXIT
export XDG_CACHE_HOME="$dir/cache"
CALL='// callable-v1: handler=m0::handler; cx0=i64:7'
SCHED='// schedule-v1: cap=64 events=512; in0=i64:1'
BODY='{ let o = m0::verdict; (o, in0) }
// file-v1: m0.gx
let seen = 0;
let handler = |x: i64| -> null { seen <- x; null };
let verdict = select seen {
  0 => `Ok(0),
  n => `TwinDiverged(n)
}'
printf '%s\n%s\n%s\n' "$SCHED" "$CALL" "$BODY" > "$dir/sched_first.gx"
printf '%s\n%s\n%s\n' "$CALL" "$SCHED" "$BODY" > "$dir/call_first.gx"
echo "== check, schedule line first"
"$GF" check "$dir/sched_first.gx" 2>/dev/null | head -1
echo "== check, callable line first"
"$GF" check "$dir/call_first.gx" 2>/dev/null | head -1
echo "== minimize the schedule-first file, budget 1"
"$GF" minimize "$dir/sched_first.gx" 1 2>/dev/null | head -1
