#!/usr/bin/env bash
# fuzz-lib-a-05: the Exact-tier print capture is sorted across the whole
# run, so print pacing and epoch order are never compared
# (graphix-fuzz/src/lib.rs drive_inner, `lines.sort_unstable()`).
#
# command: GRAPHIX_FUZZ=/path/to/graphix-fuzz bash design/review-2026-10-05/repro/fuzz-lib-a-05.sh
#
# Programs a, b and c watch the same value (in0 * 10, in0 = 0, 2, 1 over
# the three epochs) and print the same three lines, placed differently:
#   a: b0, b2, b1, one line per epoch, in the epoch's first cycle
#   b: b0, b1, b2, one line per epoch (epochs 1 and 2 swapped against a)
#   c: a's lines, each one cycle later in its epoch (`d <- in0` lands
#      next cycle)
# The witnesses aw and cw watch the println itself, to show where a's and
# c's prints land: aw is [0:null] in every epoch, cw is [1:null].
#
# expected: a, b and c give three different Traces (print output keyed by
#   epoch and cycle), so an engine that printed like b or c where the
#   node-walk printed like a would be a DIVERGENCE.
# observed (HEAD c722befe, debug build): every engine of a, b and c gives
#   the identical
#     Trace([0:i64:0]; [0:i64:20]; [0:i64:10]; stdout=[b0 | b1 | b2])
#   and Exact-tier agreement is equality of exactly that Trace
#   (Outcome::agrees_with_at -> Trace::agrees_with, `self == other`).
set -u
FUZZ=${GRAPHIX_FUZZ:-graphix-fuzz}
dir=$(mktemp -d)
trap 'rm -rf "$dir"' EXIT
export XDG_CACHE_HOME="$dir/cache"
hdr='// schedule-v1: cap=64 events=512; in0=i64:2; in0=i64:1'

printf '%s\n%s\n' "$hdr" '{ println("b[in0]"); in0 * 10 }' > "$dir/a.gx"
printf '%s\n%s\n' "$hdr" \
  '{ let k = select in0 { 0 => 0, 2 => 1, _ => 2 }; println("b[k]"); in0 * 10 }' \
  > "$dir/b.gx"
printf '%s\n%s\n' "$hdr" \
  '{ let d = never(); d <- in0; println("b[d]"); in0 * 10 }' > "$dir/c.gx"
printf '%s\n%s\n' "$hdr" 'println("b[in0]")' > "$dir/aw.gx"
printf '%s\n%s\n' "$hdr" '{ let d = never(); d <- in0; println("b[d]") }' > "$dir/cw.gx"

for p in a b c aw cw; do
  echo "== $p"
  (cd "$dir" && timeout -s KILL 180 "$FUZZ" run "$p.gx" 2>/dev/null) \
    | grep -E '^(Interp|Jit)/InLanguage: '
done
