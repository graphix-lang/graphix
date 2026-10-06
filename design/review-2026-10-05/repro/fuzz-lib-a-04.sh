#!/usr/bin/env bash
# fuzz-lib-a-04: slow-budget retries discard their own evidence, and the
# forked pair never retries a timed-out serial side
# (graphix-fuzz/src/lib.rs: check_verdict 1515-1556, session_divergence
# 1133-1148, check_par 1577-1597).
#
# command: GRAPHIX_FUZZ=/path/to/graphix-fuzz bash design/review-2026-10-05/repro/fuzz-lib-a-04.sh
#   About 2 minutes, one check at a time, up to ~9GB for the check of slow.gx.
#   `graphix-fuzz check` gives each run 10s and a slow retry 80s. PAR_K and
#   SLOW_N are sized for the debug build on a 16-thread box: raise them for
#   a release build so the serial runs still need more than 10s.
#   session_divergence (line 1137) has B's flaw and is not exercised here.
#
# A, check_par: par.gx is deterministic. It scans one 1.3MB string with a
#   regex PAR_K times (~2ms a scan). Serially both engines need more than
#   the 10s budget: with PAR_K = 8000, `graphix --no-cache [--no-fusion]
#   prog` with GRAPHIX_PAR=off prints r=8000 after 13-15s. Forked, the scans
#   take a few seconds.
#   expected: AGREE. A forked run that finishes first is not a divergence.
#   observed (HEAD c722befe, debug build):
#     DIVERGENCE — asymmetric timeout (interp exceeded 8x budget; JIT produced
#       a value — verify the node-walk terminates and agrees ...)
#       interp: Timeout(Deadline)
#       interp/par: Trace([0:i64:10000])
#   No 8x retry ran. check_par retries only a timed-out forked side, and
#   interp2 timed out like interp. par_small.gx (PAR_K / 10): AGREE.
#
# B, check_verdict: slow.gx is f-kernel-02's JIT divergence (`raised` is 2
#   in the node-walk and 1 in the JIT at cycle 4), placed beside a tail
#   recursion that takes the node-walk 10-20s and the JIT about 0.1s.
#   `graphix --no-cache --no-fusion` on the same body with go(250000) prints
#   (0, 1, 749997) and then (0, 2, 749997) after about 21s on a loaded box.
#   expected: DIVERGENCE — fusion/JIT bug (interp != jit), with the 8x
#     retry's trace as interp. That is what fast.gx (the same program with
#     go(1000)) reports:
#       interp: Trace([2:[i64:0, i64:1, i64:3003] 4:[i64:0, i64:2, i64:3003]])
#       jit: Trace([2:[i64:0, i64:1, i64:3003] 4:[i64:0, i64:1, i64:3003]])
#   observed (HEAD c722befe, debug build; go(220000), go(275000) and
#   go(350000) alike):
#     DIVERGENCE — asymmetric timeout (interp exceeded 8x budget; JIT produced
#       a value — verify the node-walk terminates and agrees ...)
#       interp: Timeout(Deadline)
#       jit: Trace([2:[i64:0, i64:1, i64:899998] 4:[i64:0, i64:1, i64:899998]])
#   The whole check takes 37-45s, so the 80s retry finished. It returned the
#   node-walk's trace (a Timeout would have taken 80s; a trace equal to the
#   JIT's would have returned AGREE). That trace was then discarded:
#   interp2 timed out like the first run, and the stale Timeout was recorded.
#   Had interp2 finished, `interp.agrees_with_at(&interp2)` (Timeout against
#   a trace) would have dropped the finding as nondeterminism.
set -u
FUZZ=${GRAPHIX_FUZZ:-graphix-fuzz}
PAR_K=${PAR_K:-10000}
SLOW_N=${SLOW_N:-300000}
dir=$(mktemp -d)
trap 'rm -rf "$dir"' EXIT
export XDG_CACHE_HOME="$dir/cache"

par() {
  echo '{'
  echo '    let s0 = "abcdefghij";'
  for i in $(seq 1 17); do
    echo "    let s$i = str::concat(s$((i - 1)), s$((i - 1)));"
  done
  echo "    let hits = array::map(array::init($1, |i| i), |i| re::is_match(#pat: r\"\\w+\\d\", s17)\$);"
  echo '    array::len(array::filter(hits, |b| !b))'
  echo '}'
}

slow() {
  cat <<EOF
{
  let rec go = |i: i64, acc: i64| -> i64 select i {
    0 => acc,
    _ => go(i - 1, acc + i % 7)
  };
  let heavy = go($1, 0);
  let s = array::iter([1, 2, 1, 2]);
  let t = "x";
  let raised = 0;
  let out = {
    catch(x) raised <- x ~ raised + 1;
    select s { 1 => cast<i64>(t)? + 1, _ => 0 }
  };
  (out, raised, heavy)
}
EOF
}

par "$PAR_K" > "$dir/par.gx"
par $((PAR_K / 10)) > "$dir/par_small.gx"
slow "$SLOW_N" > "$dir/slow.gx"
slow 1000 > "$dir/fast.gx"

for p in par_small par fast slow; do
  echo "== $p"
  (cd "$dir" && timeout -s KILL 180 "$FUZZ" check "$p.gx" 2>&1) \
    | grep -v 'could not send batch'
done
