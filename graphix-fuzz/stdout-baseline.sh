#!/usr/bin/env bash
# Capture each program's stdout under a chosen fusion mode.
#
# The trace oracle compares interp against jit, so a semantics change
# that moves BOTH engines is invisible to it. The print ruling (a print
# fires when its message updates, not when its call runs) is exactly
# that kind of change, so its only real gate is a by-hand diff of
# absolute stdout against a recorded baseline.
#
#   stdout-baseline.sh <outdir> [--no-fusion]
#
# Programs that never quiesce are normal here; each run is capped and
# whatever it printed by then is the record. stderr is dropped: it
# carries timestamped log lines that would never diff clean.
#
# GUI and TUI examples are excluded (they need a display or take over
# the terminal — CLAUDE.md says test those manually) and so are net
# examples (they need a netidx universe).
#
# PIN THE BINARY. Workers exec it per program, so a rebuild mid-run
# swaps code under the capture and the result is mixed-version garbage
# — the same hazard as a fuzz campaign (jul10h). Copy it somewhere
# private first and point GRAPHIX_BIN at the copy.
#
# SUBTRACTION SET. Comparing the two modes at a known-good commit
# leaves ~18 differences that are NOT divergences, and a reader has to
# know them or the output is unusable:
#   bench/*                     self-timed; they print elapsed times
#   soak-jul2026/08, /10        free-running counters — under a fixed
#                               wall-clock cap the line COUNT measures
#                               throughput, not semantics
#   dyncall-stale-arg-fired/02,/03   rand and wall clock
#   select-guard-shortcircuit/02     a known ORDERING difference
#   dyncall-apply-unwired/02         log SCOPE NAME differs
#
# What only this baseline catches: the trace oracle keys each print by
# epoch and cycle offset and sorts the prints of one cycle
# (graphix-fuzz/src/lib.rs, drive_inner), so order across cycles is
# compared but order within one cycle is invisible to it; programs
# compared by final values (Trace::agrees_final) compare each epoch's
# prints as a multiset; and an Excluded program's prints are not
# compared at all. A change that moves both engines, and an order within
# one cycle, show only here.
set -u

outdir=${1:?usage: stdout-baseline.sh <outdir> [--no-fusion]}
shift
mode_args=("$@")

repo=$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)
bin=${GRAPHIX_BIN:-$repo/../../tmp/target/debug/graphix}
[[ -x $bin ]] || bin=$HOME/tmp/target/debug/graphix
timeout_secs=${BASELINE_TIMEOUT:-5}

mkdir -p "$outdir"

# Programs are independent, so they run BASELINE_JOBS at a time
# (default: 2x cores — many programs sit parked against their timeout,
# so oversubscription keeps the cores fed). Each gets a PRIVATE
# sandbox cwd — a shared one would let programs that write files
# collide across parallel runs. The wall clock is dominated by the
# never-quiescing programs' caps: sequentially that was
# ~n_capped * timeout; parallel it is ~ceil(n_capped / jobs) * timeout.
jobs=${BASELINE_JOBS:-$(($(nproc) * 2))}

list() {
    find "$repo/graphix-fuzz/findings" -name '*.gx'
    find "$repo/bench" -name '*.gx' 2>/dev/null
    find "$repo/book/src/examples" -name '*.gx' 2>/dev/null \
        | grep -v '/gui/' | grep -v '/tui/' | grep -v '/net/'
}

export BASELINE_REPO=$repo BASELINE_OUTDIR=$outdir BASELINE_BIN=$bin \
    BASELINE_SECS=$timeout_secs BASELINE_MODE="${mode_args[*]-}"

list | sort | tr '\n' '\0' | xargs -0 -P "$jobs" -n 1 bash -c '
    f=$1
    rel=${f#"$BASELINE_REPO"/}
    out=$BASELINE_OUTDIR/${rel//\//__}.out
    sb=$(mktemp -d)
    ( cd "$sb" && timeout "$BASELINE_SECS" "$BASELINE_BIN" --no-cache $BASELINE_MODE "$f" 2>/dev/null ) >"$out"
    rm -rf "$sb"
' _

echo "captured $(list | wc -l) programs into $outdir"
