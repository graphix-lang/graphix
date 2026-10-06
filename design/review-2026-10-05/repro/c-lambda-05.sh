#!/usr/bin/env bash
# c-lambda-05: a definition's effect facts are lost by concurrent run-time
# binds. infer_effects (graphix-compiler/src/analysis.rs:388-393) writes
# each definition's facts as def_facts(d).join(eff[iid]): a read of
# intrinsic_effect (a Mutex) and stateless (an AtomicBool), a join, then
# two separate writes, with nothing held across them. Run-time binds in
# forked branches analyze concurrently (CallSite::build_bound ->
# analyze_bound_callee) and share the LambdaDef, so a bind of a pure
# instance that read the facts before a stateful instance's write stores
# them back as pure after it. arm_sleeps_on_deselect then reads the pure
# facts, the arm is not slept on deselect, and its `count` is not reset.
#
# Each trial binds, in one cycle and in sibling branches (a tuple's
# fields fork under GRAPHIX_PAR=force), outer_a, whose select arm calls
# apply(scb, x) (scb holds a `count`), and six outer_b, whose calls are
# all apply(pcb, x) (pure). The trial's phase goes On (m < 3), Off
# (3 <= m < 6), On again, and its last value is the count of the second
# On run.
#
# command: GRAPHIX=/path/to/graphix bash design/review-2026-10-05/repro/c-lambda-05.sh
#
# expected (CLAUDE.md: a forked cycle computes what the serial node-walk
# does; count clears in its own sleep()): every trial ends at 5, in every
# mode, as GRAPHIX_PAR=off prints.
#
# observed (HEAD c722befe, debug build, 16 cores, other probes running):
#   GRAPHIX_PAR=off: 200 x 5.
#   GRAPHIX_PAR=force, shared apply: 5 of 8 runs end one to three trials
#   at 8 (the count ran on from 3: the arm was never slept); 15 of 3000
#   trials over this script's runs and earlier ones of the same program.
#   Control (outer_b on applyb, so no bind shares apply's facts with
#   outer_a's): 200 x 5 in every run, 0 of 2600 trials. A second control,
#   outer_b's calls stateful (every concurrent write is stateful): 0 of 1000.
#   Under gdb (non-stop; breakpoints between the read in def_facts and the
#   stores in infer_effects's final loop widen the window) a one-trial
#   version ends at 8 in 11 of 25 runs, and each of those traces shows
#   apply's LambdaDef stored stateless=false by outer_a's thread, then
#   stateless=true by an outer_b thread that read it before; the 14 runs
#   ending at 5 keep stateless=false; both controls and GRAPHIX_PAR=off
#   under the same breakpoints end at 5 (0 of 28).
set -u
GRAPHIX=${GRAPHIX:-graphix}
RUNS=${RUNS:-8}
dir=$(mktemp -d)
trap 'rm -rf "$dir"' EXIT
export XDG_CACHE_HOME="$dir/cache"

# prog B_HOF: the trial program, outer_b's calls going through B_HOF
prog() {
  local fill="apply(pcb, x), apply(pcb, x), apply(pcb, x), apply(pcb, x), apply(pcb, x), apply(pcb, x), apply(pcb, x), apply(pcb, x)"
  cat <<EOP
let n = 1;
n <- select n { k if k < 200 => k + 1, _ => never() };
let make = |t: i64| {
  let m = 0;
  m <- select m { k if k < 10 => k + 1, _ => never() };
  let phase = select m { k if k < 3 => \`On, k if k < 6 => \`Off, _ => \`On };
  let apply = |cb: fn(x: i64) -> i64, x: i64| cb(x);
  let applyb = |cb: fn(x: i64) -> i64, x: i64| cb(x);
  let scb = |x: i64| count(x);
  let pcb = |x: i64| x + 1;
  let outer_a = |x: i64| {
    let filler = [$fill];
    select phase { \`On => apply(scb, x), \`Off => -1 }
  };
  let outer_b = |x: i64| {
    let filler = [${fill//apply/$1}];
    select phase { \`On => $1(pcb, x), \`Off => -1 }
  };
  let fa = select t { _ => outer_a };
  let fb = select t { _ => outer_b };
  let r = (fa(m), fb(m), fb(m), fb(m), fb(m), fb(m), fb(m));
  r.0
};
sys::exit(sys::time::after_idle(duration:3.s, n) ~ 0);
array::init(n, |t| make(t))
EOP
}
prog apply > "$dir/shared.gx"
prog applyb > "$dir/control.gx"

# tally MODE FILE: the trials' final values, as count x value
tally() {
  GRAPHIX_PAR=$1 timeout -s KILL 170 "$GRAPHIX" --no-cache "$2" | tail -1 |
    tr -d '[] ' | tr ',' '\n' | sort -n | uniq -c | awk '{printf "%s x %s; ", $1, $2}'
  echo
}
echo "== GRAPHIX_PAR=off, shared apply"
tally off "$dir/shared.gx"
echo "== GRAPHIX_PAR=force, shared apply ($RUNS runs)"
for i in $(seq 1 "$RUNS"); do tally force "$dir/shared.gx"; done
echo "== GRAPHIX_PAR=force, control: outer_b on applyb ($RUNS runs)"
for i in $(seq 1 "$RUNS"); do tally force "$dir/control.gx"; done
