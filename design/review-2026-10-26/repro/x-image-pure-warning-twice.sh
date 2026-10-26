#!/bin/bash
# x-image: a restored #[pure] assertion forgets it warned and its flags: warm runs warn twice, and -W error turns into a warning
#
# Run: bash x-image-pure-warning-twice.sh [path-to-graphix]
# Expected: run 1 (cold, writes the entry) and run 2 (restores it) print the
#   "#[pure]: this function prints with `dbg`" WARNING once each.
# Observed (HEAD e6ffc6eed): run 2 prints it twice: once replayed from
#   ProgramRoot::warnings, once more when the first collection slot's run-time
#   bind analyzes the definition, because restore_tables rebuilds each
#   DefAssertion with warned: false (and flags empty) instead of what was written.
G=${1:-/home/eric/tmp/review-2026-10-26/bin/graphix}
D=$(mktemp -d)
cat > $D/p.gx <<'GX'
#[pure]
let f = |x: i64| -> i64 dbg(x) + 1;
let clk = sys::time::timer(duration:10.ms, true);
let n = 0;
n <- clk ~ n + 1;
let a = array::init(n, |i| i);
sys::exit(select n { 3 => n, _ => never() } ~ 0);
array::map(a, |x| f(x))
GX
for i in 1 2; do
  XDG_CACHE_HOME=$D/cache timeout -s KILL 30 $G --no-fusion $D/p.gx > /dev/null 2> $D/err
  echo "run $i: $(grep -c 'WARNING' $D/err) warning(s)"
done
rm -rf $D
# Second case: under `-W error` the restored assertion has lost CFlag::WarningsAreErrors.
# A #[pure] definition first reached by a run-time bind (a dynamic call) reports
# the error "at: line: 2 ... #[pure]: this function prints with `dbg`" on the
# cold run (a failure logged under FAILURE_TARGET) and only "WARNING at ..." on the warm run.
D=$(mktemp -d)
cat > $D/q.gx <<'GX'
#[pure]
let f = |x: i64| -> i64 dbg(x) + 1;
let clk = sys::time::timer(duration:10.ms, true);
let n = 0;
n <- clk ~ n + 1;
let h = |x: i64| -> i64 x * 10;
let g = select n % 2 { 0 => h, _ => f };
sys::exit(select n { 4 => n, _ => never() } ~ 0);
g(n)
GX
for i in 1 2; do
  XDG_CACHE_HOME=$D/cache timeout -s KILL 30 $G -W error --no-fusion $D/q.gx > /dev/null 2> $D/err
  echo "-W error run $i: $(grep -c '^at: line' $D/err) refusal(s) (cold: the bind's error), $(grep -c '^WARNING' $D/err) WARNING(s)"
done
rm -rf $D
