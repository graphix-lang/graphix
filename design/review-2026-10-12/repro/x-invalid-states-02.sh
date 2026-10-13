#!/bin/sh
# x-invalid-states-02: a seq Step finds its value by position (catches.len()),
# but a `let f = <lambda>` step puts that let first, so the step completes on
# f's fire and ignores its value's SeqGuard.
#
# command: sh x-invalid-states-02.sh /path/to/graphix
#
# Both programs abort the run in the cycle S1 is entered (S0 writes through a
# reference, so S1 enters the cycle after S0; v reaches 1 that same cycle and
# fires the abort). Per design/seq_blocks.md section 9 the abort wins that
# cycle: S1's guard fails and the machine must not advance.
# expected: neither trace shows a NOTIFY_SET of pc between `pc = "S1"` and
#           `pc = "Idle"`.
# observed: the lambda variant (S1 = `let f = |x: i64| x + v`) shows
#           `NOTIFY_SET <pc>` there: S1 completed on f's fire and the machine
#           advanced into S2 in the aborted cycle. The plain variant
#           (S1 = `let f = v + 1`) does not. Program output is the same (no
#           println runs), because S2 enters in that cycle and its own guard fails.
B=${1:-graphix}
D=$(mktemp -d)
cat > $D/lambda.gx <<'GX'
let go = sys::time::timer(duration:100.ms, false);
let v = 0;
let rv = &mut v;
let r = seq go; abort(filter(v, |x| x > 0)) {
  *rv <- 1;
  let f = |x: i64| x + v;
  println("after f: [f(1)]");
  "done"
};
sys::exit(sys::time::after_idle(duration:400.ms, 0));
r
GX
sed 's/let f = |x: i64| x + v;/let f = v + 1;/; s/f(1)/[f]/' $D/lambda.gx > $D/plain.gx
for p in lambda plain; do
  GRAPHIX_DBG_VARS=1 timeout -s KILL 30 $B --no-cache $D/$p.gx > $D/$p.vars 2>&1
  pc=$(grep -o 'SET_VAR BindId([0-9]*) = "S0"' $D/$p.vars | head -1 | grep -o 'BindId([0-9]*)')
  echo "== $p: pc is $pc; events between pc=S1 and pc=Idle that touch pc:"
  sed -n "/SET_VAR $pc = \"S1\"/,/SET_VAR $pc = \"Idle\"/p" $D/$p.vars | grep -F "$pc"
done
rm -rf $D
