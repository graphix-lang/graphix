#!/usr/bin/env bash
# c-analysis-branch-02: a core-trait dispatch (an `impl Eq/Ord/Display`
# method run through coretraits::call_hook) is invisible to the
# dependency summaries (analysis.rs local_summary), so neither the seq
# boundary rule nor the block fork plan sees what the method reads, and
# pooled hook sites hand forked dispatches other state than serial ones.
#
# command: GRAPHIX=/path/to/graphix bash design/review-2026-10-05/repro/c-analysis-branch-02.sh
#
# expected (CLAUDE.md seq rule; parallel eval: a forked cycle computes
# what the serial node-walk does):
#   1. seq { sym <- "EUR"; "[price]" } is "EUR5", as with a plain fn
#   2. seq { k <- 10; Eq::eq(Key(1), Key(11)) } is true, as with a plain fn
#   3, 5. every (n, same) is true in every mode
#   4. "$0" "c11" "c22" "c33" in every mode
#   6. (x, y) is the same off and forced
# observed (HEAD c722befe, debug build):
#   1. "$5" (plan S0 -> S1 same); the plain-fn control is "EUR5" (S0 -> S1 next)
#   2. false (plan S0 -> S1 same); the plain-fn control is true
#   3. comparison before the impl: off all true, force false from n=1
#   4. impl first, Display reads *r: off "$0" "c11" "c22" "c33",
#      force "$0" "c01" "c12" "c23" (the symbol of the cycle before)
#   5. default mode (no env var), heavy statements: a mix of true and
#      false that changes from run to run (12/28, 33/7, 13/27 true/false
#      in one run of this script; 10/30 and 40/0 in another); off all true
#   6. stateful Eq: off (true, false), force (true, true)
# graphix-fuzz check reports case 6, and cases 3 and 4 with the timer
# replaced by the fuzzer's input in0 (`// schedule-v1: cap=64 events=512;
# in0=i64:3; in0=i64:4; in0=i64:5`), as "DIVERGENCE — parallel evaluation
# bug (forked node-walk != serial node-walk)"; cases 1 and 2 AGREE (both
# engines give the same wrong answer).
set -u
GRAPHIX=${GRAPHIX:-graphix}
dir=$(mktemp -d)
trap 'rm -rf "$dir"' EXIT
export XDG_CACHE_HOME="$dir/cache"
run() { timeout -s KILL 30 "$GRAPHIX" --no-cache "$@" | tr '\n' ' '; echo; }
plan() { timeout -s KILL 30 "$GRAPHIX" --expand "$1" | grep -o 'steps: .*'; }

cat > "$dir/seq_display.gx" <<'EOF'
type Money = Abstract<i64>;
let sym = "$";
impl Display for Money { let fmt = |m| "[sym][m.0]" };
let price = Money(5);
let shown = seq { sym <- "EUR"; "[price]" };
sys::exit(sys::time::after_idle(duration:100.ms, 0));
shown
EOF
cat > "$dir/seq_display_ctrl.gx" <<'EOF'
type Money = Abstract<i64>;
let sym = "$";
let show = |m: Money| "[sym][m.0]";
let price = Money(5);
let shown = seq { sym <- "EUR"; show(price) };
sys::exit(sys::time::after_idle(duration:100.ms, 0));
shown
EOF
cat > "$dir/seq_eq.gx" <<'EOF'
type Key = Abstract<i64>;
let k = 0;
impl Eq for Key { let eq = |a, b| (a.0 + k) == b.0 };
let same = seq { k <- 10; Eq::eq(Key(1), Key(11)) };
sys::exit(sys::time::after_idle(duration:100.ms, 0));
same
EOF
cat > "$dir/seq_eq_ctrl.gx" <<'EOF'
let k = 0;
let eqk = |a, b| (a + k) == b;
let same = seq { k <- 10; eqk(1, 11) };
sys::exit(sys::time::after_idle(duration:100.ms, 0));
same
EOF
cat > "$dir/fork_before_impl.gx" <<'EOF'
type Key = Abstract<i64>;
let clock = sys::time::timer(duration:20.ms, true);
let n = 0;
n <- clock ~ n + 1;
let k = n * 10;
let same = Key(n) == Key(n + n * 10);
impl Eq for Key { let eq = |a, b| (a.0 + k) == b.0 };
sys::exit(select n { 5 => 0, _ => never() });
(n, same)
EOF
cat > "$dir/fork_deref.gx" <<'EOF'
type Money = Abstract<i64>;
let clock = sys::time::timer(duration:20.ms, true);
let n = 0;
n <- clock ~ n + 1;
let base = "$";
let r: &string = &base;
impl Display for Money { let fmt = |m| "[*r][m.0]" };
let sym = "c[n]";
let shown = "[Money(n)]";
r <- &sym;
sys::exit(select n { 4 => 0, _ => never() });
shown
EOF
cat > "$dir/fork_auto.gx" <<'EOF'
type Key = Abstract<i64>;
let clock = sys::time::timer(duration:20.ms, true);
let n = 0;
n <- clock ~ n + 1;
let work = |x| array::fold(array::init(20000, |i| i), x, |acc, i| acc + i - i);
let k = work(n) * 10;
let same = Key(work(n)) == Key(n + n * 10);
impl Eq for Key { let eq = |a, b| (a.0 + k) == b.0 };
sys::exit(select n { 40 => 0, _ => never() });
(n, same)
EOF
cat > "$dir/pool.gx" <<'EOF'
type Key = Abstract<i64>;
impl Eq for Key { let eq = |a, b| count(a) == 1 };
let x = Key(1) == Key(1);
let y = Key(2) == Key(2);
sys::exit(sys::time::after_idle(duration:100.ms, 0));
(x, y)
EOF

echo "1. seq + Display impl:  $(GRAPHIX_PAR=off run "$dir/seq_display.gx")$(plan "$dir/seq_display.gx")"
echo "   plain fn control:    $(GRAPHIX_PAR=off run "$dir/seq_display_ctrl.gx")$(plan "$dir/seq_display_ctrl.gx")"
echo "2. seq + Eq::eq call:   $(GRAPHIX_PAR=off run "$dir/seq_eq.gx")$(plan "$dir/seq_eq.gx")"
echo "   plain fn control:    $(GRAPHIX_PAR=off run "$dir/seq_eq_ctrl.gx")$(plan "$dir/seq_eq_ctrl.gx")"
echo "3. comparison before impl, off:   $(GRAPHIX_PAR=off run "$dir/fork_before_impl.gx")"
echo "   comparison before impl, force: $(GRAPHIX_PAR=force run "$dir/fork_before_impl.gx")"
echo "4. impl first, fmt reads *r, off:   $(GRAPHIX_PAR=off run "$dir/fork_deref.gx")"
echo "   impl first, fmt reads *r, force: $(GRAPHIX_PAR=force run "$dir/fork_deref.gx")"
for i in 1 2 3; do
    out=$(env -u GRAPHIX_PAR timeout -s KILL 30 "$GRAPHIX" --no-cache "$dir/fork_auto.gx")
    echo "5. default mode, run $i: $(grep -c true <<< "$out") true, $(grep -c false <<< "$out") false"
done
out=$(GRAPHIX_PAR=off timeout -s KILL 30 "$GRAPHIX" --no-cache "$dir/fork_auto.gx")
echo "   off:                 $(grep -c true <<< "$out") true, $(grep -c false <<< "$out") false"
echo "6. stateful Eq, off:   $(GRAPHIX_PAR=off run "$dir/pool.gx")"
echo "   stateful Eq, force: $(GRAPHIX_PAR=force run "$dir/pool.gx")"
