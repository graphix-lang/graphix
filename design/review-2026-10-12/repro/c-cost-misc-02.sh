#!/bin/sh
# c-cost-misc-02: call-site argument independence is decided over Nop
# placeholders of omitted defaults.
# A dynamic call decides its arguments' independence at its first update
# (update_args -> ForkSite::decide_siblings), before rebind compiles the
# omitted default #d in place of its Nop placeholder. The default reaches the
# same ordered queuefn wrapper as the given argument, yet the stale
# `Independent` lets the two fork.
# Run: sh c-cost-misc-02.sh [path/to/graphix]
# Expected (and seen with GRAPHIX_PAR=off): [0, 1000, 1, 1001, 2, 1002, ...]
# Observed with GRAPHIX_PAR=force, most runs: pairs swap, e.g.
#   [0, 1000, 1, 1001, 1002, 2, 1003, 3, 4, 1004, ...]
# Controls under force keep the order: the static call `f2(q(n), 2, 3)`, and
# the dynamic call with the default written out, `f(#d: q(1000 + n), q(n), 2, 3)`.
G=${1:-graphix}
P=$(mktemp --suffix=.gx)
cat > "$P" <<'GX'
{
    let clk = sys::time::timer(duration:1.ms, true);
    let n = 0;
    n <- clk ~ n + 1;
    let args: Array<i64> = [];
    let q = queuefn(#trigger: clk, |x: i64| -> i64 { args <- x ~ array::push(args, x); x });
    let f1 = |#d: i64 = 0, a: i64, b: i64, c: i64| -> i64 a + b + c + d;
    let f2 = |#d: i64 = q(1000 + n), a: i64, b: i64, c: i64| -> i64 a + b + c + d;
    let f = select n { x if x < 0 => f1, _ => f2 };
    let s = f(q(n), 2, 3);
    sys::exit(select n { x if x > 30 => 0, _ => never() });
    select n { x if x > 20 => args, _ => never() }
}
GX
for m in off force force force; do
    echo "== GRAPHIX_PAR=$m"
    GRAPHIX_PAR=$m timeout -s KILL 30 "$G" --no-cache --no-fusion "$P" 2>&1 | head -1
done
rm -f "$P"
