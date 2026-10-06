#!/bin/bash
# x-expr-walks-01: the seq lowering's scoped rename (graphix-types/src/expr/seq.rs,
# shadow_step / rewrite_with_inner) treats `use` and `mod` as transparent.
#
# Command: GRAPHIX=/path/to/graphix bash design/review-2026-10-05/repro/x-expr-walks-01.sh
#
# 1. A block-local `use` shadows an outer `x`, but under `seqq` the read after
#    it is renamed to the outer x's capture.
#    expected: r=8 (what the same select prints outside the seqq, and under `seq t`)
#    observed: r=101
# 2. A `mod` declared in a seqq body's lambda: its file body is walked as part
#    of the seq, so the module's own `x` is renamed to a capture it cannot see.
#    expected: r=15 (what `seq t` and no seq print)
#    observed: refused, "seqqcap<id>_0 not defined" at m2.gx line 2
# 3. Same blind spot in refuse_catch (fold_outside_lambdas): a `catch` in the
#    file of a `mod` declared in a seq step's select-arm block is refused.
#    expected: r=14 (what the program prints without the seq)
#    observed: refused, "catch is not allowed inside a seq" at m3.gx line 2
set -u
GRAPHIX=${GRAPHIX:-graphix}
D=$(mktemp -d)
trap 'rm -rf "$D"' EXIT
export XDG_CACHE_HOME=$D/cache
run() { (cd "$D" && timeout -s KILL 60 "$GRAPHIX" --no-cache "$1" 2>&1 | tail -n 2); }
EXIT='sys::exit(sys::time::after_idle(duration:100.ms, 0))'

cat > "$D/use_seqq.gx" <<EOF
let y = 7;
let x = 100;
let t = 1;
let r = seqq t { select t { _ => { use package::y as x; x + 1 } } };
println("r=[r]");
$EXIT
EOF
cat > "$D/use_plain.gx" <<EOF
let y = 7;
let x = 100;
let t = 1;
let r = select t { _ => { use package::y as x; x + 1 } };
println("r=[r]");
$EXIT
EOF

printf 'let x = 7;\nlet z = x * 2\n' > "$D/m2.gx"
cat > "$D/mod_seqq.gx" <<EOF
let x = 100;
let t = 1;
let r = seqq t { let f = |y| { mod m2; m2::z + y }; f(1) };
println("r=[r]");
$EXIT
EOF
sed 's/seqq t/seq t/' "$D/mod_seqq.gx" > "$D/mod_seq.gx"

printf 'let z = {\n  catch(e) println("[e]");\n  14\n}\n' > "$D/m3.gx"
cat > "$D/catch_seq.gx" <<EOF
let t = 1;
let r = seq t { let v = select t { _ => { mod m3; m3::z } }; v };
println("r=[r]");
$EXIT
EOF
sed 's/seq t { \(.*\) }; *$/{ \1 };/' "$D/catch_seq.gx" > "$D/catch_plain.gx"

echo "== 1. use under seqq (expected r=8)"; run use_seqq.gx
echo "== 1. control, no seqq";              run use_plain.gx
echo "== 2. mod under seqq (expected r=15)"; run mod_seqq.gx
echo "== 2. control, seq t";                run mod_seq.gx
echo "== 3. catch in a mod under seq (expected r=14)"; run catch_seq.gx
echo "== 3. control, no seq";               run catch_plain.gx
