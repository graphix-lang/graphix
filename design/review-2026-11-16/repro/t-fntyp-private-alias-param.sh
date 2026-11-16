#!/bin/bash
# Title: a val implemented by a lambda whose parameter is annotated with a
#        module-private type alias is refused "undefined type V in foo"
# Command: bash design/review-2026-11-16/repro/t-fntyp-private-alias-param.sh
# Expected: both checks pass (V is an alias of i64, declared in foo.gx above
#           its use; `let f: fn(x: V) -> i64 = |x| x` and `|x: i64| -> V x`
#           in the same place pass).
# Observed: static module:
#   val f is declared fn(x: i64) -> i64 but implemented fn(x: V) -> i64: in
#   argument 0: undefined type V in foo at line: 2, column: 13 in file .../foo.gx
#           dynamic module: the load fails with the same text
#   error:`DynamicLoadError("compile error ... undefined type V in foo ...")
G=${GRAPHIX:-/home/eric/tmp/review-2026-11-16/bin/graphix}
D=$(mktemp -d)
trap 'rm -rf "$D"' EXIT
cd "$D" || exit 2
printf 'val f: fn(x: i64) -> i64;\n' > foo.gxi
printf 'type V = i64;\nlet f = |x: V| x\n' > foo.gx
printf 'mod foo;\nfoo::f(1)\n' > main.gx
echo "--- static module, --check"
timeout -s KILL 60 "$G" --no-cache --check main.gx
echo "exit $?"
cat > dyn.gx <<'GX'
let st = mod foo dynamic { sig { val f: fn(x: i64) -> i64 }; source "type V = i64; let f = |x: V| x" };
let r = select st { error as e => "[e]", null as _ => "ok [foo::f(1)]" };
sys::exit(sys::time::after_idle(duration:300.ms, 0));
r
GX
echo "--- dynamic module, run"
timeout -s KILL 30 "$G" --no-cache dyn.gx
echo "exit $?"
