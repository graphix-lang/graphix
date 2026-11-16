#!/bin/sh
# An implementation whose lambda parameter is annotated with a bare type name
# that the body declares for itself, named like
# a type its interface sees (a prelude type, an imported one) passes the
# interface check; callers then pass the interface's type into code compiled
# for the body's.
#
# command: sh design/review-2026-11-16/repro/x-typecheck-generics-sig-shadowed-param-type.sh
#
# 1 (file module, prelude name): m.gxi `val g: fn(x: Number) -> string;`,
#   m.gx `type Number = string; let g = |x: Number| x`, main `str::len(m::g(48))`
#   expected: the check refuses m.gx (the val's parameter is core's Number, the
#     implementation's is the body's string)
#   observed: --check accepts; the run dies of SIGSEGV (exit 139) in the JIT
#     (with 41 for 48 the debug build aborts: "misaligned pointer dereference:
#     address must be a multiple of 0x8 but is 0x29" in arcstr); --no-fusion
#     prints nothing
# 2 (dynamic module, imported abstract type): the body declares its own `C`
#   over a function and calls the host's i64 payload
#   expected: the load is refused
#   observed: thread 'graphix-runtime' panicked at
#     graphix-compiler/src/node/callsite.rs: value I64(41) is not a function;
#     Error: runtime did not respond
# 3 (the same resolution, a valid program): m.gxi `val g: fn(x: i64) -> i64;`,
#   m.gx `type D = i64; let g = |x: D| x`
#   expected: accepted, as `let g = |x: i64| -> D x` is
#   observed: val g is declared fn(x: i64) -> i64 but implemented fn(x: D) ->
#     i64: in argument 0: undefined type D in m
B=${GRAPHIX:-/home/eric/tmp/review-2026-11-16/bin/graphix}
D=$(mktemp -d)
mkdir $D/1 $D/3
printf 'val g: fn(x: Number) -> string;\n' > $D/1/m.gxi
printf 'type Number = string;\nlet g = |x: Number| x\n' > $D/1/m.gx
printf 'mod m;\nlet r = str::len(m::g(48));\nsys::exit(sys::time::after_idle(duration:300.ms, 0));\ndbg(r)\n' > $D/1/main.gx
echo "== 1 check"; timeout -s KILL 30 $B --no-cache --check $D/1/main.gx; echo "[exit $?]"
echo "== 1 run"; timeout -s KILL 30 $B --no-cache -n -i $D/1/main.gx 2>&1 | tail -2; 
cat > $D/2.gx <<'GX'
type C = Abstract<i64>;
let c = C(41);
let st = mod foo dynamic {
  sig { use super::C; val g: fn(x: C) -> i64 };
  source "type C = Abstract<fn(x: i64) -> i64>; let g = |x: C| x.0(1)"
};
let r = select st { null as _ => foo::g(st ~ c), error as e => never(dbg(e)) };
sys::exit(sys::time::after_idle(duration:400.ms, 0));
dbg(r)
GX
echo "== 2 run"; timeout -s KILL 30 $B --no-cache -n -i $D/2.gx 2>&1 | tail -4
printf 'val g: fn(x: i64) -> i64;\n' > $D/3/m.gxi
printf 'type D = i64;\nlet g = |x: D| x\n' > $D/3/m.gx
printf 'mod m;\nm::g(1)\n' > $D/3/main.gx
echo "== 3 check"; timeout -s KILL 30 $B --no-cache --check $D/3/main.gx 2>&1 | tail -1
rm -r $D
