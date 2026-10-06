#!/usr/bin/env bash
# c-module-traits-04: an interface's `impl<'a> T for X<'a>;` is fulfilled by
# an implementation whose head only overlaps it (`impl T for X<i64>`).
#
# register_impl (graphix-types/src/env.rs:1042) pairs an implementation with
# a declaration when their heads merely overlap (heads_overlap: containment
# either way), and check_sig's Impl arm (graphix-compiler/src/node/module.rs:
# 493-541) proxies the declared method bindings to the implementation's
# without comparing heads or method types (the `val` arm runs sig_matches).
# Consumers are typed against the declared, wider impl.
#
# command: GRAPHIX=/path/to/graphix bash design/review-2026-10-05/repro/c-module-traits-04.sh
#
# expected: both programs refused by `--check` (the implementation does not
# fulfil `impl<'a> Show for Box<'a>`).
#
# observed (HEAD c722befe, debug build):
#   static:  --check exit 0; the run is refused at elaboration:
#            "an instance at fn(self: Box<string>) -> string of a definition
#             typed fn(b: Box<i64>) -> string" (GRAPHIX_ELAB_AUDIT reports it)
#   dynamic: --check exit 0; the loaded impl typed fn(b: Box<i64>) -> i64 runs
#            on a Box<string>:
#            --no-fusion prints "n: i64 = hello", then an arith error;
#            fused, the runtime panics at fusion/kernel.rs:243 "kernel param
#            `n`: runtime String("hello") does not match the compiled
#            Scalar(I64) slot", then "Error: runtime did not respond", exit 1
set -u
GRAPHIX=${GRAPHIX:-graphix}
dir=$(mktemp -d)
trap 'rm -rf "$dir"' EXIT
export XDG_CACHE_HOME="$dir/cache"

mkdir -p "$dir/static"
cat > "$dir/static/main.gx" <<'GX'
trait Show { val show: fn(self) -> string };
mod m;
"[Show::show(m::Box(1))] [Show::show(m::Box("hello"))]"
GX
cat > "$dir/static/m.gxi" <<'GX'
use super::Show;
type Box<'a> = Abstract<'a>;
impl<'a> Show for Box<'a>;
GX
cat > "$dir/static/m.gx" <<'GX'
impl Show for Box<i64> { let show = |b| "int [b.0 + 1]" }
GX

cat > "$dir/dynamic.gx" <<'GX'
trait Val { val v: fn(self) -> i64 };
let source = """
    type Box<'a> = Abstract<'a>;
    impl Val for Box<i64> { let v = |b| b.0 };
    let make = |x| Box(x)
""";
let status = mod foo dynamic {
    sandbox whitelist [core];
    sig {
        use super::Val;
        type Box<'a>;
        impl<'a> Val for Box<'a>;
        val make: fn(x: 'a) -> Box<'a>
    };
    source source
};
let n = select status {
    error as e => never(dbg(e)),
    null as _ => Val::v(foo::make("hello"))
};
println("n: i64 = [n]");
let m = n * 2 + 1;
println("m = [m]");
sys::exit(sys::time::after_idle(duration:300.ms, 0))
GX

echo "=== static: --check"
timeout -s KILL 60 "$GRAPHIX" --check "$dir/static/main.gx"; echo "exit=$?"
echo "=== static: run"
timeout -s KILL 30 "$GRAPHIX" --no-cache "$dir/static/main.gx" 2>&1 | tail -2
echo "=== dynamic: --check"
timeout -s KILL 60 "$GRAPHIX" --check "$dir/dynamic.gx"; echo "exit=$?"
echo "=== dynamic: run --no-fusion"
timeout -s KILL 30 "$GRAPHIX" --no-cache --no-fusion "$dir/dynamic.gx" 2>&1 | head -2
echo "=== dynamic: run (fused)"
timeout -s KILL 30 "$GRAPHIX" --no-cache "$dir/dynamic.gx" 2>&1 | head -6; echo "exit=${PIPESTATUS[0]}"
