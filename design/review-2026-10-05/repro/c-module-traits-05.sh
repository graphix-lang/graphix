#!/usr/bin/env bash
# c-module-traits-05: an `impl` inside a lambda body: --check accepts it, the
# build refuses it as conflicting with itself on the first call, and an
# uncalled lambda's impl leaks into the rest of the compile.
#
# A lambda body compiles once for the definition's check (Lambda::typecheck0,
# graphix-compiler/src/node/lambda.rs:1767), which discards it through the
# deferred ctx.discard_apply (lambda.rs:1803; deleted only at
# ExecCtx::apply_deferred), and again for every instance. Impl::compile
# registers the impl in the global table at compile time
# (graphix-compiler/src/node/traits.rs:549) and only Impl::delete removes it.
# So the first instance meets the check's registration and register_impl
# (graphix-types/src/env.rs:1053) refuses it, naming its own line; --check
# (CFlag::CheckOnly, no instances) never compiles a second copy. With no call,
# the check's impl is registered until apply_deferred: compile-time trait
# resolution anywhere in the program finds it, run-time binds after
# apply_deferred do not.
#
# command: GRAPHIX=/path/to/graphix GRAPHIX_FUZZ=/path/to/graphix-fuzz \
#          bash design/review-2026-10-05/repro/c-module-traits-05.sh
#
# expected: the check and the build agree on every program: called.gx is
# either refused by both (an impl in a per-instance body) or runs "T<1>";
# leaked.gx is either refused by both or prints ["T<1>", "T<2>", "T<3>"] in
# both engines.
#
# observed (HEAD c722befe, debug build):
#   called.gx:   --check exit 0; the build fails:
#                "conflicting implementation: Display is already implemented
#                for T at line: 3, column: 3" (the impl's own position)
#   leaked.gx:   --check exit 0 (f is never called; control.gx, the same
#                program without f, is refused by --check:
#                "'self: unbound within Show does not contain T");
#                fused it prints ["T<1>", "T<2>", "T<3>"], --no-fusion prints
#                nothing; graphix-fuzz check: "DIVERGENCE — fusion/JIT bug
#                (interp != jit)", interp Trace([]), jit
#                Trace([0:["T<1>", "T<2>", "T<3>"]]), the node-walk logging
#                "a run-time bind ... did not elaborate: ... no implementation
#                of Show for T" per slot.
set -u
GRAPHIX=${GRAPHIX:-graphix}
GRAPHIX_FUZZ=${GRAPHIX_FUZZ:-graphix-fuzz}
dir=$(mktemp -d)
trap 'rm -rf "$dir"' EXIT
export XDG_CACHE_HOME="$dir/cache"

cat > "$dir/called.gx" <<'EOF'
type T = Abstract<i64>;
let f = |x: i64| -> string {
  impl Display for T { let fmt = |t| "T<[t.0]>" };
  "[T(x)]"
};
sys::exit(sys::time::after_idle(duration:100.ms, 0));
f(1)
EOF
cat > "$dir/leaked.gx" <<'EOF'
trait Show { val show: fn(self) -> string };
type T = Abstract<i64>;
let f = |x: i64| -> string {
  impl Show for T { let show = |t| "T<[t.0]>" };
  "[x]"
};
array::map([1, 2, 3], |x| Show::show(T(x)))
EOF
cat > "$dir/control.gx" <<'EOF'
trait Show { val show: fn(self) -> string };
type T = Abstract<i64>;
array::map([1, 2, 3], |x| Show::show(T(x)))
EOF
# the shell runs leaked.gx with an exit; the fuzzer runs it as written
sed '$i sys::exit(sys::time::after_idle(duration:100.ms, 0));' \
    "$dir/leaked.gx" > "$dir/leaked_exit.gx"

check() {
    echo "--- --check $1"
    timeout -s KILL 30 "$GRAPHIX" --check "$dir/$1" 2>&1 | tail -1
    echo "exit: ${PIPESTATUS[0]}"
}
run() {
    echo "--- graphix --no-cache $*"
    timeout -s KILL 30 "$GRAPHIX" --no-cache "$@" 2>&1 | tail -1
}

echo "=== called.gx"
check called.gx
run "$dir/called.gx"
echo "=== leaked.gx"
check leaked.gx
check control.gx
run "$dir/leaked_exit.gx"
run --no-fusion "$dir/leaked_exit.gx"
echo "--- graphix-fuzz check leaked.gx"
timeout -s KILL 180 "$GRAPHIX_FUZZ" check "$dir/leaked.gx" 2>&1 \
    | grep -v 'could not send batch' | sed -n '1p;/DIVERGENCE\|AGREE/,$p'
