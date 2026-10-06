#!/usr/bin/env bash
# t-image-01: expression identity key ignores decorations: a nested
# #[serial]/#[parallel] definition loses its fork control after a warm start.
#
# image::expr_key (graphix-types/src/image/mod.rs:1294) shares one image
# object between expressions Expr::same_tree (graphix-types/src/expr/mod.rs:1318)
# calls equal: same id, pos, origin and `kind ==`, where Expr equality is
# kind-only, so `dec` (and every child's id/dec) is ignored, though the codec
# writes `dec`. compiler::fork_on_body (graphix-compiler/src/node/compiler.rs:101)
# moves #[serial]/#[parallel] from a `let` onto a same-id clone of the lambda
# body. When the `let` sits inside another function, that function's
# definition (lower LambdaId, encoded first) writes the undecorated body, and
# the inner definition's decorated body becomes a reference to it: after a
# warm start every instance bound at run time (collection slots, dynamic
# calls) compiles from a body with no ForkControl, and a slot no longer
# mirrors its imaged prototype's fusion walk, so it loses the shared kernels.
#
# command: GRAPHIX=/path/to/graphix bash design/review-2026-10-05/repro/t-image-01.sh
#
# expected: each warm run behaves as the cold run and --no-cache: 0 inversions
# (a slot's bN never before its aN), a few instance-fusion lines, 2 forced
# kernel loops.
#
# observed (HEAD c722befe, debug build, default GRAPHIX_PAR=auto unless noted):
#   serial:   cold 0, warm 32 / 18 / 18 inversions, --no-cache 0 0
#             (GRAPHIX_PAR=force, the smaller program in the report: warm ~30)
#   defuse:   instance-fusion lines cold 24, warm 6027 6027 6027 (each
#             callback of the 2000-slot init/fold node-walks per slot); a warm
#             run takes 3x as long. With 200000 instead of 2000, `let f =
#             |x: i64| array::fold(array::init(200000, |i| i + x), 0, |acc, v|
#             acc + v); array::map([1, 2, 3], f)` under `#[serial]` inside a
#             function runs cold in 0.5 s and the warm start exceeds a 6 GB
#             memory cap after 30 s; without the attribute, or with it
#             written on the body, warm runs in 0.25 s.
#   parallel: forced kernel loops cold 2, warm 0 0
#   image:    the nested program's image holds one Decorations naming
#             `serial` (the `let`'s); the top-level form's holds two (the
#             `let`'s and the moved body's): the moved one was never written.
set -u
GRAPHIX=${GRAPHIX:-graphix}
dir=$(mktemp -d)
trap 'rm -rf "$dir"' EXIT
cd "$dir"
inversions() {
    awk '/^[ab][0-9]+$/ { k = substr($0, 2); if (substr($0, 1, 1) == "a") a[k]++;
         else { if (a[k] <= b[k]) inv++; b[k]++ } } END { print inv + 0 }'
}

cat > serial.gx <<'EOF'
let outer = |xs: Array<i64>| {
    #[serial]
    let f = |x: i64| (
        println(array::fold(array::init(2000, |i| i + x), 0, |acc, v| acc + v) ~ "a[x]"),
        println(array::fold(array::init(2000, |i| i * 2 + x), 0, |acc, v| acc + v) ~ "b[x]")
    );
    array::map(xs, f)
};
let clock = sys::time::timer(duration:20.ms, true);
let n = 0;
n <- clock ~ n + 1;
let xs = [0, 1000, 2000];
xs <- clock ~ array::map(xs, |v| v + 1);
sys::exit(select n { k if k >= 30 => sys::time::after_idle(duration:100.ms, 0), _ => never() });
array::len(outer(xs))
EOF

cat > parallel.gx <<'EOF'
let outer = |k: i64| {
    #[parallel]
    let f = |xs: Array<i64>| array::map(xs, |x| x * 2 + k);
    array::map([array::init(40, |i| i), array::init(40, |i| i * 3)], f)
};
let r = outer(1);
sys::exit(sys::time::after_idle(duration:300.ms, r ~ 0));
array::len(r)
EOF

echo "=== serial: slots whose bN printed before aN"
export XDG_CACHE_HOME="$dir/cache-serial"
for run in cold warm warm warm; do
    GXDBG_INSTANCE_FUSION=1 timeout -s KILL 90 "$GRAPHIX" serial.gx > out.txt 2> err.txt
    echo "$run: inversions $(inversions < out.txt), instance-fusion lines $(grep -c INSTANCE-FUSION err.txt)"
done
for run in 1 2; do
    timeout -s KILL 90 "$GRAPHIX" --no-cache serial.gx > out.txt 2> /dev/null
    echo "--no-cache: inversions $(inversions < out.txt)"
done

echo "=== parallel: kernel loops forked by #[parallel]"
export XDG_CACHE_HOME="$dir/cache-parallel"
for run in cold warm warm; do
    GRAPHIX_DBG_PAR=1 timeout -s KILL 60 "$GRAPHIX" parallel.gx > /dev/null 2> err.txt
    echo "$run: $(grep -c 'PAR kernel loop' err.txt)"
done
