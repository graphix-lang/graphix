#!/bin/sh
# Title: lang/fusion.rs's run-time-bind fixtures (bound_function_value_rebinds,
#   bound_definitions_raise_to_their_slots_handler, ...) say the bound
#   lambdas run in kernels, but FuseExpect counts only what the BUILD fused,
#   so nothing they check sees whether a bind fused. `#[native]` in the
#   lambda bodies (as first_cycle_bind_is_not_the_builds does) pins it: a
#   bind that fails it leaves the call bottom and the fixture times out.
# Command: sh tests-fusion-bound-defs-unpinned.sh
# Expected/observed (HEAD e1be2fbf2): both programs, the fixtures with
#   #[native] added to the bound lambdas, run and print the fixtures' values:
#   [100, 10, 2, 30, 104, 50, 6, 70] and [20, 18, 20, 18, 20].
G=${G:-/home/eric/tmp/review-2026-11-16/bin/graphix}
D=$(mktemp -d)
cat > $D/slots.gx <<'EOF'
let r = array::map(array::init(8, |i| i), |k| {
    let res = -1;
    let h = select k % 2 {
        0 => |x: i64| -> i64 #[native] select x % 4 { 0 => error(`Bad(x))?, _ => x },
        _ => |x: i64| -> i64 #[native] (x * 10)
    };
    let v = { catch(e) res <- select e.0.error { `Bad(b) => b + 100 }; h(k) };
    res <- v;
    res
});
filter(r, |a| array::len(array::filter(a, |x| x == -1)) == 0)
EOF
cat > $D/rebind.gx <<'EOF'
let fs = [|x: i64| -> i64 #[native] (x * 2), |x: i64| -> i64 #[native] (x + 7)];
let i = array::iter([0, 1, 0, 1, 0]);
let f = fs[i]$;
array::group(f(i + 10), |n, _| n == 5)
EOF
for f in slots rebind; do
  echo "== $f"
  timeout -s KILL 10 $G --no-cache $D/$f.gx 2>&1 | head -3
done
rm -rf $D
