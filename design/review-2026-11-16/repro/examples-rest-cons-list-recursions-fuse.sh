#!/usr/bin/env bash
# cons_list.gx and book/src/udt/collection.md say the value-building
# recursions (filter_map_l, append_l) are "an activation per element";
# under the JIT they compile to native code like fold_l does.
#
# Run: bash design/review-2026-11-16/repro/examples-rest-cons-list-recursions-fuse.sh [graphix]
# Expected (per the docs): the #[native] build of map/filter over the user
#   list is refused, and map over 5000 conses costs ~an activation each.
# Observed (debug binary at e1be2fbf2):
#   --expand with #[native] on map(l, ..) and len(filter(l, ..)): exit 0 (fuses)
#   map+fold over 5000 conses: JIT elapsed=0.0034 s, --no-fusion elapsed=1.65 s
set -u
B=${1:-/home/eric/tmp/review-2026-11-16/bin/graphix}
here="$(cd "$(dirname "$0")" && pwd)"
ex="$here/../../../book/src/examples/collection/cons_list.gx"
tmp=$(mktemp -d)
sed 's/let doubled = map(l, |x| x \* 2);/let doubled = #[native] map(l, |x| x * 2);/;
     s/let n = len(filter(l, |x| x > 1));/let n = #[native] len(filter(l, |x| x > 1));/' \
    "$ex" > "$tmp/native.gx"
timeout -s KILL 60 "$B" --no-cache --expand "$tmp/native.gx"; echo "expand exit $?"
sed -n '7,42p' "$ex" > "$tmp/perf.gx"
cat >> "$tmp/perf.gx" <<'EOF'
let rec mk = |n: i64, acc: L<i64>| -> L<i64> select n { 0 => acc, _ => mk(n - 1, `C(n, acc)) };
let l = #[native] mk(5000, `N);
let t0 = sys::time::now(l);
let seed = cast<i64>(t0)$ % 2;
let doubled = #[native] map(l, |x| x * 2 + seed);
let s = #[native] fold(doubled, 0, |a, x| a + x);
let t1 = sys::time::now(s);
println("s=[s] elapsed=[cast<f64>(t1)$ - cast<f64>(t0)$]");
sys::exit(s ~ 0)
EOF
echo -n "jit: ";       timeout -s KILL 60 "$B" --no-cache "$tmp/perf.gx" | tail -1
echo -n "node-walk: "; timeout -s KILL 120 "$B" --no-cache --no-fusion "$tmp/perf.gx" | tail -1
rm -rf "$tmp"
