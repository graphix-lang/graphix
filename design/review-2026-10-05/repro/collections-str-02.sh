#!/bin/sh
# collections-str-02: an intransitive user Ord panics Rust's sort: the runtime
# dies, or under the JIT the whole process aborts.
#
# Run: GRAPHIX=/path/to/graphix sh design/review-2026-10-05/repro/collections-str-02.sh
#
# Both programs give an abstract type an antisymmetric but intransitive Ord
# (pairs of even numbers in natural order, any pair with an odd member
# reversed: 2 < 4 < 3 < 2) and order 21 of its values. sort.gx sorts them with
# array::sort (sort_values, stdlib/graphix-package-core/src/lib.rs); map.gx
# builds a map keyed by them with map::map (pairs_to_map, CMap::from_iter,
# whose insert_many sorts).
#
# Expected: the program survives (some order, or a logged error and bottom).
# Observed at c722befe (debug build), on every run:
#   sort.gx --no-fusion  panicked at core/src/slice/sort/shared/smallsort.rs:854:5:
#                        "user-provided comparison function does not correctly
#                        implement a total order", then "Error: runtime did not
#                        respond", exit 1
#   sort.gx (JIT)        the same, exit 1 (fast_dispatch catches the panic,
#                        FusedKernel::update resumes it)
#   map.gx --no-fusion   the same, exit 1
#   map.gx (JIT)         the same panic, then "fatal runtime error: failed to
#                        initiate panic, error 5, aborting", core dumped, exit 134
#                        (graphix_valarray_into_cmap has no catch_unwind and the
#                        unwind cannot cross the kernel's frames)
GRAPHIX=${GRAPHIX:-graphix}
d=$(mktemp -d)
trap 'rm -rf "$d"' EXIT
ord='type T = Abstract<i64>;
impl Ord for T {
  let cmp = |a, b| select (a.0 % 2 + b.0 % 2 == 0, a.0 < b.0, a.0 == b.0) {
    (_, _, true) => `Equal,
    (true, true, false) => `Less,
    (false, false, false) => `Less,
    (_, _, false) => `Greater
  }
};
let ns = [0, 887, 459, 734, 703, 366, 732, 792, 546, 1003, 145, 999, 538, 780, 716, 346, 679, 706, 427, 851, 969];
sys::exit(sys::time::after_idle(duration:100.ms, 0));'
printf '%s\n%s\n' "$ord" 'array::len(array::sort(array::map(ns, |n| T(n))))' > "$d/sort.gx"
printf '%s\n%s\n%s\n' "$ord" \
  'let src = array::fold(array::enumerate(ns), {}, |m, (i, n)| map::insert(m, i, n));' \
  'map::len(map::map(src, |(i, n)| (T(n), i)))' > "$d/map.gx"
for p in sort map; do
  for mode in --no-fusion ""; do
    timeout -s KILL 60 "$GRAPHIX" --no-cache $mode "$d/$p.gx"
    echo "$p.gx ${mode:-(JIT)}: exit $?"
  done
done
