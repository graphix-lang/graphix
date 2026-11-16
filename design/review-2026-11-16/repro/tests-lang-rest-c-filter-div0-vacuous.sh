#!/bin/sh
# lang::arrays::filter_div0_slot_cache cannot tell a bottoming predicate
# that poisons the collection from one that drops its element.
#
# Command: sh design/review-2026-11-16/repro/tests-lang-rest-c-filter-div0-vacuous.sh
# Expected (for the fixture to pin its rule): "poison" and "drop" differ.
# Observed: with the fixture's source [1, 2, 0, 4] both print
#   [[1, 2, 1], [1, 2, 2], [1, 2]], fused and node-walk alike; with
#   [1, 2, 0, 1] poison prints [[1, 2, 1], [1, 2, 2], [1, 2, 1]] and drop
#   [[1, 2, 1], [1, 2, 2], [1, 2]].
G=${G:-/home/eric/tmp/review-2026-11-16/bin/graphix}
d=$(mktemp -d)
for src in '[1, 2, 0, 4]' '[1, 2, 0, 1]'; do
  for kind in poison drop; do
    case $kind in
      poison) pred='10 / x > 2' ;;
      drop) pred='select x { 0 => false, _ => 10 / x > 2 }' ;;
    esac
    cat > "$d/p.gx" <<EOF
let result = {
  let a = [1, 2, array::iter($src), 4, 5, 6, 7, 8];
  let out = array::filter(a, |x| $pred);
  array::group(out, |n, _| n == 3)
};
sys::exit(sys::time::after_idle(duration:200.ms, 0));
result
EOF
    for flag in "" --no-fusion; do
      printf '%s %s %s: ' "$src" "$kind" "${flag:-fused}"
      timeout -s KILL 60 "$G" --no-cache $flag "$d/p.gx" 2>/dev/null | tail -1
    done
  done
done
rm -rf "$d"
