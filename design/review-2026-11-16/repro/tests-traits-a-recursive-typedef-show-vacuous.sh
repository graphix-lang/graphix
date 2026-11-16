#!/bin/sh
# Title: trait_over_recursive_typedef_missing_impl pins refused("Show"), which
#   every refusal of its program matches through the context frame that quotes
#   the program's text ("in: { trait Show { val show: ..").
# Command: sh design/review-2026-11-16/repro/tests-traits-a-recursive-typedef-show-vacuous.sh
# Expected: only the fixture's intended refusal ("bool does not implement Show")
#   carries the phrase.
# Observed: the same program refused for an unrelated reason ("Json does not
#   contain f64") also prints "Show" in its error chain, in the frames quoting
#   the block.
G=${G:-/home/eric/tmp/review-2026-11-16/bin/graphix}
d=$(mktemp -d)
cat > $d/fixture.gx <<'GX'
let result = {
    trait Show { val show: fn(self) -> string };
    impl Show for i64 { let show = |x| "i" };
    impl<'a: Show> Show for Array<'a> { let show = |a| "arr" };
    type Json = [i64, bool, Array<Json>];
    let j: Json = 1;
    Show::show(j)
};
result
GX
sed 's/let j: Json = 1;/let j: Json = 1.5;/' $d/fixture.gx > $d/unrelated.gx
for f in fixture unrelated; do
  echo "== $f"
  timeout -s KILL 30 $G --no-cache --check $d/$f.gx 2>&1 | grep -n "Show\|contain"
done
rm -rf $d
