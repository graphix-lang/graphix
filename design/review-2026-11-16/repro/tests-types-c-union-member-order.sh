#!/bin/bash
# Title: whether a typedef union of function types meets another "after a param"
# depends on how the two typedef applications spell their arguments, not on the
# union (a set of members): one structural type gets opposite verdicts.
# Command: bash design/review-2026-11-16/repro/tests-types-c-union-member-order.sh
# Expected: one verdict for all four F2/G2 spellings (k's x2 is the one union
#   [fn(q: 'c) -> i64, fn(q: 'd) -> i64] in each; x0 has paired 'c with 'b),
#   and the same verdict for the Array analog P/Q.
# Observed (HEAD e1be2fbf2):
#   F2<'c, 'd>  accepted        (pinned: union_of_functions_the_formals_spell_in_the_same_order_runs_after_a_param)
#   F2<'d, 'c>  refused         (pinned: union_of_functions_the_formals_spell_in_the_other_order_is_refused_after_a_param)
#               "fn(q: 'c: 'd: unbound) -> i64 could go to any of 2 members of the union fn(q: 'd: unbound) -> i64"
#   G2<'d, 'c>  accepted        (G2 is F2 with its members listed the other way: the same union as F2<'d, 'c>)
#   G2<'c, 'd>  refused
#   P/Q (arrays), every spelling: refused
G=${GRAPHIX:-/home/eric/tmp/review-2026-11-16/bin/graphix}
d=$(mktemp -d)
for k in "F2<'c, 'd>" "F2<'d, 'c>" "G2<'d, 'c>" "G2<'c, 'd>" "P<'c, 'd>" "Q<'d, 'c>"; do
  case "$k" in F*|G*) fk="F2<'b, 'a>";; *) fk="P<'b, 'a>";; esac
  cat > $d/p.gx <<GX
type F2<'p, 'q> = [fn(q: 'p) -> i64, fn(q: 'q) -> i64];
type G2<'p, 'q> = [fn(q: 'q) -> i64, fn(q: 'p) -> i64];
type P<'p, 'q> = [Array<'p>, Array<'q>];
type Q<'p, 'q> = [Array<'q>, Array<'p>];
let f = |x0: 'b, x2: $fk| 0;
let app = |k: fn(x0: 'c, x2: $k) -> i64, v: 'c, w: 'd| 0;
app(f, 1, "s")
GX
  printf '%-12s -> ' "$k"
  if timeout -s KILL 60 $G --check --no-cache $d/p.gx > $d/out 2>&1; then echo accepted; else echo "refused: $(tail -1 $d/out | cut -c1-150)"; fi
done
rm -rf $d
# Part 2: a select of f and a twin whose x2 is the same union.
# Observed: F2<'b, 'a> accepted (pinned: select_of_a_twin_keeps_the_variables_of_a_signature_union_apart);
#           F2<'a, 'b> refused (pinned: select_of_two_spellings_of_a_union_of_functions_is_refused_where_nothing_pairs_the_members,
#                      though x0 pairs the members);
#           G2<'a, 'b> refused -- its expansion is [fn(q: 'b) -> i64, fn(q: 'a) -> i64], F2<'b, 'a>'s exactly.
d=$(mktemp -d)
for tw in "F2<'b, 'a>" "F2<'a, 'b>" "G2<'a, 'b>"; do
  cat > $d/p.gx <<GX
type F2<'p, 'q> = [fn(q: 'p) -> i64, fn(q: 'q) -> i64];
type G2<'p, 'q> = [fn(q: 'q) -> i64, fn(q: 'p) -> i64];
let c = true;
let f = |x0: 'b, x2: F2<'b, 'a>| 0;
let g = select c { true => f, false => |x0: 'b, x2: $tw| 0 };
g(1, |q: string| 0)
GX
  printf 'twin %-12s -> ' "$tw"
  if timeout -s KILL 60 $G --check --no-cache $d/p.gx > $d/out 2>&1; then echo accepted; else echo "refused: $(tail -1 $d/out | cut -c1-150)"; fi
done
rm -rf $d
