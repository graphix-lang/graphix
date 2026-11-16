#!/bin/sh
# t-seq: `live(..)` in an abort clause or a nested seq's trigger is refused as "outside a seq block"
# Run: sh design/review-2026-11-16/repro/t-seq-live-in-clause-refused.sh /home/eric/tmp/review-2026-11-16/bin/graphix
# Expected: either accepted (the abort clause and a nested trigger read live
# already, so live(..) there is redundant) or refused naming the clause.
# Observed, both: "`live` reads a variable live inside a seq step; it means
# nothing outside a seq block", though each `live` is written inside a seq,
# the second inside a step: strip_live stops at a nested Seq and is never
# applied to the abort/flush clauses.
GX=${1:-/home/eric/tmp/review-2026-11-16/bin/graphix}
D=$(mktemp -d)
printf 'let t = 1;\nlet x = 2;\nseq t; abort(live(x)) { t }\n' > "$D/abort.gx"
printf 'let t = 1;\nlet x = 2;\nseq t { let a = t; seq live(x) { a } }\n' > "$D/nested.gx"
for f in abort nested; do
  timeout -s KILL 30 "$GX" --check "$D/$f.gx" 2>&1 | tail -1
done
rm -rf "$D"
