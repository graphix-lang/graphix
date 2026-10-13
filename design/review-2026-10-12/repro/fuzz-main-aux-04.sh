#!/bin/sh
# fuzz-main-aux-04: soak-dash parses the counter line with stale field indices
#
# Run: sh design/review-2026-10-12/repro/fuzz-main-aux-04.sh
#
# Feeds the soak's progress line, in the exact format graphix-fuzz's
# campaign loop prints it (the eprintln! with "({} known)"), through the
# awk reduction in graphix-fuzz/soak-dash.gx's `remote` script (copied
# verbatim), then names the 9 fields the way soak-dash's
# `[k, c, src, runs, div, crash, corp, novel, cpu]` select arm binds them.
#
# Expected: runs=1000 div=3 crash=0 corp=7 novel=12 cpu=40%
# Observed: runs=1000 div=3 crash=(5 corp=0 novel=in cpu=novel
#   so lane_row's `f.crash != "0"` is always true (every lane red) and
#   host_row's corpus (`box_corpus`, read from f.corp) is the crash count.
c=camp
printf '  fuzz…1000 run, 3 divergences (5 known), 0 crashes, 7 in corpus, 12 novel shapes, 40%% cpu, 2%% individual\n' |
  awk -v c="$c" '
    / run, / { split($1, a, "…"); last[a[1]] = a[2] " " $3 " " $5 " " $7 " " $10 " " $13 }
    END { for (k in last) print "LANE " c " " k " " last[k] }' |
  while read k c src runs div crash corp novel cpu; do
    echo "fields=$(echo $k $c $src $runs $div $crash $corp $novel $cpu | wc -w)"
    echo "runs=$runs div=$div crash=$crash corp=$corp novel=$novel cpu=$cpu"
  done
