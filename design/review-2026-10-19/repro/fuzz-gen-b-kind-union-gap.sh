#!/bin/sh
# Title: the generators never type-test a composite union member
# Command: sh fuzz-gen-b-kind-union-gap.sh
# Expected: some `Array<..> as`, `List<..> as`, `(..) as`, `{..} as` arms
#   (the kind-marker surface) among generated programs.
# Observed (HEAD 9445a1d21): every type test is a scalar, null, error or
#   duration test; the composite forms count is 0 over 2000 sync + 4000
#   reactive programs.
B=/home/eric/tmp/review-2026-10-19/bin/graphix-fuzz
{ timeout -s KILL 60 $B gen 2000 9; timeout -s KILL 60 $B gen 4000 21 --reactive; } 2>/dev/null \
  | grep -oE "(Array|List|Map)<[^ ]*> as |\) as |\} as |[a-z0-9]+ as [a-z_0-9]+ =>" \
  | sed -E 's/v[0-9]+/vN/g' | sort | uniq -c | sort -rn
