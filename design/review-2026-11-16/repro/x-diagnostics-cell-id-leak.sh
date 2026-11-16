#!/bin/bash
# Title: refusals print generated type-variable cells by their raw ids ('_8144: unbound)
# Command: bash design/review-2026-11-16/repro/x-diagnostics-cell-id-leak.sh
# Expected: an open cell no binding names prints as the select refusals print it ('_1, '_2 in
#   the order the message meets them, Holders::number) or by the binding it is the type of ('r).
# Observed (one line per program):
#   the operand of ? has type '_8144: unbound, not known here: annotate it ...
#   the operand of $ has type '_8144: unbound, not known here: ...
#   f is fn(x: '_8144: unbound) -> '_8144: unbound and cannot hold i64
#   type mismatch string does not contain fn(x: '_8144: unbound) -> '_8144: unbound
#   cannot compare Array<'_8144: unbound> with i64: ...
#   The id is the global TVarId counter: it moves with everything compiled before (the stdlib),
#   so the text changes between builds and the user cannot find the name in their program.
G=${GRAPHIX:-/home/eric/tmp/review-2026-11-16/bin/graphix}
d=$(mktemp -d)
printf 'let f = |r| r?;\nf(1)\n' > $d/a.gx
printf 'let f = |r| r$;\nf(1)\n' > $d/b.gx
printf 'let f = |x| x;\nf <- 2;\nf\n' > $d/c.gx
printf 'let f = |x| x;\nlet y: string = f;\ny\n' > $d/d.gx
printf 'let e = [];\ne == 1\n' > $d/e.gx
for f in a b c d e; do timeout -s KILL 60 $G --check $d/$f.gx 2>&1 | tail -1; done
rm -rf $d
