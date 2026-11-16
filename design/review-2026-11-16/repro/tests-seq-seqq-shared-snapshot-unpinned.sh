#!/bin/sh
# Title: nothing pins that seqq requests waiting for a capture share its first complete snapshot.
# Command: sh design/review-2026-11-16/repro/tests-seq-seqq-shared-snapshot-unpinned.sh
# seqq.rs delayed_capture_keeps_each_request never changes `capture` after it arrives, so it
# passes whether a waiting request keeps the first complete snapshot or reads the capture when
# it is dequeued. Here capture is 22 from step 7 and 33 from step 9, while requests 2 and 3 are
# still queued behind run 1 (its after_idle answers only once the clock stops); request 10
# arrives after the 33.
# Expected (seq_blocks.md section 8, book core/seq.md): (1, 22) (2, 22) (3, 22) (10, 33).
# Observed: the same, i.e. the rule holds today but no test would see it break.
G=${G:-/home/eric/tmp/review-2026-11-16/bin/graphix}
D=$(mktemp -d)
cat > $D/p.gx <<'EOF'
let step = 0;
step <- select step { n if n < 15 => n + 1, _ => never() };
let request = select step { 1 | 2 | 3 | 10 => step, _ => never() };
let capture = never<i64>();
capture <- select step { 6 => 22, 8 => 33, _ => never() };
let r = seqq let value = request { let w = sys::time::after_idle(duration:20.ms, value); (w, capture) };
sys::exit(sys::time::after_idle(duration:300.ms, 0));
r
EOF
timeout -s KILL 30 $G --no-cache $D/p.gx 2>&1 | tail -4
rm -rf $D
