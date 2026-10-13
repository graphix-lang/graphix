#!/bin/sh
# x-fix-fuzz-tests-01: `graphix-fuzz check|run|minimize <file>` hands the file's
# leading `//` comment header to Subject::parse, which wraps it after
# `let result = ` -- a parse error in both engines, so `check` says
# "AGREE (reject)". 532 of 547 findings/ pins and every recorded divergence
# file start with such a header; only regress strips it (build.rs strip_header).
BIN=${BIN:-graphix-fuzz}
T=$(mktemp -d)
printf '// a note, as every findings/ pin starts\n{ let x = 20; x + 22 }\n' > $T/header.gx
printf '{ let x = 20; x + 22 }\n' > $T/plain.gx
timeout -s KILL 120 $BIN run $T/header.gx 2>&1 | head -1   # CompileErr(... Unexpected `/`)
timeout -s KILL 120 $BIN run $T/plain.gx 2>&1 | head -1    # Trace([0:i64:42])
timeout -s KILL 175 $BIN check graphix-fuzz/findings/deep-tail-activation-budget-oct2026/01_tail_loop_past_the_activation_budget.gx 2>&1 | tail -1
# -> AGREE (reject); the stripped program is CONTAINED / UNSURE as the manifest records
rm -rf $T
