#!/bin/sh
# x-fix-fuzz-tests-02: every `// expect: reject` corpus pin is vacuous.
# build.rs strip_header re-attaches `// expect: reject` ABOVE the program;
# Subject::parse then wraps the text as `use super::*; let result = <text>`,
# so the marker lands after `=` on the same line -- a parse error in every
# engine since comments must stand on their own line. regress sees
# CompileErr == CompileErr ("reject", as outcome.manifest records) for all
# 83 marked pins whatever the program does; the check pair's check_only
# fails to parse too. A valid program marked reject "rejects":
BIN=${BIN:-graphix-fuzz}
T=$(mktemp -d)
printf '// expect: reject\n{ let x = 20; x + 22 }\n' > $T/valid_marked.gx
timeout -s KILL 120 $BIN run $T/valid_marked.gx 2>&1 | head -1   # CompileErr(... Unexpected `/`)
# Two marked pins are not refusals at all (their own notes say so):
for p in abort-seam-jul2026/01_depth_trip_settled_taint.gx \
         tvar-lock-jul2026/01_constraint_eq_self_deadlock.gx; do
  grep -v '^\s*//' graphix-fuzz/findings/$p > $T/prog.gx
  timeout -s KILL 175 $BIN check $T/prog.gx 2>&1 | tail -1   # AGREE (contained) / AGREE (trace)
done
rm -rf $T
