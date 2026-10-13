#!/bin/bash
# x-rerun-3-01: a fuzz subject whose first line is a comment never parses,
# so every `// expect: reject` corpus pin is rejected by the parser.
#
# Subject::parse wraps the body as `use super::*; let result = <body>`, so a
# body whose first line is `// ...` becomes `let result = // ...`, refused by
# the own-line comment rule. build.rs strip_header keeps `// expect: reject`
# as the first line of every such pin (82 under graphix-fuzz/findings).
#
# Run: bash design/review-2026-10-12/repro/x-rerun-3-01.sh <graphix-fuzz binary>
# Expected: 01 (a valid program under the marker) is NOT an agreed rejection
#           (regress should flag that the pin no longer refuses); 02 runs, value 3.
# Observed: 01 and 02 both print "AGREE (reject)"; `run` shows the parse error
#           "use super::*; let result = // a note ... a comment stands on a line
#           of its own". 03 (no comment) prints "AGREE (trace)".
FUZZ=${1:-graphix-fuzz}
D=$(mktemp -d)
printf '// expect: reject\n{ let x = i64:1; x + i64:2 }\n' > $D/01.gx
printf '// a note\n{ let x = i64:1; x + i64:2 }\n' > $D/02.gx
printf '{ let x = i64:1; x + i64:2 }\n' > $D/03.gx
for f in 01 02 03; do echo "== $f"; timeout -s KILL 180 $FUZZ check $D/$f.gx 2>&1 | tail -1; done
echo "== run 02"; timeout -s KILL 180 $FUZZ run $D/02.gx 2>&1 | head -1 | cut -c1-500
rm -rf $D
