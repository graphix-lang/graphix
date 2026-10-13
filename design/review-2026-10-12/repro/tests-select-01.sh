#!/bin/sh
# tests-select-01: graphix-fuzz: every `// expect: reject` corpus pin is refused at parse,
# so the refusal pins in regress are vacuous.
#
# build.rs::strip_header keeps `// expect: reject` as the first line of an embedded
# corpus program; Subject::parse wraps the text as `use super::*; let result = <text>`,
# so the marker lands mid-line and the parser refuses it ("a comment stands on a line
# of its own"). Any program under the marker agrees as `reject`.
#
# Run: sh design/review-2026-10-12/repro/tests-select-01.sh <path-to-graphix-fuzz>
# Expected: the well-typed program under the marker gives AGREE (trace) like without it
#           (or, for a refusal pin, the type checker's refusal).
# Observed: AGREE (reject); `run` shows a parse error at line 1, column 28 in every mode.
FUZZ=${1:-graphix-fuzz}
D=$(mktemp -d)
printf '{ let x = 1; x + 1 }\n' > "$D/plain.gx"
printf '// expect: reject\n{ let x = 1; x + 1 }\n' > "$D/marked.gx"
echo "== without marker"; timeout -s KILL 180 "$FUZZ" check "$D/plain.gx"
echo "== with marker";    timeout -s KILL 180 "$FUZZ" check "$D/marked.gx"
echo "== with marker, first mode's outcome"
timeout -s KILL 180 "$FUZZ" run "$D/marked.gx" 2>&1 | head -1 | cut -c1-200
rm -rf "$D"
