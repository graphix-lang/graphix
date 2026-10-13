#!/bin/sh
# f-call-flow-04: graphix-fuzz check/run reports AGREE (reject) for any
# program whose first line is a `//` comment (every saved pin, every
# divergence_*.gx the fuzzer writes).
# Run: sh design/review-2026-10-12/repro/f-call-flow-04.sh <graphix-fuzz binary>
# Expected: both checks print AGREE (trace).
# Observed: the commented copy prints AGREE (reject); `graphix-fuzz run` on it
#   shows CompileErr "Unexpected `/` ... a comment stands on a line of its own"
#   in every mode, at `use super::*; let result = // ...`.
FUZZ=${1:-graphix-fuzz}
D=$(mktemp -d)
printf '// bisect: x\n// minimized:\nlet x = 1;\nx + 1\n' > "$D/commented.gx"
printf 'let x = 1;\nx + 1\n' > "$D/bare.gx"
echo "commented:"; timeout -s KILL 180 "$FUZZ" check "$D/commented.gx"
echo "bare:";      timeout -s KILL 180 "$FUZZ" check "$D/bare.gx"
rm -rf "$D"
