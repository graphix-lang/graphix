#!/usr/bin/env bash
# x-rerun-8-12 (extends x-rerun-8-02): every `// expect: reject` pin in the
# embedded regression corpus is vacuous. build.rs strip_header keeps the
# `// expect: reject` line above the program; Subject::parse
# (graphix-fuzz/src/lib.rs:379) then wraps it as
# `use super::*; let result = {// expect: reject` on one line, which the
# parser refuses (own-line comment rule), so the pin "rejects" whatever its
# program is: a pin whose refusal the checker stopped making still passes.
# This file feeds graphix-fuzz exactly what the corpus holds for such a pin,
# with a VALID program under the marker.
# command: FUZZ=/path/to/graphix-fuzz bash x-rerun-8-12.sh
# expected: the valid program is not a rejection (check reports that the
#   pin no longer rejects)
# observed: AGREE (reject); run shows CompileErr "a comment stands on a line
#   of its own" in every mode
set -u
F=${FUZZ:-graphix-fuzz}
d=$(mktemp -d); trap 'rm -rf "$d"' EXIT
printf '// expect: reject\nlet x = 1;\nx + 1\n' > "$d/pin.gx"
timeout -s KILL 170 "$F" check "$d/pin.gx" 2>&1 | head -1
timeout -s KILL 170 "$F" run "$d/pin.gx" 2>&1 | head -1 | cut -c1-300
