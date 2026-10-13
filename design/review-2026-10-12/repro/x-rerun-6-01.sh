#!/usr/bin/env bash
# x-rerun-6-01: graphix-fuzz wraps a subject as `let result = {<body>`
# (graphix-fuzz/src/lib.rs Subject::parse), so a body whose first line is a
# `//` comment puts the comment on the `{` line, which the parser refuses
# since own-line comments (eb97e70d). build.rs re-attaches `// expect:
# reject` as the FIRST line of every such corpus pin, so all of them reject
# at parse, whatever the program: the 82 `// expect: reject` pins pin
# nothing, and `check`/`run` on any file with a leading comment report
# AGREE (reject).
#
# command: GRAPHIX_FUZZ=/path/to/graphix-fuzz bash x-rerun-6-01.sh
# expected: case 1 is a valid program marked expect-reject: the gate should
#   fail it (the program compiles); case 2 should AGREE (trace) like case 3.
# observed (HEAD 7324207f): 1 AGREE (reject); 2 AGREE (reject) with
#   "Parse error at line: 1, column: 29 Unexpected `/`"; 3 AGREE (trace).
set -u
F=${GRAPHIX_FUZZ:-graphix-fuzz}
dir=$(mktemp -d); trap 'rm -rf "$dir"' EXIT
printf '// expect: reject\nlet x = 1;\nx + 1\n' > "$dir/1.gx"
printf '// a header comment\nlet x = 1;\nx + 1\n' > "$dir/2.gx"
printf 'let x = 1;\nx + 1\n' > "$dir/3.gx"
for i in 1 2 3; do echo "$i: $(timeout -s KILL 170 "$F" check "$dir/$i.gx" 2>&1 | tail -1)"; done
timeout -s KILL 170 "$F" run "$dir/2.gx" 2>&1 | head -1 | cut -c1-250
