#!/bin/bash
# t-format-resolver: a `#!` line is stripped only when RootFile::load reads a
# root file; the same file loaded as a module (`mod tool;`) fails to parse.
#
# Run: bash design/review-2026-11-16/repro/t-format-resolver-module-shebang.sh
# Expected: tool.gx, which `graphix --check tool.gx` accepts and `graphix fmt`
#   formats with its `#!` line kept, also loads as a module: main.gx checks.
# Observed (e1be2fbf2): tool.gx checks (rc=0); main.gx fails, rc=1:
#   "Parse error at line: 1, column: 1 Unexpected `#`" in tool.gx.
set -u
G=${G:-/home/eric/tmp/review-2026-11-16/bin/graphix}
D=$(mktemp -d)
trap 'rm -rf "$D"' EXIT
printf '#!/usr/bin/env graphix\nlet x = 1;\nx\n' > "$D/tool.gx"
printf 'mod tool;\ntool::x\n' > "$D/main.gx"
echo "== tool.gx as the root"
timeout -s KILL 30 $G --no-cache -n -i --check "$D/tool.gx"; echo "rc=$?"
echo "== tool.gx as a module of main.gx"
timeout -s KILL 30 $G --no-cache -n -i --check "$D/main.gx" 2>&1 | grep -m2 -E "Parse error|Unexpected"; echo "rc=${PIPESTATUS[0]}"
echo "== fmt keeps the line"
timeout -s KILL 30 $G fmt --stdout "$D/tool.gx" | head -1
