#!/bin/sh
# A UTF-8 byte-order mark at the start of a file is a parse error, reported
# at an invisible character: "Unexpected `<U+FEFF>`".
# command: sh design/review-2026-10-19/repro/t-parser-a-bom.sh
# expected: the program checks (the mark skipped, as the `#!` line is)
# observed: Parse error at line: 1, column: 1, Unexpected `﻿` and an expected-token list
G=${G:-/home/eric/tmp/review-2026-10-19/bin/graphix}
D=$(mktemp -d)
printf '\357\273\277let x = 1;\nx\n' > $D/bom.gx
timeout -s KILL 30 $G --check $D/bom.gx 2>&1 | head -6
rm -rf $D
