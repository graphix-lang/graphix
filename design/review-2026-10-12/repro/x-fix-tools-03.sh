#!/bin/sh
# x-fix-tools-03: piped REPL stops reading input at the first line that has a value; EOF never ends it
# Usage: sh x-fix-tools-03.sh [path/to/graphix]
# Expected: "-: i64", 2, then line 2 (sys::exit(0)) compiles and the shell exits 0
#           (or, without the exit, EOF ends the shell).
# Observed: "-: i64" and 2, then nothing: line 2 is never read, EOF never seen;
#           timeout kills the shell after 20 s (exit 137).
# Control:  printf 'let x = 1\nlet y = x\n' | graphix --no-cache -n  exits 0 at once
#           (no line has a value, so the output stays Output::None and stdin is read).
B=${1:-graphix}
printf '1 + 1\nsys::exit(0)\n' | timeout -s KILL 20 "$B" --no-cache -n
echo "exit $?"
