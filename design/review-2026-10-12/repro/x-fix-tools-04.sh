#!/bin/sh
# x-fix-tools-04: a REPL fed by a pipe stops reading its input at the first
# line that has a value: InputReader::read_line (graphix-shell/src/input.rs:92)
# waits only for Ctrl-C while the output is Output::Text, so the later lines
# are never compiled and EOF never ends the shell. The input reader reads a
# non-terminal stdin line by line (f51d8964), but nothing after the first
# value is read.
#
# command: sh design/review-2026-10-12/repro/x-fix-tools-04.sh <graphix binary>
# expected: "-: i64", 2, "-: i64", 3, then the shell exits at EOF
# observed: "-: i64", 2, then nothing until the timeout kills it (exit 137)
G=${1:-graphix}
printf '1 + 1\n3\n' | timeout -s KILL 20 $G --no-cache -n
echo "exit $?"
