#!/bin/sh
# x-fix-tools-01: `graphix fmt` on a CRLF file changes the value of a string
# literal that spans a line: format_source (graphix-types/src/expr/format.rs:499)
# turns every CRLF into LF before parsing, and the reparse guard compares
# against that LF text, not the input. The printer writes the newline as `\n`,
# so the literal's CR is gone for good.
#
# command: sh design/review-2026-10-12/repro/x-fix-tools-01.sh <graphix binary>
# expected: 4 and 4 (or fmt refuses the file)
# observed: 4 then 3
G=${1:-graphix}
D=$(mktemp -d)
printf 'sys::exit(sys::time::after_idle(duration:100.ms, 0));\r\nlet s = "a\r\nb";\r\nstr::len(s)\r\n' > $D/p.gx
timeout -s KILL 30 $G --no-cache $D/p.gx
timeout -s KILL 30 $G fmt $D/p.gx
timeout -s KILL 30 $G --no-cache $D/p.gx
rm -rf $D
