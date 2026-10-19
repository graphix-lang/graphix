#!/bin/sh
# A program's string values depend on the file's line endings: a string that
# spans lines keeps the `\r` of a CRLF file, so a checkout with
# core.autocrlf=true (Windows) changes what the program computes.
# command: sh design/review-2026-10-19/repro/t-parser-a-crlf-string-values.sh
# expected: 5 5 for both files (CRLF read as a line break, as rustc does)
# observed: LF file prints 5 5; CRLF file prints 6 6
G=${G:-/home/eric/tmp/review-2026-10-19/bin/graphix}
D=$(mktemp -d)
printf 'let x = """\nab\ncd""";\nlet y = "ef\ngh";\nprintln(str::len(x));\nprintln(str::len(y));\nsys::exit(sys::time::after_idle(duration:100.ms, 0))\n' > $D/lf.gx
sed 's/$/\r/' $D/lf.gx > $D/crlf.gx
echo "LF:";   timeout -s KILL 30 $G --no-cache $D/lf.gx
echo "CRLF:"; timeout -s KILL 30 $G --no-cache $D/crlf.gx
rm -rf $D
