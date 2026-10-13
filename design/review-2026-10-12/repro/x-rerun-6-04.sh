#!/usr/bin/env bash
# x-rerun-6-04: `graphix fmt` on a CRLF file changes a quoted string
# literal that spans a line: format_source (graphix-types/src/expr/format.rs:499)
# replaces every CRLF with LF before parsing, so "q<CR><LF>z" (4 chars)
# parses as "q<LF>z", prints as "q\nz" (3 chars), and the reparse guard,
# which compares the LF text with its own output, cannot see it.
# command: GRAPHIX=/path/to/graphix bash x-rerun-6-04.sh
# expected: fmt keeps the value (4 before and after) or refuses
# observed (HEAD 7324207f): before 4, fmt exit 0 writing `let b = "q\nz";`, after 3
set -u
G=${GRAPHIX:-graphix}
dir=$(mktemp -d); trap 'rm -rf "$dir"' EXIT
export XDG_CACHE_HOME="$dir/cache"
printf 'let b = "q\r\nz";\r\nprintln(str::len(b));\r\nsys::exit(sys::time::after_idle(duration:100.ms, 0))\r\n' > "$dir/p.gx"
echo "before: $(timeout -s KILL 30 "$G" --no-cache "$dir/p.gx" 2>&1 | tail -1)"
timeout -s KILL 30 "$G" fmt "$dir/p.gx"; echo "fmt exit $?"
head -1 "$dir/p.gx" | od -c | head -2
echo "after: $(timeout -s KILL 30 "$G" --no-cache "$dir/p.gx" 2>&1 | tail -1)"
