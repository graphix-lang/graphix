#!/usr/bin/env bash
# t-parser-b-08: on CRLF input every `//` comment and `///` doc line keeps
# its `\r`, so graphix fmt writes mixed line endings.
#
# line_text() is many(none_of(['\n'])) (graphix-types/src/expr/parser/
# mod.rs:227): the `\r` before each `\n` becomes the last char of the
# comment's (or doc's) text, and the printer writes `//{text}` + `\n`. Code
# lines come out LF, comment and doc lines CRLF; the Doc an LSP hover shows
# ends in `\r`.
#
# command: GRAPHIX=/path/to/graphix bash design/review-2026-10-05/repro/t-parser-b-08.sh
#
# expected: one line ending throughout (the comment text holds no `\r`).
# observed (HEAD c722befe, debug build), od -c of the output:
#   1. `/ /   f i r s t \r \n l e t   x   =   1 ; \n \n / /   s e c o n d \r \n x \n`
#   2. `v a l   x :   i 6 4 ; \n \n / / /   d o c   l i n e \r \n v a l   y :   i 6 4 \n`
set -u
GRAPHIX=${GRAPHIX:-graphix}
dir=$(mktemp -d)
trap 'rm -rf "$dir"' EXIT
export XDG_CACHE_HOME="$dir/cache"
echo "== 1 .gx"
printf '// first\r\nlet x = 1;\r\n// second\r\nx\r\n' > "$dir/crlf.gx"
timeout -s KILL 30 "$GRAPHIX" fmt --stdout "$dir/crlf.gx" | od -c
echo "== 2 .gxi"
printf 'val x: i64;\r\n/// doc line\r\nval y: i64\r\n' | timeout -s KILL 30 "$GRAPHIX" fmt --interface | od -c
