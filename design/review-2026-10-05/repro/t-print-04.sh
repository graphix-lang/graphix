#!/usr/bin/env bash
# t-print-04: CRLF sources: multi-line raw strings are refused, comments
# keep \r (mixed endings), multi-line `///` docs are refused.
#
# The parser keeps every \r of a CRLF file: a raw string reads it
# verbatim (parser/mod.rs raw_string) and a comment or doc line keeps it
# (line_text is `many(none_of(['\n']))`). In as-written mode the printer
# writes a raw string raw only when raw_writable (print.rs:1893) passes,
# and that refuses \r, so the literal prints quoted and the delimiter
# guard refuses the file. Comment lines print `//{text}` + \n, so they
# keep \r\n while every other line gets \n. A doc prints through
# str::lines, which drops the \r of every line but the last, so the
# reparse differs and the file is refused. The CRLF inputs are generated
# here: .gitattributes (`* text eol=lf`) would rewrite a committed one.
#
# command: GRAPHIX=/path/to/graphix bash design/review-2026-10-05/repro/t-print-04.sh
#
# expected: all three format (the raw string stays raw, the doc stays
# whole), and the output uses one line-ending style throughout.
#
# observed (HEAD c722befe, debug build):
#   case 1: exit 1, `formatter bug: the formatted text changed a
#           string's delimiters` (the same file with LF line endings
#           formats; so do book/src/examples/gui/svg.gx and markdown.gx
#           with LF, and both are refused once converted to CRLF)
#   case 2: exit 0, bytes `// first\r\nlet x = 1;\n\n// second\r\nx\n`
#   case 3: exit 1, `formatter bug: the formatted text says something else`
set -u
GRAPHIX=${GRAPHIX:-graphix}
dir=$(mktemp -d)
trap 'rm -rf "$dir"' EXIT

echo "== case 1: a multi-line raw string in a CRLF file"
printf 'let a = r"line one\r\nline two";\r\na\r\n' > "$dir/raw.gx"
timeout -s KILL 60 "$GRAPHIX" fmt --stdout "$dir/raw.gx"
echo "exit=$?"
echo "   (control: the same file with LF endings)"
printf 'let a = r"line one\nline two";\na\n' > "$dir/raw_lf.gx"
timeout -s KILL 60 "$GRAPHIX" fmt --stdout "$dir/raw_lf.gx" > /dev/null
echo "exit=$?"

echo "== case 2: comments in a CRLF file (formatted bytes)"
printf '// first\r\nlet x = 1;\r\n// second\r\nx\r\n' > "$dir/comments.gx"
timeout -s KILL 60 "$GRAPHIX" fmt --stdout "$dir/comments.gx" > "$dir/out.gx"
echo "exit=$?"
od -c "$dir/out.gx"

echo "== case 3: a two-line doc in a CRLF interface"
printf '/// the v\r\n/// second line\r\nval v: i64;\r\nval w: i64\r\n' > "$dir/docs.gxi"
timeout -s KILL 60 "$GRAPHIX" fmt --stdout "$dir/docs.gxi" > /dev/null
echo "exit=$?"
