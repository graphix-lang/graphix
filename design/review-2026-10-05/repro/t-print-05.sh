#!/usr/bin/env bash
# t-print-05: Doc printing uses str::lines(): a doc ending in an empty
# `///` line, and a multi-line doc in a CRLF file, are refused by fmt.
#
# The parser (graphix-types/src/expr/parser/mod.rs doc_comment) joins the
# `///` lines with '\n', each line's text verbatim up to '\n' (a CRLF
# line keeps its '\r'). Doc's Display and PrettyDisplay
# (graphix-types/src/expr/print.rs:520 and :552) print doc.lines(), which
# drops a final empty line and strips the '\r' of every '\r\n'. The
# reprinted doc differs, SigItem/TraitMethod equality compares docs, and
# format_source refuses; its was/now texts go through the same lossy
# Display, so they read the same.
#
# command: GRAPHIX=/path/to/graphix bash design/review-2026-10-05/repro/t-print-05.sh
#
# expected: every case formats (exit 0).
# observed (HEAD c722befe, debug build):
#   1 lf, val doc ending in `///`: exit 1, "formatter bug: the formatted
#     text says something else / was /// first line / val f: fn(x: i64)
#     -> i64 / now /// first line / val f: fn(x: i64) -> i64" (was == now)
#   2 lf, trait method doc ending in `///`: exit 1, the same refusal
#   3 crlf, two-line doc: exit 1, the same refusal, was == now
#   controls: 4 the case-1 file without the empty `///`, 5 crlf with no
#   doc, 6 crlf with a one-line doc: all exit 0
set -u
G=${GRAPHIX:-graphix}
run() {
    echo "=== $1"
    printf "$2" | timeout -s KILL 60 "$G" fmt --interface
    echo "exit=$?"
}
run '1 lf, val doc ending in ///' \
    '/// first line\n///\nval f: fn(x: i64) -> i64;\n/// ok\nval g: i64\n'
run '2 lf, trait method doc ending in ///' \
    'trait Show {\n    /// show it\n    ///\n    val show: fn(self) -> string\n}\n'
run '3 crlf, two-line doc' \
    '/// first line\r\n/// second line\r\nval f: fn(x: i64) -> i64;\r\nval g: i64\r\n'
run '4 control: lf, no trailing empty ///' \
    '/// first line\nval f: fn(x: i64) -> i64;\n/// ok\nval g: i64\n'
run '5 control: crlf, no doc' \
    'val f: fn(x: i64) -> i64;\r\nval g: i64\r\n'
run '6 control: crlf, one-line doc' \
    '/// only line\r\nval f: fn(x: i64) -> i64;\r\nval g: i64\r\n'
