#!/usr/bin/env bash
# t-parser-b-07: in a .gxi, a `//` line below an item's `///` docs fails
# with a bare "Unexpected ` `", and a `///` line in a .gxi can earn the note
# that `///` is legal only in a .gxi.
#
# sig_item and trait_method read `//` lines, then `///` lines
# (parser/modexp.rs:32-33, traitexp.rs:42-43). doc_comment's
# `string("///")` (parser/mod.rs:376) is not under attempt: on `// comment`
# it matches `//`, commits, and fails at the space, with no reason noted.
# comment_line (parser/mod.rs:238-258) notes "`///` is a doc comment, legal
# only in a .gxi interface file" whenever leading_comments meets a `///`
# line, in a .gxi too, and the note is shown for a failure on that line.
#
# command: GRAPHIX=/path/to/graphix bash design/review-2026-10-05/repro/t-parser-b-07.sh
#
# expected: case 1 checks (Rust accepts `//` between `///` and its item), or
#   fails naming the order rule; case 2 fails without a note that contradicts
#   the file being an interface.
# observed (HEAD c722befe, debug build):
#   1. Parse error at line: 3, column: 1 / Unexpected ` ` (no reason)
#      (control 1b, `//` above `///`, checks)
#   2. Parse error at line: 2, column: 17 / Unexpected end of input ...
#      note: at line: 2, column: 1: `///` is a doc comment, legal only in a
#      .gxi interface file; a .gx file comments with `//`
set -u
GRAPHIX=${GRAPHIX:-graphix}
dir=$(mktemp -d)
trap 'rm -rf "$dir"' EXIT
export XDG_CACHE_HOME="$dir/cache"
run() {
    echo "== $1"
    mkdir -p "$dir/$1"
    printf 'mod m;\nm::x\n' > "$dir/$1/main.gx"
    printf 'let x = 1;\nlet y = 2\n' > "$dir/$1/m.gx"
    printf "$2" > "$dir/$1/m.gxi"
    timeout -s KILL 30 "$GRAPHIX" --check "$dir/$1/main.gx" 2>&1 | grep -E 'Parse error|Unexpected|note'
    echo "exit=${PIPESTATUS[0]}"
}
run 1_comment_below_doc 'val x: i64;\n/// doc\n// comment\nval y: i64\n'
run 1b_comment_above_doc 'val x: i64;\n// comment\n/// doc\nval y: i64\n'
run 2_dangling_doc 'val x: i64;\n/// dangling doc'
