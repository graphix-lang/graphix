#!/bin/sh
# Title: "conflicting implementation ... at line L, column C" names a position
#        with no file: it reads as a place in the file the error is reported in
#
# Command: sh design/review-2026-11-16/repro/t-env-conflicting-impl-other-file.sh
#
# Env::register_impl prints the earlier impl's `pos` and not its `ori`. Here
# the earlier impl is at a.gx 3:1 and the refused one at b.gx 1:1.
#
# Expected: the refusal names a.gx (as Env::later_block_type_note does for a
#   declaration in another source: "declared at <pos> in <source>").
# Observed:
#   0: in file .../b.gx
#   1: at: line: 1, column: 1 in file .../b.gx, in: impl super::T for i64 ..
#   2: conflicting implementation: T is already implemented for i64 at line: 3, column: 1
#   b.gx has one line: nothing in the message says the other impl is in a.gx.
G=${GRAPHIX:-/home/eric/tmp/review-2026-11-16/bin/graphix}
D=$(mktemp -d "${TMPDIR:-/tmp}/t-env.XXXXXX")
trap 'rm -rf "$D"' EXIT
cd "$D" || exit 1
printf 'let pad = 0;\nlet pad2 = 0;\nimpl super::T for i64 { let m = |s| s + 1 }\n' > a.gx
printf 'impl super::T for i64 { let m = |s| s + 2 }\n' > b.gx
printf 'trait T { val m: fn(self) -> i64 };\nmod a;\nmod b;\nT::m(1)\n' > main.gx
timeout -s KILL 60 "$G" --check main.gx 2>&1 | tail -4
