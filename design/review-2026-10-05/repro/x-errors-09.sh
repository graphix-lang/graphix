#!/usr/bin/env bash
# x-errors-09: report_failure! (graphix-compiler/src/node/error.rs:567)
# eprintln!s every unhandled `?` error and every unchecked-arith failure
# from the compiler library. In a TUI program stderr is the terminal
# ratatui draws on (alternate screen, raw mode): the message lands at the
# cursor, a message that runs past the bottom row scrolls the alternate
# screen, and ratatui's diff renderer repaints only the cells it changes,
# so one failure corrupts the display for the rest of the run. --log-dir
# does not help (the eprintln! is unconditional). analysis.rs:265-266
# repeats the same log::error! + eprintln! pair by hand.
#
# Each case runs the program below (a header block, a content block, a
# one-line footer "status: N" counting every 200ms; the failure fires
# ONCE, at n == 5) in a detached 80x24 tmux pane whose tty is the
# program's stdout and stderr, and prints the screen 4s in.
#
# command: GRAPHIX=/path/to/graphix bash design/review-2026-10-05/repro/x-errors-09.sh
#          (needs tmux; the program with FAIL = a[n + 5]? shows the same
#          when run by hand in a terminal)
#
# expected (every case, as the `$` control shows): rows 0-2 the "Header"
#   block, rows 3-22 the "Main Content" block, row 23 "status: N".
# observed (HEAD c722befe, debug build):
#   unhandled ? (the select's region fused): the alternate screen has
#     scrolled up 2 rows: "Header" is gone (row 0 is its bottom border),
#     Main Content's box sits at rows 1-20, rows 21-22 read
#     "status: 4unhandled error in file /tmp/tmp.*/unhandled/p.gx at line: 7,"
#     "column: 10 ["ArrayIndexError", "array index out of bounds"]", and
#     row 23 reads "        19": ratatui redraws only the changed digits.
#   ignored $ (control): the expected screen, "status: 19" on row 23.
#   arith /0 under --no-fusion: the same corruption, with
#     "arith error in file /tmp/tmp.*/arith/p.gx at line: 7, column: 10
#     "arithmetic error"".
#   The scroll is as many rows as the message wraps, so a longer path
#   scrolls more (3 rows, the whole Header block, from a deeper directory).
set -u
G=${GRAPHIX:-graphix}
D=$(mktemp -d)
S=xerr09-$$
trap 'tmux -L "$S" kill-server 2>/dev/null; rm -rf "$D" "${TMUX_TMPDIR:-/tmp}/tmux-$(id -u)/$S"' EXIT
prog() {
    sed "s|FAIL|$1|" <<'EOF'
use tui::{block::block, layout::{child, layout}, text::text};
let clock = sys::time::timer(duration:200.ms, true);
let n = 0;
n <- clock ~ n + 1;
let a = [1, 2, 3];
let once = select n {
    5 => FAIL,
    _ => 0
};
let header = block(#border: &`All, &text(&"Header"));
let content = block(#border: &`All, &text(&"Main Content"));
let footer = text(&"status: [n]");
layout(
    #direction: &`Vertical,
    &[
        child(#constraint: `Length(3), header),
        child(#constraint: `Fill(1), content),
        child(#constraint: `Length(1), footer)
    ]
)
EOF
}
run() {
    local name=$1 fail=$2 flags=$3
    mkdir -p "$D/$name"
    prog "$fail" > "$D/$name/p.gx"
    tmux -L "$S" -f /dev/null new-session -d -x 80 -y 24 -s "$name" \
        "env XDG_CACHE_HOME=$D/cache timeout -s KILL 30 $G --no-cache $flags $D/$name/p.gx"
    sleep 4
    echo "== $name: $fail $flags"
    tmux -L "$S" capture-pane -p -t "$name" | awk '{ printf "%2d |%s\n", NR - 1, $0 }'
    tmux -L "$S" kill-session -t "$name"
}
run unhandled 'a[n + 5]?' ''
run ignored 'a[n + 5]$' ''
run arith '100 / (n - n)' '--no-fusion'
