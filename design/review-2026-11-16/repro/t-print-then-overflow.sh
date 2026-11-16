#!/bin/sh
# t-print: layouts that write a line past the width because the text that
# follows a child on its line is not kept free (no with_tail, or a `<=` fit):
#  1. pretty_then: `e` + suffix fits with `<=` and keeps no column for the
#     `;`/`,` after it (fmt_flat keeps one, its newline; pretty_tail_bare and
#     pretty_lookup test with `<`): `x.b[cc]$;` laid out under its head.
#  2. a select guard: pretty_after(buf, "if", guard) then ` =>`, no tail.
#  3. a map key: Bare(k).fmt_pretty then ` =>`, no tail.
#  4. a formal's type before its default's ` =` (Arg::pretty_head), no tail;
#     BindExpr::fmt_pretty_inner keeps it with with_tail(" =").
# Command: sh design/review-2026-11-16/repro/t-print-then-overflow.sh
# Expected: no line longer than the width given to each run (20, 30, 24, 40).
# Observed:
#   21:     aaaaaaaaa.b[cc]$;                      (width 20)
#   32:     y if ggg(aaaaaa, bbbbbbb) =>           (width 30)
#   26:     fff(aaaaa, bbbbbbb) =>                 (width 24)
#   41:     #selected_background: [null, Color] =  (width 40)
# (1 shows in the stdlib at the default width: formatting
#  stdlib/graphix-package-tui/src/graphix/browser.gx at 90 writes the 91-column
#  line `structure.columns[slot_item(first_col, col_slots, c)]$;`)
G=${G:-/home/eric/tmp/review-2026-11-16/bin/graphix}
f() { timeout -s KILL 10 "$G" fmt --stdout --width "$1" | awk '{print length($0)": "$0}'; echo; }
printf 'let c = aaaaaaaaa.b[cc]$;\nc\n' | f 20
printf 'let r = select x { y if ggg(aaaaaa, bbbbbbb) => 1, _ => 2 };\nr\n' | f 30
printf 'let m = {fff(aaaaa, bbbbbbb) => 1, x => 2};\nm\n' | f 24
printf 'let f = |#selected_background: [null, Color] = null, x| x;\nf\n' | f 40
