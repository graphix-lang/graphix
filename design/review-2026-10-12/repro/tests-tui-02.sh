#!/bin/sh
# tests-tui-02: line_edit's paste turns only "\n" into a space; a terminal's CR or CRLF
# paste leaves "\r" in the single-line editor's value (line_edit_takes_a_paste pins LF only).
# Run: sh design/review-2026-10-12/repro/tests-tui-02.sh [path/to/graphix]
# Drives a real pty (script(1)) with bracketed paste; crossterm passes the bytes through.
# The program exits 1 when ed.value holds a "\r", 2 when it does not.
# Expected: LF=2 CRLF=2 CR=2.  Observed at 7324207f: LF=2 CRLF=1 CR=1.
B=${1:-graphix}
D=$(mktemp -d)
cat > "$D/paste.gx" <<'GX'
use tui::{input_handler::{Event, input_handler}, line_edit, text::text};
let ed = line_edit::state("");
let pasted = never();
let f = |e: Event| -> [`Stop, `Continue] {
    pasted <- select e { `Paste(_) => e ~ true, _ => never() };
    line_edit::handle(&mut ed, e)
};
let code = select str::contains(#part: "\r", ed.value) { true => 1, false => 2 };
sys::exit(sys::time::after_idle(duration:300.ms, pasted ~ code));
tui::exit(sys::time::timer(duration:8.s, false));
input_handler(#handle: &f, &text(&"v=[ed.value]"))
GX
for case in 'LF:a\nb' 'CRLF:a\r\nb' 'CR:a\rb'; do
    name=${case%%:*}; body=${case#*:}
    (sleep 2; printf "\033[200~$body\033[201~"; sleep 4) |
        timeout -s KILL 30 script -qfec "$B --no-cache $D/paste.gx" /dev/null >/dev/null 2>&1
    printf '%s=%s ' "$name" "$?"
done
echo
rm -rf "$D"
