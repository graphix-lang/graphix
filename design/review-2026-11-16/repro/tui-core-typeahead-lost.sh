#!/bin/bash
# Title: keys typed ahead are lost when a key's handler switches the widget tree
# Command: bash design/review-2026-11-16/repro/tui-core-typeahead-lost.sh
# Ten keys arrive in one burst. Each key's handler counts it; in flip.gx it also switches the root
# between two input_handlers. The first key is in flight while the other nine wait in the
# InputHandlerW's queue; its answer is held behind its writes, so the root's new value reaches
# Screen::update first, the old tree (queue included) is dropped, and the answer is ignored.
# Expected: both programs count 10 keys ("B 10"/"A 10" for flip.gx, "A 10" for noflip.gx).
# Observed: flip.gx draws "A 0" then "B 1" and nothing more (9 keys lost); noflip.gx draws "A 10".
here=$(dirname "$0")
G=${G:-/home/eric/tmp/review-2026-11-16/bin/graphix}
d=$(mktemp -d)
cat > "$d/flip.gx" <<'GX'
use tui::{input_handler::{input_handler, on_press}, text::text};
let mode: [`A, `B] = `A;
let n = 0;
let ha = |e: tui::input_handler::Event| on_press(e, |k| { n <- k ~ n + 1; mode <- k ~ `B; `Stop });
let hb = |e: tui::input_handler::Event| on_press(e, |k| { n <- k ~ n + 1; mode <- k ~ `A; `Stop });
select mode {
  `A => input_handler(#handle: &ha, &text(&"A [n]")),
  `B => input_handler(#handle: &hb, &text(&"B [n]"))
}
GX
cat > "$d/noflip.gx" <<'GX'
use tui::{input_handler::{input_handler, on_press}, text::text};
let n = 0;
let ha = |e: tui::input_handler::Event| on_press(e, |k| { n <- k ~ n + 1; `Stop });
input_handler(#handle: &ha, &text(&"A [n]"))
GX
for f in flip noflip; do
  echo "=== $f.gx (final frames)"
  TAIL=1200 timeout -s KILL 30 python3 "$here/tui-core-ptyrun.py" --size 30x3 --wait 6 \
    --keys '2:abcdefghij' --keys '5:\x03' -- "$G" --no-cache "$d/$f.gx" | tail -1 \
    | grep -o '1;1H[AB]\|H[0-9]*\\x1b\[39m' | tail -3
done
