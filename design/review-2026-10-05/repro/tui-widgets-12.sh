#!/usr/bin/env bash
# tui-widgets-12: tui integer fields bypass validate.rs: a negative wraps, or
# fails the widget compile and blanks the whole screen.
#
# Every count, index and size in the tui .gxi files is `[i64, null]`. ListW,
# TabsW, TableW and SparklineW read selected/scroll/max as u32/usize/u64
# through netidx's wrapping `I64(v) => v as u32` (list.rs:23-24, tabs.rs:26,
# table.rs:23-24 and 81-83, sparkline.rs:60). TableW reads column_spacing and
# a Row's height/top_margin/bottom_margin as u16 (table.rs:36-39, 72), a
# range-checked cast that FAILS on a negative; so does a Color's Rgb/Indexed
# u8 (lib.rs:143-151). A failed TRef fails the widget's compile, layout,
# block and overlay compile their children with `?`, the root never compiles
# and the screen stays blank, with no message unless --log-dir is given.
# validate.rs states the policy the other widgets follow (clamp, warn once,
# never fail the compile); bar_chart's max is the control.
#
# command (needs tmux; each case runs in a private, detached 40x6 server):
#   GRAPHIX=/path/to/graphix bash design/review-2026-10-05/repro/tui-widgets-12.sh
#
# expected: list ">>Alpha" (-1 clamped to 0 like bar_chart's max), tabs shows
#   "body of A", spark draws against a clamped max with a warning, margin and
#   rgb show the Header block above their body, as their controls do.
# observed (HEAD c722befe, debug build):
#   list     "  Alpha", "  Beta", ">>Gamma": -1 selects the LAST item
#   tabs     " A │ B" and no body
#   spark    empty, no warning
#   bar      full bars, log "bar_chart max -1 negative; clamping to 0"
#   margin   blank screen, log "invalid widget specification compiling child
#            Caused by: can't cast" (row(#top_margin: -1) in a table under a layout)
#   margin0  Header block, then "cell-a               cell-b"
#   rgb      blank screen, same log (style(#fg: `Rgb({r: 256, ..})) on a span)
set -u
G=${GRAPHIX:-graphix}
D=$(mktemp -d)
trap 'tmux -S "$D/sock" kill-server 2>/dev/null; rm -rf "$D"' EXIT
printf 'set -g remain-on-exit on\nset -g status off\n' > "$D/tmux.conf"

run() {
    printf '%s\n' "$2" > "$D/$1.gx"
    mkdir -p "$D/log-$1"
    tmux -S "$D/sock" -f "$D/tmux.conf" new-session -d -x 40 -y 6 -s "$1" \
        "XDG_CACHE_HOME=$D/cache RUST_LOG=warn timeout -s KILL 20 $G -n --no-cache --log-dir $D/log-$1 $D/$1.gx"
    sleep 4
    echo "== $1"
    tmux -S "$D/sock" capture-pane -p -t "$1" | sed -e '/^$/d' -e 's/^/| /'
    tmux -S "$D/sock" kill-session -t "$1"
    cat "$D/log-$1"/* 2>/dev/null | sed -e '/^$/d' -e 's/^/  log: /'
}

run list 'use tui::{line, list::list};
list(#highlight_symbol: &">>", #selected: &-1, &[line("Alpha"), line("Beta"), line("Gamma")])'

run tabs 'use tui::{line, paragraph::paragraph, tabs::tabs};
tabs(#selected: &-1, &[(line("A"), paragraph(&"body of A")), (line("B"), paragraph(&"body of B"))])'

run spark 'use tui::sparkline::sparkline;
sparkline(#max: &-1, &[1.0, 2.0, 3.0, 4.0])'

run bar 'use tui::{line, barchart::{bar, bar_chart, bar_group}};
let b1 = bar(#label: &line("X"), &1);
let b2 = bar(#label: &line("Y"), &4);
bar_chart(#max: &-1, &[bar_group(#label: line("G"), [b1, b2])])'

for m in -1 0; do
    name=margin; [ "$m" = 0 ] && name=margin0
    run $name "use tui::{line, block::block, layout::{child, layout}, text::text, table::{cell, row, table}};
let header = block(#border: &\`All, &text(&\"Header\"));
let r1 = row(#top_margin: $m, [cell(line(\"cell-a\")), cell(line(\"cell-b\"))]);
layout(#direction: &\`Vertical, &[child(#constraint: \`Length(3), header), child(#constraint: \`Fill(1), table(&[&r1]))])"
done

run rgb 'use tui::{line, span, style, block::block, layout::{child, layout}, text::text};
let header = block(#border: &`All, &text(&"Header"));
let body = text(&[line([span(#style: style(#fg: `Rgb({r: 256, g: 0, b: 0})), "red")])]);
layout(#direction: &`Vertical, &[child(#constraint: `Length(3), header), child(#constraint: `Fill(1), body)])'
