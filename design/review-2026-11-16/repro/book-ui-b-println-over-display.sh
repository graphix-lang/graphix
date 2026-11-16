#!/bin/sh
# book-ui-b: book/src/ui/tui/overview.md "Printing While a TUI Runs" tells the reader to
# redirect stderr and write to it, but println/print/log default to stdout (the display),
# and a widget's warning (line_gauge's clamp) reaches neither stderr nor the screen by default.
# command:  sh book-ui-b-println-over-display.sh /path/to/graphix
# expected (a reader of the section): with `2>debug.log`, the program's debug output lands in debug.log
# observed: "PRINTED-a" (println) is drawn over the display; only dbg's line reaches debug.log;
#           line_gauge(&2.0)'s "ratio 2 outside [0, 1]" warning is in neither place, and appears
#           only with RUST_LOG=warn --log-dir (in the log file)
GX=${1:-/home/eric/tmp/review-2026-11-16/bin/graphix}
D=$(dirname "$0"); T=$(mktemp -d)
cat > $T/prt.gx <<'EOF'
use tui::{input_handler::{Event, input_handler, on_press}, paragraph::paragraph};
let last = "none";
let handle = |e: Event| on_press(e, |k| select k.code {
  c@ `Char(s) => { last <- c ~ s; println("PRINTED-[s]"); dbg(s); `Stop },
  _ => `Continue
});
input_handler(#handle: &handle, &paragraph(&"last: [last]"))
EOF
printf 'use tui::line_gauge::line_gauge;\nline_gauge(&2.0)\n' > $T/lg.gx
STDERR_TO=$T/debug.log timeout -s KILL 60 python3 $D/book-ui-b-tui-drive.py $GX $T/prt.gx 40 4 5 'a|\x03'
echo "--- debug.log:"; cat $T/debug.log
STDERR_TO=$T/lg.err timeout -s KILL 60 python3 $D/book-ui-b-tui-drive.py $GX $T/lg.gx 40 2 5 '\x03' > /dev/null
echo "--- line_gauge stderr (default logging):"; cat $T/lg.err
mkdir $T/logs; RUST_LOG=warn timeout -s KILL 60 python3 $D/book-ui-b-tui-drive.py $GX $T/lg.gx 40 2 5 '\x03' --log-dir $T/logs > /dev/null
echo "--- line_gauge log file (RUST_LOG=warn --log-dir):"; cat $T/logs/*
rm -rf $T
