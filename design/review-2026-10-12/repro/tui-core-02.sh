#!/bin/sh
# tui-core-02: tui::exit is lost when it fires before the display installs
# its stop, and is a no-op when the program's value is not a Tui.
# Command: sh design/review-2026-10-12/repro/tui-core-02.sh <path-to-graphix>
# Expected: every case exits on its own (exit 0) after the exit fires.
# Observed (HEAD 7324207f, debug build):
#   init     exit=137  (tui::exit(null) at init: never exits, killed)
#   delayed  exit=0    (the same exit 1ms later: exits)
#   no-tui   exit=137  (tui::exit after 200ms, value is a string: never exits)
G=${1:-graphix}
D=$(mktemp -d)
cat > "$D/init.gx" <<'GX'
use tui::text::text;
tui::exit(null);
text(&"hello")
GX
cat > "$D/delayed.gx" <<'GX'
use tui::text::text;
tui::exit(sys::time::timer(duration:1.ms, false));
text(&"hello")
GX
cat > "$D/no-tui.gx" <<'GX'
tui::exit(sys::time::timer(duration:200.ms, false));
"hello"
GX
for f in init delayed no-tui; do
  timeout -s KILL 8 script -qfec "$G --no-cache $D/$f.gx" /dev/null > /dev/null 2>&1
  echo "$f exit=$?"
done
rm -rf "$D"
