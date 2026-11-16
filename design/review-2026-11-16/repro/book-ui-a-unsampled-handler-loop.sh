#!/bin/sh
# Title: a UI handler that reads state without sampling it self-loops from the start
# Command: sh design/review-2026-11-16/repro/book-ui-a-unsampled-handler-loop.sh
# Context: book/src/ui/gui/overview.md walks through hello.gx, whose button
#   writes `|c| clicks <- c ~ clicks + 1`, and never says why the `c ~` is there.
# Expected: neither program below redraws without input (no key is pressed).
# Observed: the sampled handler (`n <- e ~ n + 1`) prints ~100 bytes in 3 s;
#   the unsampled one (`n <- n + 1`) fills the 2 MB cap with redraws: the
#   handler's body re-fires itself every cycle with no event at all.
GX=${GX:-/home/eric/tmp/review-2026-11-16/bin/graphix}
d=$(mktemp -d)
cat > "$d/loop.gx" <<'EOF'
use tui::{input_handler::{Event, input_handler}, text::text};
let n = 0;
let h = |e: Event| { n <- n + 1; `Continue };
input_handler(#handle: &h, &text(&"count [n]"))
EOF
sed 's/n <- n + 1/n <- e ~ n + 1/' "$d/loop.gx" > "$d/ok.gx"
for p in ok loop; do
  bytes=$(TERM=xterm timeout -s KILL 3 script -qfc "$GX --no-cache $d/$p.gx" /dev/null 2>&1 \
    | head -c 2000000 | wc -c)
  echo "$p: $bytes bytes of terminal output in 3 s"
done
rm -rf "$d"
