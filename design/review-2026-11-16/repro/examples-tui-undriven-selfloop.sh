#!/bin/bash
# Title: an interactive TUI example examples_test never drives can regress
# into the self-loop trap with every gate green
#
# input_vim's Insert arm made unsampled (`text <- "[text][c]"`, the trap the
# skill's first rule names) checks clean even with -W error, still draws its
# first frame (all every_example_renders asks), and is not among the examples
# examples_test drives with keys. One key in insert mode then appends forever
# at 100% CPU.
#
# Command: bash design/review-2026-11-16/repro/examples-tui-undriven-selfloop.sh
#   (needs tmux; runs the program in a private tmux server for ~10 s)
# Expected: some gate fails for the broken example.
# Observed: "check rc=0" (no warning), the first frame "`Normal:", and after
#   `i` `a` the paragraph fills with "aaaa..." without end.
set -u
GX=${GX:-/home/eric/tmp/review-2026-11-16/bin/graphix}
ROOT=$(cd "$(dirname "$0")/../../.." && pwd)
TMP=$(mktemp -d)
SOCK=repro_selfloop_$$
trap 'tmux -L $SOCK kill-server 2>/dev/null; rm -rf "$TMP"' EXIT
export XDG_CACHE_HOME=$TMP/cache
sed 's/text <- k ~ "\[text\]\[c\]";/text <- "[text][c]";/' \
  "$ROOT/book/src/examples/tui/input_vim.gx" > "$TMP/input_vim.gx"
diff "$ROOT/book/src/examples/tui/input_vim.gx" "$TMP/input_vim.gx"
timeout -s KILL 60 "$GX" --check --no-cache -W error "$TMP/input_vim.gx"; echo "check rc=$?"
tmux -L $SOCK new-session -d -s s -x 80 -y 24 \
  "XDG_CACHE_HOME=$XDG_CACHE_HOME timeout --foreground -s KILL 30 $GX --no-cache $TMP/input_vim.gx; sleep 30"
sleep 5
echo "== first frame"; tmux -L $SOCK capture-pane -p -t s | head -2
tmux -L $SOCK send-keys -t s -l i; sleep 0.5
tmux -L $SOCK send-keys -t s -l a; sleep 1
echo "== after i, a"; tmux -L $SOCK capture-pane -p -t s | head -4
