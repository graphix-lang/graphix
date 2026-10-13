#!/bin/sh
# Run a TUI repro in a pty: tui-widgets-run.sh prog.gx [cols] [rows]
# Prints any panic, then the final screen (tui-widgets-screen.py replays the
# cursor-addressed output). GRAPHIX defaults to `graphix` on PATH; a debug
# build shows the overflow panics, a release build the misrender.
f=$1; c=${2:-80}; r=${3:-24}
BIN=${GRAPHIX:-graphix}
out=$(mktemp)
timeout -s KILL 40 script -qec "stty cols $c rows $r; $BIN --no-cache $f" /dev/null > "$out" 2>&1
echo "exit=$?"
tr -d '\033' < "$out" | grep -a -E 'panicked|overflow' | head -4
python3 "$(dirname "$0")/tui-widgets-screen.py" "$out" "$c" "$r"
rm -f "$out"
