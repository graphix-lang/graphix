#!/bin/bash
# Title: `graphix fmt --interface` is silently ignored when files are named
# Command: bash design/review-2026-11-16/repro/x-formatter-interface-flag-ignored.sh
# Expected: clap refuses `--interface` beside FILES (it says "stdin is an
#   interface"), or the file is formatted as an interface.
# Observed: the file is parsed as a program and refused with a parse error
#   ("a `;` separates this statement from the one before"), exit 1.
G=${G:-/home/eric/tmp/review-2026-11-16/bin/graphix}
d=$(mktemp -d)
printf 'val x: i64\n' > "$d/iface.gx"
timeout -s KILL 30 "$G" fmt --interface --stdout "$d/iface.gx"; echo "exit=$?"
printf 'val x: i64\n' | timeout -s KILL 30 "$G" fmt --interface; echo "stdin exit=$?"
rm -rf "$d"
