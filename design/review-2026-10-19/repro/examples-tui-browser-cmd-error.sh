#!/bin/bash
# Title: browser_commands names the wrong command and dumps the whole ErrChain in the title
# Command: timeout -s KILL 120 design/review-2026-10-19/repro/examples-tui-browser-cmd-error.sh
# Expected: typing `:quit` Enter shows something like `invalid command quit` in the bottom title.
# Observed: `error:{cause: null, error: "invalid command qu", ori: {parent: null, source:
#   `File("/.../browser_commands.gx"), text: ""}, pos: {column: 14, line: 24}}`
#   (the message carries only the first two characters, and "[e]" prints the ErrChain struct).
D=$(dirname "$0"); E=$(cd "$D/../../.." && pwd)/book/src/examples/tui
"$D/examples-tui-tmux-driver.sh" $E/browser_commands.gx 6 250 "sleep:5;key::;key:q;key:u;key:i;key:t;sleep:0.3;key:Enter;sleep:1;cap" | tail -2
