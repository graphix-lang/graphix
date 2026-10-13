#!/bin/sh
# Run the TUI repro in a pseudo-terminal (no window) and strip escapes.
G=${GRAPHIX:-graphix}
D=$(dirname "$0")
TERM=xterm-256color timeout -s KILL 25 script -q -c \
  "stty rows 20 cols 60; timeout -s KILL 15 $G --no-cache $D/examples-tui-04-size-writes-readonly-ref.gx" /dev/null 2>&1 \
  | cat -v | sed 's/\^\[\[[0-9;?]*[a-zA-Z]/ /g'
echo
