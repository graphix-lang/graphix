#!/bin/sh
# tui-core-05: held stderr grows without bound while a TUI display runs, and is
# all dumped to the terminal at exit (StderrHeld in stdlib/graphix-package-tui/src/lib.rs).
# Program: tui-core-stderr-unbounded.gx (an unhandled `?` failing every 1 ms tick,
# exit after 2000 ticks).
# Command: sh tui-core-05.sh <path-to-graphix>
# Expected: the held output is bounded (a cap, or head/tail with a "dropped" line).
# Observed (HEAD 7324207f): 1 failure before the alternate screen, then all 2000
#   'unhandled error ... ["Boom", i64:N]' lines (~257 KB) written after
#   ESC[?1049l (leaving the alternate screen): every byte was held in the tempfile.
B=${1:-graphix}
D=$(dirname "$0")
OUT=$(mktemp)
timeout -s KILL 120 script -qec "stty cols 80 rows 24; $B --no-cache $D/tui-core-stderr-unbounded.gx" "$OUT" </dev/null >/dev/null 2>&1
python3 -I -c "
import sys
d=open(sys.argv[1],'rb').read(); i=d.rfind(b'\x1b[?1049l')
print('failures before leaving alt screen', d[:i].count(b'Boom'), 'after', d[i:].count(b'Boom'), 'bytes after', len(d)-i)" "$OUT"
rm -f "$OUT"
