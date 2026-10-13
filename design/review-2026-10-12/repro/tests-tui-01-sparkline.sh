#!/bin/sh
# Renders tests-tui-01-sparkline.gx on a 10x10 pty. Values 10/40/70 of max 100
# should fill exactly 1/4/7 rows (all full blocks); each bar's top cell draws
# U+2587 (7/8 block) instead, one tick short.
# Expected: 12 x █, 0 x ▇.  Observed at 7324207f: 9 x █, 3 x ▇.
G=${GRAPHIX:-graphix}
D=$(dirname "$0")
timeout -s KILL 20 script -qfec "stty rows 10 cols 10; $G --no-cache $D/tests-tui-01-sparkline.gx" /dev/null </dev/null >/tmp/spark.out 2>&1
grep -o "[▇█]" /tmp/spark.out | sort | uniq -c
