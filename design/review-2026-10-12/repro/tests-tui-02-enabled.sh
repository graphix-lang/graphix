#!/bin/sh
# Types "abc" into tests-tui-02-enabled.gx on a pty. The outer handler is
# disabled; the inner one should see the 3 keys (hits=3). Observed at
# 7324207f: hits stays 0 (no "1;6H" redraws in the output). Remove
# `#enabled: &false, ` and the same run shows hits 1, 2, 3.
G=${GRAPHIX:-graphix}
D=$(dirname "$0")
(sleep 1; printf 'abc'; sleep 2) | timeout -s KILL 20 script -qfec "stty rows 5 cols 20; $G --no-cache $D/tests-tui-02-enabled.gx" /dev/null >/tmp/enabled.out 2>&1
cat -v /tmp/enabled.out; echo
