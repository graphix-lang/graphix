#!/bin/sh
# x-errors-01: errors raised in Lambda::compile carry no ErrorSite: no
# position in the CLI, wrong file and line in the LSP.
#
# Command: sh x-errors-01.sh /path/to/graphix   (run from this directory)
#
# Expected: each refusal names the lambda's line and column (as a type error
#   inside a lambda does: "at: line: 1, column: 9 .. in: |x| ..."), and the
#   LSP publishes the module case on helper.gx line 2.
# Observed (HEAD 7324207f):
#   1. "Caused by: arguments must have unique names" with no position.
#   2. "a variadic argument (`@args`) is for builtins only", no position.
#   3. "unknown builtin function nosuchbuiltin", no position.
#   4. nested: innermost site is the OUTER lambda "line: 1, column: 9 .. |z| {..".
#   5. LSP: diagnostic on main.gx range 0:0-0:3, none on helper.gx.
B=${1:?graphix binary}
T=$(mktemp -d)
printf 'let f = |x, x| x;\nf(1, 2)\n' > $T/dup.gx
printf 'let f = |@args: i64| args;\nf(1)\n' > $T/var.gx
printf "let f = |x: i64| -> i64 'nosuchbuiltin;\nf(1)\n" > $T/bi.gx
for f in dup var bi; do echo "== $f"; timeout -s KILL 60 $B --check $T/$f.gx; done
echo "== nested"; timeout -s KILL 60 $B --check x-errors-01-nested.gx
echo "== lsp module"
timeout -s KILL 90 python3 -I x-errors-lsp_probe.py $B x-errors-01-module \
  x-errors-01-module/main.gx x-errors-01-module/helper.gx
rm -rf $T
