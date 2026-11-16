#!/bin/bash
# Title: the "settled when its input ended; declare e's type" note attaches to a refusal a union's OTHER member causes
# Command: bash design/review-2026-11-16/repro/x-diagnostics-settled-note-union.sh
# Expected: the third input is refused with no settled note: its f64 member fits neither
#   string nor [i64, string], so declaring e's type cannot lift it (the second run, with
#   `let e: Array<i64> = []`, is refused the same way). A tuple holding `e` already omits it.
# Observed: "cannot compare [f64, Array<_>] with string ...: it holds the type of `e` (line 1,
#   column 5 in `let e = []`), settled when its input ended; declare e's type where it is bound",
#   and the same note on "type mismatch [i64, string] does not contain [f64, Array<_>]".
G=${GRAPHIX:-/home/eric/tmp/review-2026-11-16/bin/graphix}
run() { printf "$1" | timeout -s KILL 20 $G --no-cache 2>&1 | grep -v '^$' | tail -1; }
run 'let e = []\nlet v = select true { true => e, false => 1.5 }\nv == "s"\n'
run 'let e = []\nlet v = select true { true => e, false => 1.5 }\nlet s: [string, i64] = v\n'
run 'let e: Array<i64> = []\nlet v = select true { true => e, false => 1.5 }\nv == "s"\n'
run 'let e = []\nlet t = (e, 1.5)\nlet s: (Array<i64>, string) = t\n'
