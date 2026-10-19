#!/bin/bash
# Title: fundamental_types.md transcripts print errors in the pre-kind-marker array form
# Command: bash design/review-2026-10-19/repro/book-core-fundamental-printed-forms.sh
# Expected (the book): error:["ArithError", "attempt to divide by zero"],
#   error:["ArithError", "attempt to subtract with overflow"], error:["MapKeyError", ..], 10.0
# Observed: error:`ArithError("attempt to divide by zero"), error:`ArithError("attempt to
#   subtract with overflow"), error:`MapKeyError("map key \"missing\" not found"), and
#   2.5 * 4.0 prints 10 (getting_started.md shows 10.0)
G=/home/eric/tmp/review-2026-10-19/bin/graphix
run() { printf "$1" | timeout -s KILL 4 $G --no-cache 2>&1 | grep -v '^Welcome\|^Press\|^$\|^-:'; }
run '0 /? 0\n'
run 'u32:0 -? u32:1\n'
run 'let m = {"a" => 1, "b" => 2, "c" => 3}\nm{"missing"}\n'
run '2.5 * 4.0\n'
