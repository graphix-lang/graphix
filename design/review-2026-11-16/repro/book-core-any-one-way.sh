#!/bin/sh
# Title: fundamental_types.md says Any "unifies with any other type"; no concrete type holds an Any
# Command: sh design/review-2026-11-16/repro/book-core-any-one-way.sh
# Expected (a two-way "unifies"): `let y: i64 = x` with `x: Any` checks.
# Observed: "type mismatch i64 does not contain Any", for the let and for a call `f(x)` alike.
G=/home/eric/tmp/review-2026-11-16/bin/graphix
printf 'let x: Any = 1\nlet y: i64 = x\nlet f = |a: i64| a + 1\nf(x)\n' \
  | timeout -s KILL 30 $G --no-cache 2>&1 | grep -v '^Welcome\|^Press\|^$'
