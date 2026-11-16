#!/bin/sh
# x-leaks: the same REPL line entered twice is refused when it declares a
# block type with an impl of a global trait.
#
# A body/block type of a REPL input is keyed by declaration_place, the
# input's text hash and the position (AbstractId::declared), so two inputs
# with one text declare ONE type; the earlier input is still live while the
# later compiles (the shell drops the previous output after the compile, and
# keeps a `let` input for the session), so its impl is still registered.
#
# Run: sh design/review-2026-11-16/repro/x-leaks-repl-same-line-conflict.sh
# Expected: both pairs print `-: bool` / `true`.
# Observed (HEAD e1be2fbf2):
#   -: bool
#   true
#   error: ... conflicting implementation: Eq is already implemented for C at line: 1, column: 27
# and for the `let` form the second `let a = ...` is refused for the rest of
# the session (its first input is kept). The same refusal through
# GXHandle::compile with a root trait instead of Eq:
# x-leaks-registry-counts.rs, failed_compiles.
G=/home/eric/tmp/review-2026-11-16/bin/graphix
L='{ type C = Abstract<i64>; impl Eq for C { let eq = |a, b| a.0 == b.0 }; C(1) == C(1) }'
printf '%s\n%s\n' "$L" "$L" | timeout -s KILL 20 $G --no-cache -n 2>&1 | tail -n +3
L2='let a = { type C = Abstract<i64>; impl Eq for C { let eq = |a, b| a.0 == b.0 }; C(1) == C(1) }'
printf '%s\n1\n%s\na\n' "$L2" "$L2" | timeout -s KILL 20 $G --no-cache -n 2>&1 | tail -n +3
