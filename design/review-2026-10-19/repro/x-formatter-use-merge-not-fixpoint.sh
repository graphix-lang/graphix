#!/bin/sh
# Title: graphix fmt refuses a file whose use run is split by a dependency
# Command: sh x-formatter-use-merge-not-fixpoint.sh [path-to-graphix]
# Expected: the uses laid out, e.g. "use x::p;\nuse a::b;\nuse p::q;\nq + b", exit 0
# Observed: "formatter bug: the formatted text says something else
#   was line: 1, column: 1: use x::p
#   now line: 1, column: 1: use a::b", exit 1.
# `use p::q` reads what `use x::p` binds, so the first run ends; the second
# run sorts `a::b` ahead of `p::q`, which puts `a::b` next to `x::p`, and the
# reparse merges and sorts those two: merge_uses is not a fixpoint.
# (A comment above the first use would decorate it and hide the bug, so the
# program is fed on stdin.)
G=${1:-graphix}
printf 'use x::p;\nuse p::q;\nuse a::b;\nq + b\n' | "$G" fmt
echo "exit $?"
