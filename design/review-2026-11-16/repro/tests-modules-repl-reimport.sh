#!/bin/sh
# Title: the REPL's re-`use` (CFlag::ReplaceImports) has no pin anywhere
# design/module_system.md: "The REPL compiles with CFlag::ReplaceImports so a
# re-`use` shadows, like a `let` re-binding; file modules keep the error."
# Only the file side is pinned (two_imports_of_one_name_are_refused). Piped
# into the REPL, the second import shadows the first:
# Command: sh tests-modules-repl-reimport.sh  (G= the debug graphix)
# Observed: 3  (str::len("abc"); the file-module form is refused)
G=${G:-/home/eric/tmp/review-2026-11-16/bin/graphix}
(printf 'use array::len\n'; sleep 1; printf 'use str::len\n'; sleep 1
 printf 'len("abc")\n'; sleep 2) | timeout -s KILL 30 "$G" --no-cache 2>&1 | tail -1
