#!/bin/sh
# Title: the LSP "failed check, a checker panic included" pin no longer panics
# Command: sh tests-shell-compiler-lsp-panic-pin-vacuous.sh
# Expected (for the pin to cover what it says): the check of the bad.gx text
#   used by graphix-shell/tests/lsp/main.rs a_failed_check_leaves_the_server_running
#   reaches the checker panic that graphix-rt's catch_unwind in GX::check turns
#   into "the check panicked: ...".
# Observed: an ordinary type error ("this function is called with 2 arguments,
#   but its type fn() -> i64 takes 0"); no panic, so the recovery path is unpinned.
G=${G:-/home/eric/tmp/review-2026-10-19/bin/graphix}
d=$(mktemp -d)
printf "let g = |a: i64, f: fn() -> i64| -> i64 'array_group;\n0\n" > "$d/bad.gx"
timeout -s KILL 60 "$G" --no-cache --check "$d/bad.gx"
echo "rc=$?"
rm -rf "$d"
