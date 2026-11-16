#!/bin/sh
# Title: the shell test harness runs the user's init module and module path
# Command: sh design/review-2026-11-16/repro/tests-shell-compiler-b-repl-user-init.sh
# graphix-shell/tests/common/mod.rs `command` isolates the cache and the log
# but leaves XDG_DATA_HOME and GRAPHIX_MODPATH as the developer has them, and
# `repl` passes no --no-init. Here a data dir stands in for the developer's
# ~/.local/share/graphix holding an init.gx.
# Expected: a test's REPL run sees only its own input.
# Observed: "from init" is printed before the input's value, by the init
# module the user's data dir holds.
GX=${GX:-/home/eric/tmp/review-2026-11-16/bin/graphix}
D=$(mktemp -d)
mkdir -p "$D/graphix"
printf 'println("from init");\n' > "$D/graphix/init.gx"
printf '1 + 1\n' |
  XDG_DATA_HOME="$D" timeout -s KILL 30 "$GX" --no-netidx --no-cache 2>&1 | tail -3
rm -rf "$D"
