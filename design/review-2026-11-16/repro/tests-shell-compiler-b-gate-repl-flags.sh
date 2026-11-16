#!/bin/sh
# Title: examples_compile's Repl is not the shell's REPL: it compiles without
# CFlag::ReplaceImports (and skips GX::compile's other steps)
# Command: sh design/review-2026-11-16/repro/tests-shell-compiler-b-gate-repl-flags.sh
# The REPL (Mode::Repl -> ReplaceImports) accepts a second `use` of a name;
# compiled without that flag, as Repl::statement compiles a transcript input
# (CFlag::CheckOnly | CFlag::FusionDisabled), the same statements are refused.
# Expected: the gate judges a transcript as the REPL does.
# Observed: REPL prints `-: i64` / `3`; without ReplaceImports (here a
# script's --check) the second `use` is refused:
#   `len` is already imported here (from `array`); rename one (`use ... as ...`)
# So a page transcript showing the REPL's own answer would fail the gate.
GX=${GX:-/home/eric/tmp/review-2026-11-16/bin/graphix}
D=$(mktemp -d)
printf '%s\n' 'use array::len' 'use str::len' 'len("abc")' |
  timeout -s KILL 30 "$GX" --no-cache --no-netidx 2>&1 | tail -2
printf 'use array::len;\nuse str::len;\nlen("abc")\n' > "$D/s.gx"
timeout -s KILL 30 "$GX" --no-cache --no-netidx --check "$D/s.gx" 2>&1 | tail -1
rm -rf "$D"
