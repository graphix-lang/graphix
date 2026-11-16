#!/bin/sh
# Title: `--check` passes a program the build refuses; CLAUDE.md and `graphix --help`
#        describe --check as "compile + typecheck" and --expand as only "check + print".
# Command: sh design/review-2026-11-16/repro/x-doc-drift-check-passes-native-refusal.sh
# Expected (per the docs): both runs refuse, or --check is described as the check alone.
# Observed: --check exits 0; --expand refuses with
#   "#[native] expression did not fully fuse to native code: builtin `count` has no fast-call entry".
G=${G:-/home/eric/tmp/review-2026-11-16/bin/graphix}
d=$(mktemp -d)
printf 'let f = |x: i64| #[native] count(x);\nf(1)\n' > "$d/nat.gx"
timeout -s KILL 30 "$G" --no-cache --check "$d/nat.gx"; echo "--check rc=$?"
timeout -s KILL 30 "$G" --no-cache --expand "$d/nat.gx"; echo "--expand rc=$?"
rm -rf "$d"
