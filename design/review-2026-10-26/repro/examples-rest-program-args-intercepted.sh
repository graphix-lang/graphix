#!/usr/bin/env bash
# Title: arguments after the script's filename are parsed as graphix's own
# flags and subcommands when the first one matches a graphix option.
# Command: bash design/review-2026-10-26/repro/examples-rest-program-args-intercepted.sh
# Expected (graphix --help: "arguments passed to the graphix program
# (everything after the filename)"): every case prints the script's own
# args::parse answer (its own --help text, its own "-n" count, "Command:
# [\"fmt\"]"-style output or its usage error).
# Observed: `cli.gx --help` prints graphix's help; `cli.gx -n 3 foo` is
# graphix's --no-netidx and the script sees ["3", "foo"]; `cli.gx fmt x`
# runs `graphix fmt x` (formats the file x in place: "x: No such file");
# `cli.gx --check` only typechecks.
G=${G:-/home/eric/tmp/review-2026-10-26/bin/graphix}
f=$(dirname "$0")/../../../book/src/examples/net/cli.gx
for args in "--help" "-n 3 foo" "fmt x" "--check"; do
    echo "== graphix cli.gx $args"
    # shellcheck disable=SC2086
    timeout -s KILL 10 "$G" --no-cache "$f" $args 2>&1 | head -3
done
