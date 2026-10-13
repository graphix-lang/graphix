#!/bin/bash
# shell-03: --log-dir without RUST_LOG hides a program's runtime failures entirely
#
# Command: bash design/review-2026-10-12/repro/shell-03.sh /path/to/graphix
#
# Expected: both runs print
#   unhandled error in file .../main.gx at line: 3, column: 1 "E"
# on stderr (CLAUDE.md: "the shell shows that target on stderr"; graphix-shell
# main.rs: "a program's failures reach stderr with or without a log"), and the
# --log-dir run also records it in the log file.
# Observed: the --log-dir run prints nothing, exits 0, and its log directory
# holds no file. Logger::try_with_env() is LogSpecification::off() when
# RUST_LOG is unset, so graphix::failure records are filtered out before
# Duplicate::Error sees them. With RUST_LOG=warn the same run shows the error.
G=${1:-graphix}
D=$(mktemp -d)
cd "$D" || exit 1
printf 'let r: [i64, Error<`E>] = error(`E);\nsys::exit(sys::time::after_idle(duration:100.ms, 0));\nr? + 1\n' > main.gx
echo "--- plain"
timeout -s KILL 30 "$G" --no-cache -W no-unhandled main.gx
echo "--- --log-dir, RUST_LOG unset"
env -u RUST_LOG timeout -s KILL 30 "$G" --no-cache -W no-unhandled --log-dir "$D/log" main.gx
echo "exit $?; log files:"; ls "$D/log"
echo "--- --log-dir, RUST_LOG=warn"
RUST_LOG=warn timeout -s KILL 30 "$G" --no-cache -W no-unhandled --log-dir "$D/log2" main.gx
rm -rf "$D"
