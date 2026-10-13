#!/bin/bash
# shell-03: with --log-dir and RUST_LOG unset a program's failures are
# shown nowhere. Usage: shell-03-logdir-hides-failures.sh /path/to/graphix
G=${1:-graphix}
D=$(mktemp -d); cd $D
printf 'let r: [i64, Error<`E>] = error(`E);\nsys::exit(sys::time::after_idle(duration:100.ms, 0));\nr? + 1\n' > main.gx
echo "--- plain (prints: unhandled error ...)"; $G --no-cache -W no-unhandled main.gx
echo "--- --log-dir (bug: prints nothing, log empty)"; env -u RUST_LOG $G --no-cache -W no-unhandled --log-dir $D/log main.gx
cat $D/log/* 2>/dev/null
rm -rf "$D"
