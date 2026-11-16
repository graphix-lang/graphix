#!/bin/sh
# Title: error.md describes `$`'s warning but not that it reaches only the log
# Command: sh design/review-2026-11-16/repro/book-core-dollar-log.sh
# Expected (error.md "A warning from `$` names the site and the error ... Run with --no-fusion
#   to inspect the interpreter's diagnostics"): a warning on stderr, at least under --no-fusion.
# Observed: nothing on stderr with or without --no-fusion; the warning ("WARN ... ignored error
#   ... ArrayIndexError") appears only in the --log-dir log under RUST_LOG=warn.
G=/home/eric/tmp/review-2026-11-16/bin/graphix
D=$(mktemp -d /home/eric/tmp/review-2026-11-16/work/book-core/dollar.XXXX)
printf 'let a = [1];\nlet i = 5;\nlet z = a[i]$;\nz\n' > "$D/p.gx"
echo "== stderr, default"; timeout -s KILL 2 $G --no-cache "$D/p.gx"
echo "== stderr, --no-fusion"; timeout -s KILL 2 $G --no-cache --no-fusion "$D/p.gx"
mkdir -p "$D/log"; RUST_LOG=warn timeout -s KILL 2 $G --no-cache --log-dir "$D/log" "$D/p.gx"
echo "== log"; cat "$D"/log/*
