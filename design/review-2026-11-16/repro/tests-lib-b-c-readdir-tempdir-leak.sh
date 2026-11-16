#!/bin/sh
# Title: test_readdir_skips_an_unreadable_subdirectory leaks its temp dir on every run
#
# The fixture chmods `<tempdir>/locked` to 0o000 and never restores it.
# tempfile's TempDir::drop calls std::fs::remove_dir_all, which must open
# `locked` to list it, gets EACCES, gives up, and the drop ignores the
# error: the whole temp dir (a.txt + locked) stays in /tmp, one per mode
# per run (4 per gate).
#
# Command: sh design/review-2026-11-16/repro/tests-lib-b-c-readdir-tempdir-leak.sh
# Expected: 0 leaked dirs (each run cleans up after itself)
# Observed (2026-11-16, washu-chan): 1577 leaked dirs, every one holding
#   exactly `a.txt locked`, `locked` mode d---------, dated over several days.

tmp=${TMPDIR:-/tmp}
n=$(ls -d "$tmp"/.tmp*/locked 2>/dev/null | wc -l)
echo "leaked temp dirs holding a 000 'locked' subdir: $n"
for d in $(ls -d "$tmp"/.tmp*/locked 2>/dev/null | head -5); do
    ls -ld --time-style=+%F "$d"
    ls -A "$(dirname "$d")"
done

# The mechanism, without the test: a directory holding an empty 000
# subdirectory is not removed by a remover that opens each directory to
# list it (std::fs::remove_dir_all, Python's shutil.rmtree alike).
w=$(mktemp -d)
mkdir "$w/locked" && touch "$w/a.txt" && chmod 000 "$w/locked"
timeout -s KILL 10 python3 -I -c 'import shutil,sys
try:
    shutil.rmtree(sys.argv[1])
    print("removed")
except OSError as e:
    print("not removed:", e)' "$w"
chmod 700 "$w/locked" 2>/dev/null; rm -rf "$w"
