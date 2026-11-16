#!/bin/bash
# Title: `graphix fmt` rewrites a read-only file and breaks its hard links
# Command: bash design/review-2026-11-16/repro/x-formatter-readonly-hardlink.sh
# Expected: formatting a 0444 file fails (or leaves it alone) as a write to
#   it would; a hard link to the file sees the formatted text.
# Observed: exit 0; a.gx is replaced by a new inode (still 0444) holding the
#   formatted text; hard.gx keeps the old inode and the unformatted text.
G=${G:-/home/eric/tmp/review-2026-11-16/bin/graphix}
d=$(mktemp -d)
printf 'let  x=1;\nx\n' > "$d/a.gx"
chmod 444 "$d/a.gx"
ln "$d/a.gx" "$d/hard.gx"
ls -li "$d"
timeout -s KILL 30 "$G" fmt "$d/a.gx"; echo "fmt exit=$?"
ls -li "$d"
echo "a.gx:"; cat "$d/a.gx"
echo "hard.gx:"; cat "$d/hard.gx"
chmod 644 "$d"/*.gx; rm -rf "$d"
