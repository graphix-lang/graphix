#!/bin/sh
# Title: the image cache restores an entry anyone could have written: no
# owner/mode check, and the crc32 seal is forgeable, so a planted entry
# runs a different program (its JIT records install as machine code).
# Command: sh design/review-2026-10-26/repro/x-security-cache-trust.sh
# Expected: the second run refuses the 0666 entry in the 0777 directory (or
#   at least the tampered one) and prints hello-from-the-author.
# Observed: PWNED-by-a-cache-file   (the patched string literal runs)
set -e
G=${GRAPHIX:-/home/eric/tmp/review-2026-10-26/bin/graphix}
HERE=$(cd "$(dirname "$0")" && pwd)
D=$(mktemp -d)
cat > $D/p.gx <<'GX'
sys::exit(sys::time::after_idle(duration:100.ms, 0));
println("hello-from-the-author")
GX
XDG_CACHE_HOME=$D/cache timeout -s KILL 60 $G $D/p.gx
# what another user could do to a shared/pre-created cache directory
chmod 0777 $D/cache/graphix $D/cache/graphix/registration $D/cache/graphix/registration/*
chmod 0666 $D/cache/graphix/registration/*/*.img
timeout -s KILL 30 python3 -I $HERE/x-security-image-patch.py $D/cache \
  "hello-from-the-author" "PWNED-by-a-cache-file"
XDG_CACHE_HOME=$D/cache timeout -s KILL 60 $G $D/p.gx
rm -rf $D
