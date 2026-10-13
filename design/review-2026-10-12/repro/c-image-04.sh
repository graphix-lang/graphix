#!/bin/bash
# c-image-04: an image carries no identity of the build that wrote it.
#
# Feeds a registration image written by ANOTHER graphix executable (an
# older build at the same REGISTRATION_FORMAT) to this build, filed under
# this build's id the way an embedder that stores images itself would hand
# it to `RegistrationImage::load`.
#
# usage: c-image-04.sh <graphix binary> <older build's registration .img>
#   e.g. c-image-04.sh ~/tmp/target/debug/graphix \
#     ~/.cache/graphix/registration/e2b2a3ae638810fc3a79914556d68e884ce3f565/e649f3eed3e7bb5c2c7464f0d8950624.img
#
# expected: the reader refuses an image another build wrote and compiles
#   cold (a BuildGraph phase appears, the entry is rewritten).
# observed (HEAD 7324207f against the build of 2026-10-11 19:23):
#   ImageEnv/ImageDefs/ImageNodes decode, no module compiles, the foreign
#   entry is kept: "ACCEPTED". The same format byte from the 16:21 build
#   decodes through ImageNodes and then fails, so the format changed
#   without a REGISTRATION_FORMAT bump: the byte is no compatibility key.
set -u
BIN=$1
OLD=$2
W=$(mktemp -d)
printf 'sys::exit(sys::time::after_idle(duration:100.ms, 0));\n1 + 1\n' > "$W/p.gx"
XDG_CACHE_HOME=$W/cache timeout -s KILL 120 "$BIN" "$W/p.gx" > /dev/null 2>&1
D=$(dirname "$(find "$W/cache" -name '*.img' | head -1)")
REG=$(basename "$OLD")
rm -f "$D"/*.img
cp "$OLD" "$D/$REG"
GRAPHIX_PROFILE=1 XDG_CACHE_HOME=$W/cache timeout -s KILL 120 "$BIN" "$W/p.gx" 2>&1 \
    | grep -E 'phase=(Image[A-Za-z]+)|mphase=BuildGraph' | sed 's/ thread.*//' | sort -u | head
if cmp -s "$D/$REG" "$OLD"; then echo ACCEPTED; else echo "refused (rewritten)"; fi
rm -rf "$W"
