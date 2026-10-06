#!/usr/bin/env bash
# t-format-resolver-12: one bad GRAPHIX_MODPATH entry silently discards
# them all, and the book documents such entries.
#
# parse_modpath (graphix-types/src/expr/resolver.rs:170) bails on any
# entry that is neither `file:` nor a registered scheme, the empty entry
# a trailing comma leaves included; GX::new (graphix-rt/src/gx.rs:261)
# logs that at error level, which the shell does not show, and falls
# back to the data dir alone. book/src/shell.md:383 shows
# `GRAPHIX_MODPATH=/opt/graphix-libs`, and book/src/modules/implementation.md
# says an entry without `netidx:` is a file path.
#
# command: GRAPHIX=/path/to/graphix bash design/review-2026-10-05/repro/t-format-resolver-12.sh
#
# expected: every case finds lib/mylib.gx, or the shell says which entry it
# refused.
# observed (HEAD c722befe, debug build):
#   file:<lib>          exit 0
#   file:<lib>,         exit 1, "module mylib could not be found: <proj>/mylib.gx ...;
#                       <data>/graphix/mylib.gx ..." (lib never tried, nothing about MODPATH)
#   <lib>               exit 1, the same
#   file:<lib>,bogus:x  exit 1, the same
set -u
GRAPHIX=${GRAPHIX:-graphix}
dir=$(mktemp -d)
trap 'rm -rf "$dir"' EXIT
export XDG_CACHE_HOME="$dir/cache" XDG_DATA_HOME="$dir/data"
mkdir -p "$dir/proj" "$dir/lib" "$dir/data"
printf 'mod mylib;\nmylib::v\n' > "$dir/proj/main.gx"
printf 'let v = 42\n' > "$dir/lib/mylib.gx"
for mp in "file:$dir/lib" "file:$dir/lib," "$dir/lib" "file:$dir/lib,bogus:x"; do
  echo "GRAPHIX_MODPATH='${mp//$dir/<tmp>}':"
  GRAPHIX_MODPATH="$mp" timeout -s KILL 60 "$GRAPHIX" --check "$dir/proj/main.gx" 2>&1 \
    | sed "s|$dir|<tmp>|g"
  echo "exit ${PIPESTATUS[0]}"
done
