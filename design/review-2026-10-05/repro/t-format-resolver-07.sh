#!/usr/bin/env bash
# t-format-resolver-07: a stray file named like a module makes `mod x;`
# fail instead of trying the next resolver.
#
# read_optional (graphix-types/src/expr/resolver.rs:850) treats only
# ErrorKind::NotFound as absent. For `mod util;` FilesResolver opens
# <dir>/util.gx and then <dir>/util/mod.gx; when <dir>/util is a regular
# file the second open fails with ENOTDIR, resolve_from_files returns
# Resolution::Broken, and no later resolver (GRAPHIX_MODPATH, the data
# dir) is tried, although no module file exists at that path.
#
# command: GRAPHIX=/path/to/graphix bash design/review-2026-10-05/repro/t-format-resolver-07.sh
#
# expected: both runs check (util comes from GRAPHIX_MODPATH's lib/util.gx)
# observed (HEAD c722befe, debug build):
#   without proj/util: exit 0
#   with proj/util:    "could not resolve module util ... proj/util/mod.gx:
#                      Not a directory (os error 20)", exit 1
set -u
GRAPHIX=${GRAPHIX:-graphix}
dir=$(mktemp -d)
trap 'rm -rf "$dir"' EXIT
export XDG_CACHE_HOME="$dir/cache"
mkdir -p "$dir/proj" "$dir/lib"
printf 'mod util;\nutil::v\n' > "$dir/proj/main.gx"
printf 'let v = "from lib"\n' > "$dir/lib/util.gx"
echo "without proj/util:"
GRAPHIX_MODPATH="file:$dir/lib" timeout -s KILL 60 "$GRAPHIX" --check "$dir/proj/main.gx"
echo "exit $?"
printf '#!/bin/sh\necho util\n' > "$dir/proj/util"
chmod +x "$dir/proj/util"
echo "with proj/util (an executable script, not a module):"
GRAPHIX_MODPATH="file:$dir/lib" timeout -s KILL 60 "$GRAPHIX" --check "$dir/proj/main.gx"
echo "exit $?"
