#!/bin/sh
# A script's own `mod map;` (map.gx beside it) is refused as a
# "duplicate module definition map": the resolver skips the VFS package
# root for it (resolve_from_vfs), but the compile then collides with the
# registered package's `/map` scope.
#
# Run: sh design/review-2026-10-19/repro/t-format-resolver-package-named-module.sh
# Expected: exit 0 (the program's own module), or a refusal that says the
#           name is taken by a package.
# Observed: Error ... in: mod map
#             1: duplicate module definition map
#           (the same file as mymap.gx checks clean)
B=${GRAPHIX:-/home/eric/tmp/review-2026-10-19/bin/graphix}
d=$(mktemp -d)
printf 'let lookup = |k: string| k\n' > "$d/map.gx"
printf 'mod map;\nmap::lookup("a")\n' > "$d/main.gx"
timeout -s KILL 30 "$B" --check "$d/main.gx"
echo "exit $?"
rm -rf "$d"
