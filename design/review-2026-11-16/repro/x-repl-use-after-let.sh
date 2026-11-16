#!/bin/sh
# Title: in the REPL a `use` of a name a top-level `let` (or `type`) already
#        declares is accepted and does nothing; a script refuses it.
# Command: sh design/review-2026-11-16/repro/x-repl-use-after-let.sh
# Expected: the REPL either refuses `use array::map` as the script does
#           ("`map` is already defined in this scope; use `as` to rename") or
#           the import shadows the let (CFlag::ReplaceImports: "a colliding
#           `use` shadows instead of erroring"), so `map` shows the function.
# Observed: `use array::map` and `use str::Escape as T` are accepted silently;
#           `map` still shows 5 and `T` is still i64; the script refuses.
G=${GRAPHIX:-/home/eric/tmp/review-2026-11-16/bin/graphix}
D=$(mktemp -d)
export XDG_CACHE_HOME=$D/cache
printf 'let map = 5\nuse array::map\nmap\ntype T = i64\nuse str::Escape as T\nlet t: T = 3\nt\n' |
  timeout -s KILL 30 "$G" --no-cache --no-init 2>&1
echo "--- script:"
printf 'let map = 5;\nuse array::map;\nmap\n' > $D/p.gx
timeout -s KILL 30 "$G" --no-cache --check $D/p.gx 2>&1 | tail -2
rm -rf $D
