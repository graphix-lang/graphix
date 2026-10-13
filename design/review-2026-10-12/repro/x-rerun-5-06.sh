#!/usr/bin/env bash
# x-rerun-5-06 (variant of package-01): `graphix package create` validates
# the short name's characters only (graphix-package/src/lib.rs:234-241), so it
# scaffolds `select` (a reserved word: the package's root module can never be
# named, `use select;` and `select::var` are parse errors) and `core` (the
# stdlib's own: register() returns early on root_mods.contains("core"), so
# its builtins and modules are silently never registered, and the crate name
# graphix-package-core collides with the stdlib crate).
# command: GRAPHIX=/path/to/graphix bash design/review-2026-10-12/repro/x-rerun-5-06.sh
# expected: both refused with a message, as `Foo` and `2d` are
# observed (HEAD 7324207f): rc=0 for both; `use select;` fails to parse
G=${GRAPHIX:-graphix}
T=$(mktemp -d); trap 'rm -rf "$T"' EXIT
export HOME=$T/home XDG_CACHE_HOME=$T/cache XDG_DATA_HOME=$T/data XDG_CONFIG_HOME=$T/config
mkdir -p "$HOME" "$T/out"
for n in select core; do
  timeout -s KILL 60 "$G" package create "$n" --dir "$T/out"; echo "create $n rc=$?"
done
printf 'use select;\nselect::var\n' > "$T/use.gx"
timeout -s KILL 60 "$G" --check "$T/use.gx" 2>&1 | tail -3
