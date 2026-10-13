#!/bin/sh
# package-01: the removal cascade reads graphix-shell's [features] graph, which
# disagrees with the stdlib crates' [dependencies] that decide registration
# (defpackage!'s register registers every graphix-package-* runtime dep).
#
# Runs the real `graphix package remove` in a throwaway sandbox: a fake cargo
# root whose registry holds HEAD's graphix-shell/Cargo.toml (all the cascade
# reads), a stub cargo, private XDG dirs. stdin is not a tty and --yes is not
# passed, so it prints the cascade and stops before any build.
#
# command: sh design/review-2026-10-12/repro/package-01.sh /path/to/graphix-binary
# expected (crate deps): remove sys -> gui, tui; remove array -> http, list, tui;
#   remove str -> gui, tui
# observed (HEAD 7324207f):
#   remove sys   -> hbs, json, pack, toml, tui, xls   (json/toml/pack/xls/hbs never use sys; gui, which registers sys, missing)
#   remove array -> list, tui                          (http, which depends on array, missing)
#   remove str   -> gui                                (tui, which depends on str, missing)
set -u
GX=${1:?graphix binary}
REPO=$(cd "$(dirname "$0")/../../.." && pwd)
W=$(mktemp -d)
trap 'rm -rf "$W"' EXIT
mkdir -p "$W/fc/bin" "$W/fc/registry/src/idx/graphix-shell-0.9.0" "$W/data" "$W/cfg" "$W/cache"
cp "$REPO/graphix-shell/Cargo.toml" "$W/fc/registry/src/idx/graphix-shell-0.9.0/"
printf '#!/bin/sh\nexit 1\n' > "$W/fc/bin/cargo"; chmod +x "$W/fc/bin/cargo"
ln -s "$GX" "$W/fc/bin/graphix"
for t in sys array str; do
    echo "=== remove $t"
    env PATH="$W/fc/bin:/usr/bin:/bin" HOME="$W" XDG_DATA_HOME="$W/data" \
        XDG_CONFIG_HOME="$W/cfg" XDG_CACHE_HOME="$W/cache" \
        timeout -s KILL 60 graphix package remove "$t" < /dev/null
done
