#!/usr/bin/env bash
# package-07: `graphix package remove` exits 0 with "No changes needed." when
# a cascade needs a TTY it does not have.
#
# confirm_yn (graphix-package/src/lib.rs:941-944) answers "no" without
# prompting when stdin is not a terminal; remove_packages (1389-1391) then
# skips the package and, with nothing else changed, prints "No changes
# needed." and returns Ok. `remove` has no --yes, so a script has no way to
# accept the cascade; `update` refuses the same situation with a hard error
# (1646-1650).
#
# Everything runs under a mktemp dir: HOME/XDG_*/CARGO_HOME point there, a
# fake `graphix` (for `graphix --version`) and a fake `cargo` (whose
# registry/src holds a copy of graphix-shell/Cargo.toml, the feature graph
# remove reads) come first on PATH, so nothing is downloaded or compiled and
# the real packages.toml is never touched.
#
# command: GRAPHIX=/path/to/graphix bash design/review-2026-10-05/repro/package-07.sh
#
# expected: case 1 exits non-zero (an error naming the missing confirmation
#   or --yes), as `update` does without a TTY; it never says "No changes
#   needed." while sys, which was asked for, is still installed.
# observed (HEAD c722befe, debug build):
#   case 1 `remove sys </dev/null`:
#     Removing sys also removes packages that depend on it: hbs, json, pack, toml, tui, xls
#     Skipping sys (its dependents are still installed)
#     No changes needed.
#     rc=0, packages.toml byte-identical, sys still installed
#   case 2 `remove sys hbs json pack toml tui xls </dev/null` (fake cargo
#     succeeds): sys is skipped "(its dependents are still installed)", then
#     the same command removes every one of those dependents, rebuilds and
#     exits rc=0 with sys still installed; the same list with sys last
#     removes everything.
set -u
GRAPHIX=${GRAPHIX:-graphix}
repo=$(cd "$(dirname "$0")/../../.." && pwd)
ver=$(sed -n 's/^version = "\(.*\)"/\1/p' "$repo/graphix-shell/Cargo.toml" | head -1)
T=$(mktemp -d)
trap 'rm -rf "$T"' EXIT
mkdir -p "$T/home" "$T/fakebin" "$T/cargo/bin" "$T/cargo/registry/src/idx/graphix-shell-$ver"
cp "$repo/graphix-shell/Cargo.toml" "$T/cargo/registry/src/idx/graphix-shell-$ver/Cargo.toml"
printf '#!/bin/bash\necho "graphix %s"\n' "$ver" > "$T/fakebin/graphix"
cat > "$T/cargo/bin/cargo" <<EOF
#!/bin/bash
echo "fake cargo: \$*"
[ -n "\${FAKE_CARGO_OK:-}" ] && exit 0
exit 101
EOF
chmod +x "$T/fakebin/graphix" "$T/cargo/bin/cargo"
pm() { # pm [VAR=val..] -- <package args..>; stdin is /dev/null
    local envs=()
    while [ "$1" != -- ]; do envs+=("$1"); shift; done; shift
    env PATH="$T/fakebin:$T/cargo/bin:$PATH" HOME="$T/home" CARGO_HOME="$T/home/cargo" \
        XDG_DATA_HOME="$T/data" XDG_CACHE_HOME="$T/cache" XDG_CONFIG_HOME="$T/config" \
        "${envs[@]}" timeout -s KILL 60 "$GRAPHIX" package "$@" </dev/null
}
toml=$T/data/graphix/packages.toml
installed() { sed -n '/^installed/,/^]/p' "$toml" | grep -c "\"$1\""; }
pm -- list >/dev/null # seeds the default packages.toml
before=$(sha256sum <"$toml")
echo "== case 1: remove sys </dev/null"
pm -- remove sys
echo "rc=$?"
[ "$before" = "$(sha256sum <"$toml")" ] && echo "packages.toml unchanged"
echo "sys installed: $(installed sys)"
echo "== case 2: remove sys hbs json pack toml tui xls </dev/null, cargo succeeds"
pm FAKE_CARGO_OK=1 -- remove sys hbs json pack toml tui xls
echo "rc=$?"
echo "sys installed: $(installed sys), json installed: $(installed json)"
echo "== control: the same list with sys last, from the default packages.toml"
rm -f "$toml"; pm -- list >/dev/null
pm FAKE_CARGO_OK=1 -- remove hbs json pack toml tui xls sys
echo "rc=$?"
echo "sys installed: $(installed sys)"
