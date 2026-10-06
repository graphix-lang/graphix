#!/usr/bin/env bash
# package-05: add/remove/rebuild pass packages.toml's stdlib set to cargo
# unreconciled with the shell source they build.
#
# `update` alone filters [stdlib].installed against the build source's set
# (graphix-package/src/lib.rs:1674-1675) and then writes the UNFILTERED set
# back (1679, "e.g. removed upstream; keep the recorded intent").
# add_packages (1338), remove_packages (1412) and do_rebuild (1449) pass
# Packages::build_plan (285) straight to `cargo install --features`, so a
# recorded name the source no longer ships is an unknown feature and every
# later add/remove/rebuild fails. `remove <name>` cannot clear it:
# is_stdlib_package (240) is false for a name this binary does not ship, so
# it is looked up in `external` and reported "not installed" (1403-1407);
# `update` at the latest shell returns "already up to date" before writing
# (1637-1640). `time` stands in for the dropped package (fs/net/time were
# once stdlib packages, folded into sys): neither this binary nor its
# source knows it, exactly as release N+1 would not know a package N+1
# dropped. Case 2 is the reverse: a shipped stdlib package that the file
# never recorded (written by an older binary, shell then upgraded with
# `cargo install graphix-shell`) is left out of every rebuild; `update`
# offers new stdlib packages only at a shell bump (1606-1613, 791-799).
#
# Everything runs under a mktemp dir: HOME/XDG_*/CARGO_HOME point there; a
# fake `graphix` (for `graphix --version`) and a fake `cargo` come first on
# PATH; the fake cargo's registry/src holds a copy of graphix-shell/
# Cargo.toml. The fake cargo logs its args and applies cargo's own check
# for the root package (an unknown --features name is an error, exit 101:
# "the package '..' does not contain this feature: ..", the wording in the
# installed cargo binary; cargo's changelog #9437 made it an error for every
# non-virtual package); it compiles nothing. The real packages.toml is
# never touched.
#
# command: GRAPHIX=/path/to/graphix bash design/review-2026-10-05/repro/package-05.sh
#
# expected: case 1 rebuilds without `time` (or reports it), and
#   `remove time` clears it; case 2 builds `list`, or reports that it is
#   shipped but unrecorded.
# observed (HEAD c722befe, debug build):
#   case 1 rebuild: cargo got `--features sys time`, "does not contain
#     this feature: time", "Error: cargo install failed with status exit
#     status: 101" rc=1
#   case 1 add gui: same `--features gui sys time`, rc=1, gui still removed
#   case 1 remove time: "time is not installed" / "No changes needed." rc=0
#   case 1 remove sys: `--features time`, rc=1
#   case 1 list: "stdlib packages: core sys time"
#   control (time deleted by hand): rebuild "Done!" rc=0
#   case 2 rebuild: cargo got `--features args array db gui hbs http json
#     map pack rand re sqlite str sys toml tui xls` (no list), "Done!"
#     rc=0; `package list` names `list` neither installed nor removed
set -u
GRAPHIX=${GRAPHIX:-graphix}
repo=${REPO:-$(cd "$(dirname "$0")/../../.." && pwd)}
ver=$(sed -n 's/^version = "\(.*\)"/\1/p' "$repo/graphix-shell/Cargo.toml" | head -1)
T=$(mktemp -d)
trap 'rm -rf "$T"' EXIT
mkdir -p "$T/home" "$T/fakebin" "$T/cargo/bin" "$T/cargo/registry/src/idx/graphix-shell-$ver"
cp "$repo/graphix-shell/Cargo.toml" "$T/cargo/registry/src/idx/graphix-shell-$ver/Cargo.toml"
printf '#!/bin/bash\necho "graphix %s"\n' "$ver" > "$T/fakebin/graphix"
cat > "$T/cargo/bin/cargo" <<'EOF'
#!/bin/bash
echo "  [fake cargo] $*"
path= feats=
while [ $# -gt 0 ]; do
    case $1 in --path) path=$2; shift ;; --features) feats=$2; shift ;; esac
    shift
done
python3 - "$path/Cargo.toml" $feats <<'PY'
import sys, tomllib
m = tomllib.load(open(sys.argv[1], "rb"))
p = m["package"]
for f in sys.argv[2:]:
    if f not in m.get("features", {}):
        print(f"  [fake cargo] error: the package '{p['name']}' does not "
              f"contain this feature: {f}")
        sys.exit(101)
print("  [fake cargo] build ok")
PY
EOF
chmod +x "$T/fakebin/graphix" "$T/cargo/bin/cargo"
toml=$T/data/graphix/packages.toml
pm() {
    env PATH="$T/fakebin:$T/cargo/bin:$PATH" HOME="$T/home" CARGO_HOME="$T/home/cargo" \
        XDG_DATA_HOME="$T/data" XDG_CACHE_HOME="$T/cache" XDG_CONFIG_HOME="$T/config" \
        timeout -s KILL 60 "$GRAPHIX" package "$@" </dev/null 2>&1 |
        grep -v '^Unpacking\|^Updating\|^Building'
    echo "  rc=${PIPESTATUS[0]}"
}
state() { mkdir -p "$(dirname "$toml")"; printf '%s\n' "$@" > "$toml"; }
show() { echo "  packages.toml: $(tr -s '\n ' ' ' < "$toml")"; }

echo "== case 1: [stdlib].installed holds a name the source does not ship"
state '[stdlib]' 'installed = ["core", "sys", "time"]' 'removed = ["gui"]' '' '[packages]'
echo "-- rebuild"; pm rebuild
echo "-- add gui"; pm add gui; show
echo "-- remove time"; pm remove time; show
echo "-- remove sys"; pm remove sys
echo "-- list"; pm list
echo "-- control: the same file with time deleted by hand"
state '[stdlib]' 'installed = ["core", "sys"]' 'removed = ["gui"]' '' '[packages]'
pm rebuild

echo "== case 2: a shipped stdlib package (list) that the file never recorded"
state '[stdlib]' \
    'installed = ["args", "array", "core", "db", "gui", "hbs", "http", "json", "map",' \
    '  "pack", "rand", "re", "sqlite", "str", "sys", "toml", "tui", "xls"]' \
    'removed = []' '' '[packages]'
echo "-- rebuild"; pm rebuild
echo "-- list"; pm list | grep -c '^  list$' | sed 's/^/  lines naming list: /'
