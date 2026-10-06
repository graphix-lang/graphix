#!/usr/bin/env bash
# package-03: build-standalone --source-override edits the override's
# Cargo.toml in place and never restores it.
#
# GraphixPM::build_standalone (graphix-package/src/lib.rs:1524-1540) uses
# the override directory itself as the source tree and fs::writes the
# `graphix-package-<pkg> = { path = .. }` dep into its Cargo.toml before
# building, on every exit path. The CLI help (graphix-shell/src/main.rs:
# 109-112) suggests the workspace as the override, i.e.
# `--source-override ~/proj/graphix/graphix-shell`. This script stands a
# copy of graphix-shell/Cargo.toml in for that tree (the checkout is only
# read) and puts a fake `cargo` first on PATH, so nothing is compiled.
#
# command: GRAPHIX=/path/to/graphix bash design/review-2026-10-05/repro/package-03.sh
#
# expected: after every run the override's Cargo.toml is unchanged (each
# diff prints nothing).
#
# observed (HEAD c722befe, debug build):
#   run 1 (mypkg, build ok): `> graphix-package-mypkg = { path = "<tmp>/mypkg" }`
#   run 2 (otherpkg, same override): both the mypkg and the otherpkg
#     lines; fake cargo builds otherpkg with `--features sys
#     graphix-package-otherpkg/standalone` over a manifest that still
#     holds mypkg as a non-optional dep, which packages!()
#     (graphix-derive/src/lib.rs:475) registers in otherpkg's binary
#   run 3 (manifest reset, cargo fails with 101): the mypkg line is
#     still there after `Error: cargo build --release failed`
set -u
GRAPHIX=${GRAPHIX:-graphix}
repo=$(cd "$(dirname "$0")/../../.." && pwd)
dir=$(mktemp -d)
trap 'rm -rf "$dir"' EXIT
mkdir -p "$dir/fakebin" "$dir/shell" "$dir/home"
cp "$repo/graphix-shell/Cargo.toml" "$dir/shell/Cargo.toml"
cp "$dir/shell/Cargo.toml" "$dir/orig.toml"
for p in mypkg otherpkg; do
    mkdir -p "$dir/$p"
    printf '[package]\nname = "graphix-package-%s"\nversion = "0.1.0"\nedition = "2024"\n\n[features]\nstandalone = []\n\n[dependencies]\ngraphix-package-core = "0.9.0"\ngraphix-package-sys = "0.9.0"\n' "$p" > "$dir/$p/Cargo.toml"
done
cat > "$dir/fakebin/cargo" <<'EOF'
#!/bin/bash
echo "fake cargo: $*"
[ -n "${FAKE_CARGO_FAIL:-}" ] && exit 101
while [ $# -gt 0 ]; do [ "$1" = --target-dir ] && td=$2; shift; done
mkdir -p "$td/release" && : > "$td/release/graphix"
EOF
chmod +x "$dir/fakebin/cargo"
run() { # run <pkg> [env..]
    local p=$1; shift
    (cd "$dir/$p" && env PATH="$dir/fakebin:$PATH" HOME="$dir/home" \
        XDG_DATA_HOME="$dir/home/data" XDG_CACHE_HOME="$dir/home/cache" \
        XDG_CONFIG_HOME="$dir/home/config" CARGO_HOME="$dir/home/cargo" "$@" \
        timeout -s KILL 60 "$GRAPHIX" package build-standalone \
        --source-override "$dir/shell")
    echo "exit=$?; diff of the override's Cargo.toml:"
    diff "$dir/orig.toml" "$dir/shell/Cargo.toml"
}
echo "== run 1: mypkg"; run mypkg
echo "== run 2: otherpkg, same override"; run otherpkg
cp "$dir/orig.toml" "$dir/shell/Cargo.toml"
echo "== run 3: mypkg, cargo fails"; run mypkg FAKE_CARGO_FAIL=1
