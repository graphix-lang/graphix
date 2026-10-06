#!/usr/bin/env bash
# package-04: build-standalone passes every graphix-package-* dep as a
# shell feature, third-party too.
#
# GraphixPM::build_standalone (graphix-package/src/lib.rs:1514) turns
# every `graphix-package-<x>` key of the package's [dependencies] except
# core into `cargo build --features <x>`, through
# stdlib_packages_in_cargo_toml, which (despite its name) keeps every
# such key. graphix-shell has features only for the stdlib packages, so a
# package that depends on a third-party graphix package cannot be built
# standalone. A package whose short name is a stdlib name (a local fork
# of json) is written over the shell's optional dep by update_cargo_toml
# (line 1513, 1539), leaving the shell's `json = ["dep:..."]` feature
# pointing at a non-optional dep.
#
# Stage 1 runs against a copy of the real graphix-shell/Cargo.toml with a
# fake `cargo` that prints its argv. Stage 2 runs real cargo (offline,
# CARGO_HOME in the temp dir) over a mock shell with the real shell's
# feature shape (core always, sys and json optional, json enabling sys)
# and empty path crates, so nothing but a few empty crates is compiled.
#
# command: GRAPHIX=/path/to/graphix bash design/review-2026-10-05/repro/package-04.sh
#
# expected: the third-party package builds like the stdlib-only control
#   (its widgets dep compiles as mypkg's dependency and mypkg's register
#   registers it); the json fork is built or refused by build-standalone
#   with a message of its own.
#
# observed (HEAD c722befe, debug build, cargo 1.98.1):
#   stage 1: fake cargo argv ... [--no-default-features] [--features]
#     [sys widgets graphix-package-mypkg/standalone]
#   stage 2, plain (core + sys): "Done! Binary written to .../plain", exit 0
#   stage 2, mypkg (core + sys + widgets):
#     "error: the package 'graphix-shell' does not contain this feature: widgets"
#     "Error: cargo build --release failed with status exit status: 101", exit 1
#   stage 2, json fork: "error: failed to parse manifest ... feature `json`
#     includes `dep:graphix-package-json`, but `graphix-package-json` is not
#     an optional dependency", exit 1
set -u
GRAPHIX=${GRAPHIX:-graphix}
repo=$(cd "$(dirname "$0")/../../.." && pwd)
real_cargo=$(rustup which cargo 2>/dev/null || command -v cargo)
dir=$(mktemp -d)
trap 'rm -rf "$dir"' EXIT
mkdir -p "$dir/home"
run() { # run <pkg dir> <path dirs> <source override>
    (cd "$1" && env PATH="$2:$PATH" HOME="$dir/home" CARGO_HOME="$dir/home/cargo" \
        CARGO_NET_OFFLINE=true XDG_DATA_HOME="$dir/home/data" \
        XDG_CACHE_HOME="$dir/home/cache" XDG_CONFIG_HOME="$dir/home/config" \
        timeout -s KILL 180 "$GRAPHIX" package build-standalone --source-override "$3" 2>&1)
    echo "exit=$?"
}

echo "== stage 1: the real shell manifest, fake cargo"
mkdir -p "$dir/s1/fakebin" "$dir/s1/shell" "$dir/s1/mypkg"
cp "$repo/graphix-shell/Cargo.toml" "$dir/s1/shell/Cargo.toml"
cat > "$dir/s1/fakebin/cargo" <<'EOF'
#!/bin/bash
printf 'fake cargo argv:'; for a in "$@"; do printf ' [%s]' "$a"; done; echo
exit 101
EOF
chmod +x "$dir/s1/fakebin/cargo"
cat > "$dir/s1/mypkg/Cargo.toml" <<'EOF'
[package]
name = "graphix-package-mypkg"
version = "0.1.0"
edition = "2024"

[dependencies]
graphix-package-core = "0.9.0"
graphix-package-sys = "0.9.0"
graphix-package-widgets = "0.3.0"

[features]
standalone = []
EOF
run "$dir/s1/mypkg" "$dir/s1/fakebin" "$dir/s1/shell"

echo "== stage 2: real cargo, mock shell with the shell's feature shape"
s2=$dir/s2
for c in core sys json widgets; do
    mkdir -p "$s2/crates/$c/src"
    printf '[package]\nname = "graphix-package-%s"\nversion = "0.9.0"\nedition = "2021"\n' "$c" \
        > "$s2/crates/$c/Cargo.toml"
    : > "$s2/crates/$c/src/lib.rs"
done
mkdir -p "$s2/shell/src"
echo 'fn main() {}' > "$s2/shell/src/main.rs"
cat > "$s2/shell/Cargo.toml" <<EOF
[package]
name = "graphix-shell"
version = "0.9.0"
edition = "2021"

[workspace]

[[bin]]
name = "graphix"
path = "src/main.rs"

[features]
default = ["all"]
all = ["sys", "json"]
sys = ["dep:graphix-package-sys"]
json = ["dep:graphix-package-json", "sys"]

[dependencies]
graphix-package-core = { version = "0.9.0", path = "$s2/crates/core" }
graphix-package-sys = { version = "0.9.0", path = "$s2/crates/sys", optional = true }
graphix-package-json = { version = "0.9.0", path = "$s2/crates/json", optional = true }
EOF
pkg() { # pkg <dir> <crate name> <dep short names..>
    local d=$1 n=$2; shift 2
    mkdir -p "$s2/$d/src"
    : > "$s2/$d/src/lib.rs"
    {
        printf '[package]\nname = "%s"\nversion = "0.1.0"\nedition = "2021"\n\n[features]\nstandalone = []\n\n[dependencies]\n' "$n"
        for x in "$@"; do
            printf 'graphix-package-%s = { version = "0.9.0", path = "%s/crates/%s" }\n' "$x" "$s2" "$x"
        done
    } > "$s2/$d/Cargo.toml"
}
pkg plain graphix-package-plain core sys
pkg mypkg graphix-package-mypkg core sys widgets
pkg jsonfork graphix-package-json core sys
for p in plain mypkg jsonfork; do
    echo "-- $p"
    rm -rf "$s2/shell-$p"
    cp -r "$s2/shell" "$s2/shell-$p"
    run "$s2/$p" "$(dirname "$real_cargo")" "$s2/shell-$p"
done
