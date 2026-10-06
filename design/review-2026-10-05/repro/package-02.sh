#!/usr/bin/env bash
# package-02: build-standalone with a relative --source-override builds where
# the copy never looks. build_standalone (graphix-package/src/lib.rs:1485)
# canonicalizes package_dir (1490) but takes the override as given
# (`dir.to_path_buf()`, 1525). It then runs cargo with
# `.current_dir(&source_dir)` (1557) and `--target-dir source_dir/target`
# (1544), both relative, so cargo resolves the target dir against the
# override: <cwd>/<override>/<override>/target. The copy (1565-1569) reads
# <override>/target/release/graphix against the caller's cwd, i.e.
# <cwd>/<override>/target. Only an override whose `..` cancels (`../x`)
# lands in the same place.
#
# Real cargo is not run: a stand-in `cargo` first on PATH logs its cwd and
# args and writes the "binary" where cargo puts a relative --target-dir,
# against its own working directory (cargo book: CARGO_TARGET_DIR / CLI
# paths are "relative to the current working directory"; --target-dir is
# the same setting). Everything happens under a throwaway temp dir.
#
# command: GRAPHIX=/path/to/graphix bash design/review-2026-10-05/repro/package-02.sh
#
# expected: every case ends "Done!" with mypkg holding the FRESH build, as
#   the absolute-path control does.
# observed (HEAD c722befe, debug build):
#   ../graphix/graphix-shell: cargo cwd=<T>/proj/graphix/graphix-shell,
#     --target-dir ../graphix/graphix-shell/target, build written to
#     <T>/proj/graphix/graphix/graphix-shell/target (a stray tree), then
#     "Error: copying ../graphix/graphix-shell/target/release/graphix to
#     <T>/proj/mypkg/mypkg: No such file or directory" rc=1
#   same, with a binary left at graphix/graphix-shell/target/release/graphix
#     by an earlier build: "Done!" rc=0 and mypkg holds that STALE binary
#     (`package add`/`rebuild` leave a plain graphix in exactly that spot of
#     the unpacked tree: `cargo install --path` builds into <path>/target)
#   shell (a plain relative name): build in shell/shell/target, copy fails rc=1
#   absolute path: Done, FRESH (control); ../shell: Done, FRESH (cancels)
set -u
G=${GRAPHIX:-graphix}
T=$(mktemp -d)
trap 'rm -rf "$T"' EXIT
export HOME=$T/home XDG_CACHE_HOME=$T/cache XDG_DATA_HOME=$T/data \
    XDG_CONFIG_HOME=$T/config CARGO_HOME=$T/cargo-home
mkdir -p "$HOME" "$T/fakebin"
cat > "$T/fakebin/cargo" <<'EOF'
#!/usr/bin/env bash
td= prev=
for a in "$@"; do [ "$prev" = --target-dir ] && td=$a; prev=$a; done
echo "  [cargo] cwd=$(pwd -P) args: $*"
case $td in /*) ;; *) td=$(pwd -P)/$td ;; esac
mkdir -p "$td/release" && echo FRESH > "$td/release/graphix"
echo "  [cargo] built $(realpath -m "$td/release/graphix")"
EOF
chmod +x "$T/fakebin/cargo"
export PATH=$T/fakebin:$PATH

mkpkg() {
    rm -rf "$T/proj"
    mkdir -p "$T/proj/mypkg" "$1"
    printf '[package]\nname = "graphix-package-mypkg"\nversion = "0.1.0"\n\n[dependencies]\n\n[features]\nstandalone = []\n' \
        > "$T/proj/mypkg/Cargo.toml"
    printf '[package]\nname = "graphix-shell"\nversion = "0.1.0"\n\n[dependencies]\n' \
        > "$1/Cargo.toml"
}
run() { # $1 = the override as typed
    echo "  (cwd $T/proj/mypkg) graphix package build-standalone --source-override $1"
    (cd "$T/proj/mypkg" &&
        timeout -s KILL 60 "$G" package build-standalone --source-override "$1" \
            > "$T/out" 2>&1)
    local rc=$?
    grep -v '^Updating\|^Building\|^$' "$T/out" | sed 's/^/  /'
    echo "  rc=$rc, mypkg: $(cat "$T/proj/mypkg/mypkg" 2>/dev/null || echo '<missing>')"
}

echo "=== ../graphix/graphix-shell (a package beside a graphix checkout)"
mkpkg "$T/proj/graphix/graphix-shell"
run ../graphix/graphix-shell
echo "  stray tree: $(cd "$T/proj" && find graphix/graphix -name graphix -type f)"

echo "=== same, after an earlier build left target/release/graphix in the source tree"
mkpkg "$T/proj/graphix/graphix-shell"
mkdir -p "$T/proj/graphix/graphix-shell/target/release"
echo STALE > "$T/proj/graphix/graphix-shell/target/release/graphix"
run ../graphix/graphix-shell

echo "=== shell (a plain relative name)"
mkpkg "$T/proj/mypkg/shell"
run shell

echo "=== control: the same tree by absolute path"
mkpkg "$T/proj/graphix/graphix-shell"
run "$T/proj/graphix/graphix-shell"

echo "=== control: ../shell (its .. cancels)"
mkpkg "$T/proj/shell"
run ../shell
