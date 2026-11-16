#!/bin/bash
# Title: `graphix package build-standalone` is refused when the running graphix
#        is a cargo build output, though a standalone build never replaces it.
# Command: bash design/review-2026-11-16/repro/package-standalone-target-exe.sh [GRAPHIX]
#   GRAPHIX defaults to the review's prebuilt debug binary. Everything runs in a
#   throwaway sandbox: a fake `cargo` on PATH (logs, never builds), a fake cargo
#   home holding the shell source, private HOME/XDG dirs, network blocked.
# Expected: both runs print "Done! Binary written to .../foo" (the exe's location
#   is irrelevant to a standalone build).
# Observed: the run from inside a dir with a `.cargo-lock` marker fails before
#   unpacking: "Error: cannot replace .../faketarget/release/graphix inside a
#   Cargo target directory; copy the executable outside the build directory
#   before rebuilding packages"; the same binary elsewhere succeeds.
#   Cause: GraphixPM::prepare_source runs check_rebuild_exe, and
#   build_standalone (no --source-override) calls prepare_source.
set -u
REAL=${1:-/home/eric/tmp/review-2026-11-16/bin/real/graphix}
SHELL_TOML=$(dirname "$0")/../../../graphix-shell/Cargo.toml
W=$(mktemp -d)
mkdir -p $W/fakebin $W/home $W/data $W/pkgs $W/elsewhere $W/faketarget/release \
  $W/cargo_home/registry/src/index.crates.io-0000000000000000/graphix-shell-0.9.0/src
cp "$SHELL_TOML" $W/cargo_home/registry/src/index.crates.io-0000000000000000/graphix-shell-0.9.0/Cargo.toml
echo 'fn main(){}' > $W/cargo_home/registry/src/index.crates.io-0000000000000000/graphix-shell-0.9.0/src/main.rs
echo built > $W/fakebuilt
cat > $W/fakebin/cargo <<EOF
#!/bin/bash
case "\$1" in
  metadata) echo '{"packages":[],"resolve":{"nodes":[],"root":null}}' ;;
  build) echo '{"reason":"compiler-artifact","target":{"name":"graphix","kind":["bin"]},"executable":"$W/fakebuilt"}' ;;
esac
EOF
chmod +x $W/fakebin/cargo
cp "$REAL" $W/elsewhere/graphix
cp "$REAL" $W/faketarget/release/graphix
touch $W/faketarget/release/.cargo-lock
run() {
  env HOME=$W/home XDG_DATA_HOME=$W/data CARGO_HOME=$W/cargo_home PATH=$W/fakebin:/usr/bin:/bin \
    https_proxy=http://127.0.0.1:9 HTTPS_PROXY=http://127.0.0.1:9 ALL_PROXY=http://127.0.0.1:9 \
    timeout -s KILL 60 "$@" </dev/null
}
run $W/elsewhere/graphix package create foo --dir $W/pkgs
cd $W/pkgs/graphix-package-foo && echo 'println("hi")' > src/graphix/main.gx
echo "== from a cargo target dir"
run $W/faketarget/release/graphix package build-standalone; echo "rc=$?"
echo "== the same binary elsewhere"
run $W/elsewhere/graphix package build-standalone; echo "rc=$?"
rm -rf $W
