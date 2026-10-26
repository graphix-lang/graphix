#!/bin/sh
# Title: `graphix package add --path` refuses a package whose version is
#        inherited from its workspace (`version.workspace = true`).
# Command: sh design/review-2026-10-26/repro/package-path-workspace-version.sh
# Expected: the package is accepted (the version is only printed; cargo
#           resolves an inherited version for a path dependency), and the
#           rebuild starts.
# Observed: Error: no version found in .../ws/graphix-package-foo/Cargo.toml
#           (exit 1), before anything is built.
set -e
GX=${GX:-/home/eric/tmp/review-2026-10-26/bin/graphix}
d=$(mktemp -d)
mkdir -p "$d/ws/graphix-package-foo/src"
cat > "$d/ws/Cargo.toml" <<'T'
[workspace]
members = ["graphix-package-foo"]
[workspace.package]
version = "0.1.0"
T
cat > "$d/ws/graphix-package-foo/Cargo.toml" <<'T'
[package]
name = "graphix-package-foo"
version.workspace = true
edition = "2024"
T
cd "$d"
XDG_DATA_HOME="$d/data" timeout -s KILL 60 "$GX" package add foo --path ws/graphix-package-foo || echo "exit $?"
rm -rf "$d"
