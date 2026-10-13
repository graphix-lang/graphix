#!/bin/bash
# package-03: an external package whose name the shell source ships as stdlib
# overwrites the shell's optional stdlib dependency.
#
# Run: bash design/review-2026-10-12/repro/package-03.sh [path/to/graphix]
# Sandbox only: XDG_DATA_HOME/HOME point into a temp dir, and `cargo` and
# `graphix` on PATH are fakes (the fake cargo prints the manifest it is handed
# and fails, so nothing is built or installed). The fake registry holds a
# graphix-shell-0.9.0 source shipping graphix-package-foo as optional stdlib
# (feature foo = ["dep:graphix-package-foo"]); packages.toml has external
# foo = "1.0.0" (what an older shell, which did not ship foo, let the user add).
#
# Expected: the external collides with a shipped stdlib name and is refused or
#   dropped with a message (reconcile), the shell's optional entry left alone.
# Observed: `graphix package rebuild` hands cargo a manifest with
#   graphix-package-foo = "1.0.0"   (optional = true gone)
# beside foo = ["dep:graphix-package-foo"], which cargo refuses at manifest
# load ("feature `foo` includes `dep:graphix-package-foo`, but
# `graphix-package-foo` is not an optional dependency"), so every later add,
# remove, rebuild against that shell fails the same way.
set -e
GX=${1:-graphix}
GX=$(command -v "$GX" || echo "$GX")
W=$(mktemp -d)
trap 'rm -rf "$W"' EXIT
SRC=$W/fakecargo/registry/src/index.crates.io-x/graphix-shell-0.9.0
mkdir -p $W/fakecargo/bin $SRC $W/data/graphix $W/home
cat > $SRC/Cargo.toml <<'T'
[package]
name = "graphix-shell"
version = "0.9.0"
edition = "2024"

[features]
default = ["all"]
all = ["array", "foo"]
array = ["dep:graphix-package-array"]
foo = ["dep:graphix-package-foo"]

[dependencies]
graphix-package = { version = "0.9.0" }
graphix-package-core = { version = "0.9.0" }
graphix-package-array = { version = "0.9.0", optional = true }
graphix-package-foo = { version = "0.9.0", optional = true }
T
cat > $W/fakecargo/bin/cargo <<'T'
#!/bin/bash
echo "[fake cargo] args: $*"
for a in "$@"; do [ -f "$a/Cargo.toml" ] && { echo "[fake cargo] manifest:"; cat "$a/Cargo.toml"; }; done
exit 1
T
printf '#!/bin/bash\necho "graphix 0.9.0"\n' > $W/fakecargo/bin/graphix
chmod +x $W/fakecargo/bin/*
cat > $W/data/graphix/packages.toml <<'T'
[stdlib]
installed = ["core", "array"]
removed = []

[packages]
foo = "1.0.0"
T
cd $W
env HOME=$W/home XDG_DATA_HOME=$W/data XDG_CACHE_HOME=$W/cache \
  PATH=$W/fakecargo/bin:/usr/bin:/bin timeout -s KILL 60 "$GX" package rebuild || true
