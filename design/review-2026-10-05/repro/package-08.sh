#!/usr/bin/env bash
# package-08: graphix-package's slow tests delete the developer's
# <workspace>/.cargo/config.toml.
#
# vendor() (graphix-package/src/test.rs:220-233), reached by
# created_package_compiles and build_standalone_produces_working_binary,
# runs vendor.py and then removes <ws>/.cargo/config.toml unconditionally,
# saying vendor.py wrote it. vendor.py (main, 301-313) only creates .cargo/
# and prints the snippet, so the file it deletes is always the developer's
# own, and .gitignore excludes .cargo, so git cannot restore it.
#
# This runs the REAL vendor.py and the REAL slow test, from a prebuilt
# graphix-package unit-test binary, against a throwaway copy of the
# workspace: inside `unshare -Urm` the copy is bind-mounted over the
# workspace path compiled into the binary and $HOME is read-only; `cargo`
# is a stub on PATH that logs and exits 0 (cargo vendor never writes a
# config either), so nothing is downloaded, built or touched outside the
# copy.
#
# command: bash design/review-2026-10-05/repro/package-08.sh [test-binary]
#   (default: the newest ${CARGO_TARGET_DIR:-~/tmp/target}/debug/deps/
#    graphix_package-<hash>; `cargo test -p graphix-package --lib --no-run`
#    builds one)
#
# expected: the test leaves a file it did not create alone:
#   after vendor.py alone: .cargo/config.toml PRESENT
#   after test::created_package_compiles (passed): .cargo/config.toml PRESENT
# observed (HEAD c722befe, debug test binary):
#   after vendor.py alone: .cargo/config.toml PRESENT
#   after test::created_package_compiles (passed): .cargo/config.toml DELETED
#   stub cargo calls: vendor vendor/;vendor vendor/;check;
set -euo pipefail

if [ "${1:-}" = --inside ]; then
    T=$2 WS=$3 BIN=$4
    mount --bind "$T/ws" "$WS"
    mount --bind "$T" "$T"
    mount --rbind "$HOME" "$HOME"
    mount -o remount,bind,ro "$HOME"
    findmnt -no OPTIONS "$HOME" | grep -q '^ro' || { echo "ABORT: HOME not read-only"; exit 97; }
    findmnt -T "$WS/vendor.py" -no OPTIONS | grep -q '^rw' || { echo "ABORT: copy not writable"; exit 96; }
    if [ ! -f "$WS/SANDBOX_MARKER" ] || [ -e "$WS/design" ]; then
        echo "ABORT: the binary would not see the copy"
        exit 95
    fi
    mkdir -p "$WS/.cargo"
    cat > "$WS/.cargo/config.toml" <<'EOF'
# the developer's own project config: the snippet vendor.py asks them to add
[source.crates-io]
replace-with = "vendored-sources"

[source.vendored-sources]
directory = "vendor"
EOF
    export PATH="$T/bin:$PATH" HOME="$T/home" CARGO_HOME="$T/home/.cargo"
    export TMPDIR="$T/tmp" STUB_LOG="$T/cargo.log"
    state() { if [ -f "$WS/.cargo/config.toml" ]; then echo PRESENT; else echo DELETED; fi; }
    (cd "$WS" && python3 vendor.py > "$T/vendor.out")
    echo "after vendor.py alone: .cargo/config.toml $(state)"
    "$BIN" --include-ignored --exact test::created_package_compiles > "$T/test.out" 2>&1 || true
    result=$(grep -o 'test result: [a-zA-Z]*' "$T/test.out" | sed 's/test result: //')
    echo "after test::created_package_compiles ($result): .cargo/config.toml $(state)"
    echo "stub cargo calls: $(sed 's/^stub cargo: //' "$STUB_LOG" | tr '\n' ';')"
    exit 0
fi

WS=$(cd "$(dirname "$0")/../../.." && pwd -P)
TARGET=${CARGO_TARGET_DIR:-$HOME/tmp/target}
BIN=${1:-$(ls -t "$TARGET"/debug/deps/graphix_package-* 2>/dev/null | grep -v '\.d$' | head -1 || true)}
if [ -z "$BIN" ] || [ ! -x "$BIN" ]; then
    echo "no graphix-package unit-test binary: cargo test -p graphix-package --lib --no-run"
    exit 2
fi
grep -qaF "$WS/graphix-package" "$BIN" || { echo "$BIN was not built from $WS"; exit 2; }
T=$(mktemp -d)
T=$(cd "$T" && pwd -P)
trap 'rm -rf "$T"' EXIT
mkdir -p "$T/ws" "$T/bin" "$T/home" "$T/tmp"
members=$(python3 -c 'import sys, tomllib
print(" ".join(tomllib.load(open(sys.argv[1], "rb"))["workspace"]["members"]))' "$WS/Cargo.toml")
# shellcheck disable=SC2086
git -C "$WS" archive --format=tar HEAD Cargo.toml vendor.py $members | tar -x -C "$T/ws"
echo "throwaway copy of $WS at $(git -C "$WS" rev-parse --short HEAD)" > "$T/ws/SANDBOX_MARKER"
printf '#!/bin/sh\necho "stub cargo: $*" >> "$STUB_LOG"\nexit 0\n' > "$T/bin/cargo"
chmod +x "$T/bin/cargo"
echo "test binary: $BIN"
unshare -Urm --propagation private bash "$0" --inside "$T" "$WS" "$BIN"
