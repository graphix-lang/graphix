#!/bin/sh
# Title: two accepted sys.rs fixtures are marked `; build_only`, dropping the check-vs-build test
#
# `run!`'s `; build_only` form (graphix-package-core testing.rs) exists for
# "a refusal only a build makes" and skips the interp run's check of the
# program alone. handle_accessors_are_sync and born_builtins_and_parse_are_sync
# are accepted by the check AND the build, so the exemption only removes
# the assertion that the two agree.
#
# Command: sh design/review-2026-11-16/repro/tests-lib-b-c-build-only-accepted.sh
# Expected (for build_only to be needed): the check refuses or the build refuses.
# Observed: both programs pass `--check` and `--expand` (no output, exit 0).

B=/home/eric/tmp/review-2026-11-16/bin
w=$(mktemp -d)
cat > "$w/accessors.gx" <<'EOF'
#[sync]
let peer = |s: sys::tcp::TcpStream| sys::tcp::Socket::peer_addr(s);
#[sync]
let local = |s: sys::tcp::TcpStream| sys::tcp::Socket::local_addr(s);
#[sync]
let listener = |l: sys::tcp::TcpListener| sys::tcp::listener_addr(l);
#[sync]
let pid = |p: sys::process::Proc| sys::process::pid(p);
(peer(never()), local(never()), listener(never()), pid(never()));
true
EOF
cat > "$w/born.gx" <<'EOF'
#[sync]
let cfg = |name: string| "[sys::dirs::config_dir()]/[name]";
#[sync]
let argv = |n: i64| (n, sys::args());
#[sync]
let parse = |c: args::Command| args::parse(c);
(cfg("x.toml"), argv(1), parse(never()));
true
EOF
for f in accessors born; do
    timeout -s KILL 30 "$B/graphix" --check "$w/$f.gx"; echo "$f --check exit=$?"
    timeout -s KILL 60 "$B/graphix" --no-cache --expand "$w/$f.gx"; echo "$f --expand exit=$?"
done
rm -rf "$w"
