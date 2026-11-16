#!/bin/sh
# toml::write_str's check refuses a root map whose key type is not string (a TOML
# document is a table) but accepts every other root that is no table, and the
# decimal and duration members TomlData admits: each such write fails on every value.
# command: sh small-pkgs-toml-root-check.sh
# expected: the check refuses all five writes, as it refuses toml::write_str({1 => 2})
# observed: `--check` exits 0 for all five; the run prints
#   error:`TomlErr("unsupported i64 type") | error:`TomlErr("unsupported array type") |
#   error:`TomlErr("unsupported str type") | error:`TomlErr("cannot represent decimal in TOML") |
#   error:`TomlErr("cannot represent duration in TOML")
G=/home/eric/tmp/review-2026-11-16/bin/graphix
D=$(mktemp -d)
cat > $D/root.gx <<'GX'
let a = toml::write_str(42);
let b = toml::write_str([1, 2]);
let c = toml::write_str(`Tag);
let d = toml::write_str({d: decimal:1.5});
let e = toml::write_str({t: duration:1.s});
println("[a] | [b] | [c] | [d] | [e]");
sys::exit(sys::time::after_idle(duration:100.ms, 0));
0
GX
cat > $D/map.gx <<'GX'
toml::write_str({1 => 2})
GX
timeout -s KILL 60 $G --check $D/root.gx; echo "check root: $?"
timeout -s KILL 60 $G --check $D/map.gx > $D/out 2>&1; echo "check map: $?"; tail -1 $D/out
timeout -s KILL 60 $G --no-cache $D/root.gx
rm -r $D
