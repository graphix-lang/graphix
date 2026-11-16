#!/bin/bash
# Title: TomlData refuses a non-table TOML document by its type only when the root is a map with
# non-string keys (Type::toml_root_pairs); an i64, an array or an Array<(string, T)> root is
# not a table either, passes the check, and fails at run time.
# Command: bash book-modules-udt-toml-root-only-maps.sh
# Expected: all three refused by the check, as the Map<i64, string> root is
#   ("a TOML document is a table ...").
# Observed: the map root is refused by --check; the other three check, and the run answers
#   error:`TomlErr("unsupported i64 type") / ("unsupported array type").
G=/home/eric/tmp/review-2026-11-16/bin/graphix
D=$(mktemp -d /home/eric/tmp/review-2026-11-16/work/book-modules-udt/toml.XXXX)
printf 'let m: Map<i64, string> = {1 => "a"};\ntoml::write_str(m)\n' > $D/map.gx
printf 'sys::exit(sys::time::after_idle(duration:100.ms, 0));\ntoml::write_str(1)\n' > $D/int.gx
printf 'sys::exit(sys::time::after_idle(duration:100.ms, 0));\nlet a: Array<i64> = [1];\ntoml::write_str(a)\n' > $D/arr.gx
printf 'sys::exit(sys::time::after_idle(duration:100.ms, 0));\nlet p: Array<(string, i64)> = [("a", 1)];\ntoml::write_str(p)\n' > $D/pairs.gx
echo "map root --check:"; timeout -s KILL 20 $G --no-cache --check $D/map.gx 2>&1 | tail -1
for f in int arr pairs; do echo "$f root --check (exit):"; timeout -s KILL 20 $G --no-cache --check $D/$f.gx; echo $?; echo "$f root run:"; timeout -s KILL 20 $G --no-cache $D/$f.gx 2>&1 | tail -1; done
