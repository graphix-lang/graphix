#!/bin/sh
# A module file under a directory whose name is not UTF-8 never has its
# submodules resolved (resolver.rs resolve_modules_level: `p.to_str()`
# is None -> `return Ok(None)`); the compile then reports
# "external modules are not allowed in this context" at `mod x`.
# usage: t-format-resolver-non-utf8-dir.sh /path/to/graphix
G=${1:-graphix}
T=$(mktemp -d)
D="$T/$(printf 'd\377')"
mkdir -p "$D/m"
printf 'mod m;\nm::x::k\n' > "$D/main.gx"
printf 'mod x;\n' > "$D/m.gx"
printf 'let k = 1\n' > "$D/m/x.gx"
cd "$D" && timeout -s KILL 20 "$G" --check main.gx
echo "exit $? (expected 0)"
cd / && rm -rf "$T"
