#!/bin/sh
# A directory symlink makes a real import cycle (m.gx's `mod m` is m/m.gx,
# which is m.gx again). LoadChain compares lexical Source::File paths, which
# always deepen, so it never fires: the load runs 40 levels deep and fails
# with ELOOP ("Too many levels of symbolic links") instead of "import cycle".
# usage: t-format-resolver-symlink-cycle.sh /path/to/graphix
G=${1:-graphix}
T=$(mktemp -d)
cd "$T" || exit 1
ln -s . m
printf 'mod m;\n1\n' > main.gx
printf 'mod m;\nlet a = 1\n' > m.gx
timeout -s KILL 30 "$G" --check main.gx 2>&1 | cut -c1-300
rm -rf "$T"
