#!/bin/sh
# An old-format packages.toml written by the first package manager (its
# default set: core array str map fs time net re rand tui) migrates with
# every stdlib package that did not exist then recorded as "removed".
# Expect: args db gui hbs http json list pack sqlite toml xls listed under
# "removed stdlib packages" although the user never declined them.
GRAPHIX=${GRAPHIX:-graphix}
D=$(mktemp -d)
mkdir -p "$D/graphix"
cat > "$D/graphix/packages.toml" <<'T'
[packages]
core = "0.4.0"
array = "0.4.0"
str = "0.4.0"
map = "0.4.0"
fs = "0.4.0"
time = "0.4.0"
net = "0.4.0"
re = "0.4.0"
rand = "0.4.0"
tui = "0.4.0"
T
XDG_DATA_HOME="$D" "$GRAPHIX" package list
rm -rf "$D"
