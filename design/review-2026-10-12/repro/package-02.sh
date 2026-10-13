#!/bin/sh
# package-02: old-format migration records every stdlib package newer than
# the file as "removed".
# Command: GRAPHIX=<graphix binary> sh design/review-2026-10-12/repro/package-02.sh
# The file below is what the first package manager (6d553ef0) wrote by
# default; the old format was written once at creation and never grew.
# Expected: args db gui hbs http json list pack sqlite toml xls untracked
#   (neither installed nor removed), so reconcile/update announce them.
# Observed: `graphix package list` prints them all under
#   "removed stdlib packages:", so reconcile's note and update's new_stdlib
#   skip them and the user is never told they exist.
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
