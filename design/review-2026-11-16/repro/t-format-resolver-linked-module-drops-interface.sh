#!/bin/bash
# t-format-resolver: a module file reached through a symlink loses the
# interface beside its real file (the root file keeps it).
#
# Run: bash design/review-2026-11-16/repro/t-format-resolver-linked-module-drops-interface.sh
# Expected: both checks refuse `common::hidden` ("common::hidden not defined"),
#   since shared/common.gxi hides it, whichever path reaches common.gx.
# Observed (e1be2fbf2): through proj/common.gx -> ../shared/common.gx the check
#   passes (rc=0): the module is loaded with no interface, every item public and
#   the interface unchecked; the same module reached directly is refused (rc=1).
#   A ROOT file reached through a link is paired with the .gxi beside its real
#   file (RootFile::load canonicalizes), so the two paths disagree.
set -u
G=${G:-/home/eric/tmp/review-2026-11-16/bin/graphix}
D=$(mktemp -d)
trap 'rm -rf "$D"' EXIT
mkdir -p "$D/shared" "$D/proj"
printf 'let x = 1;\nlet hidden = 2\n' > "$D/shared/common.gx"
printf 'val x: i64\n' > "$D/shared/common.gxi"
ln -s ../shared/common.gx "$D/proj/common.gx"
printf 'mod common;\ncommon::hidden\n' > "$D/proj/main.gx"
cp "$D/proj/main.gx" "$D/shared/main.gx"
echo "== through the link"
timeout -s KILL 30 $G --no-cache -n -i --check "$D/proj/main.gx"; echo "rc=$?"
echo "== directly"
timeout -s KILL 30 $G --no-cache -n -i --check "$D/shared/main.gx"; echo "rc=$?"
