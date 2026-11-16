#!/bin/bash
# Title: a package whose short name is no Graphix module name (crate
#        graphix-package-2d, graphix-package-select) is accepted by
#        defpackage! and `graphix package add`; register_packages then writes
#        `mod 2d` / `mod select` into the root module source, which cannot parse.
# Command: bash design/review-2026-11-16/repro/package-unparseable-name.sh [GRAPHIX]
#   This shows the half that runs without cargo: the root lines
#   register_packages would emit (`mod <short name>`) refused by the parser.
#   The other half is by reading: defpackage! takes the name from
#   CARGO_CRATE_NAME less `graphix_package_` and never validates it, and
#   add_packages takes any name; only create_package checks
#   `[a-z][-a-z0-9]*` and the reserved words.
# Expected: a name the root cannot parse is refused where the package is
#   built or added.
# Observed: "Unexpected `2`" for `mod 2d`; "can't use keyword as a function or
#   variable name" for `mod select`.
set -u
REAL=${1:-/home/eric/tmp/review-2026-11-16/bin/graphix}
W=$(mktemp -d)
cd $W
echo 'let x = 1;' > 2d.gx
echo 'let x = 1;' > select.gx
printf 'mod 2d;\n1\n' > root_2d.gx
printf 'mod select;\n1\n' > root_select.gx
timeout -s KILL 30 "$REAL" --no-cache --check root_2d.gx; echo "rc=$?"
timeout -s KILL 30 "$REAL" --no-cache --check root_select.gx; echo "rc=$?"
rm -rf $W
