#!/bin/sh
# Title: reduced_feature_build_drops_packages' negative checks cannot fail
# The test asserts `!check("use http\n")` and `!check("use sqlite\n")` to show
# the dropped features are gone. A program that is only a `use` statement is
# refused by EVERY build, the full one included, so both asserts pass whether
# or not the feature drop worked.
# Command: sh design/review-2026-10-19/repro/package-vacuous-feature-drop-test.sh
# Expected: the full (all-features) graphix accepts `use http` (so a refusal
#           in the reduced build would mean the package is gone).
# Observed: the full graphix refuses `use http` with "a use declaration is not
#           an expression"; `use http;\n1` is accepted (exit 0).
G=${G:-/home/eric/tmp/review-2026-10-19/bin/graphix}
D=$(mktemp -d)
printf 'use http\n' > "$D/a.gx"
printf 'use http;\n1\n' > "$D/b.gx"
timeout -s KILL 60 "$G" --check "$D/a.gx"; echo "use http      -> exit $?"
timeout -s KILL 60 "$G" --check "$D/b.gx"; echo "use http; 1   -> exit $?"
rm -rf "$D"
