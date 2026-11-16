#!/bin/sh
# Title: a misplaced #[native] (on a function binding) is refused only by a
# fused build; --check and --no-fusion accept it.
# Command: sh tests-lib-b-a-native-placement-check.sh
# Expected (one rule everywhere): all three refuse with "#[native] annotates a
#   computation or a call, not a function".
# Observed: --check exits 0 silently; --no-fusion runs and prints 3; the default
#   (fused) build refuses. By contrast `#[native(1)]` is refused by --check.
G=${GRAPHIX:-/home/eric/tmp/review-2026-11-16/bin/graphix}
F=$(mktemp --suffix=.gx)
printf '{ #[native]\nlet f = |x: i64| x + i64:1; f(i64:2) }\n' > "$F"
echo "--- --check"; timeout -s KILL 20 "$G" --no-cache --check "$F"; echo "exit $?"
echo "--- --no-fusion"; timeout -s KILL 5 "$G" --no-cache --no-fusion "$F"; echo "exit $? (137 = ran until killed)"
echo "--- fused build"; timeout -s KILL 20 "$G" --no-cache --expand "$F" 2>&1 | tail -1; echo "exit $?"
rm -f "$F"
