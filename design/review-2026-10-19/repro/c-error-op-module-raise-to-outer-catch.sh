#!/bin/sh
# Title: a module with an interface cannot raise to a catch outside it, even
# when the catch is annotated with exactly what the module raises.
# Command: sh c-error-op-module-raise-to-outer-catch.sh [path/to/graphix]
# Expected: --check accepts (the catch's ascription covers the raise, so
#   join_raised has nothing to write); deleting m.gxi is accepted today.
# Observed: "compiling module m: the check decides a type the code around the
#   module left open: annotate the binding it decides". The catch's type is
#   ascribed in Catch::typecheck0, which a block runs after the module's
#   check, so join_raised writes the catch's bind cell from the module task.
G=${1:-/home/eric/tmp/review-2026-10-19/bin/graphix}
D=$(mktemp -d)
cat > "$D/main.gx" <<'GX'
let n = 0;
catch(e: Error<ErrChain<`E>>) n <- e ~ n + 1;
mod m;
(m::y, n)
GX
cat > "$D/m.gx" <<'GX'
let f = |x: i64| -> i64 select x { 1 => error(`E)?, x => x };
let y = f(2)
GX
printf 'val y: i64;\n' > "$D/m.gxi"
timeout -s KILL 30 "$G" --check "$D/main.gx"; echo "with m.gxi: exit $?"
rm "$D/m.gxi"
timeout -s KILL 30 "$G" --check "$D/main.gx"; echo "without m.gxi: exit $?"
rm -rf "$D"
