#!/bin/sh
# Title: the remedy module_in_a_function_raising_to_it_refused names is unpinned
# The refusal says "catch it inside the module"; the module caught inside
# (a `catch` above `let n = g(1)?`) builds and runs, which no test holds.
# Command: sh tests-modules-catch-inside-the-module.sh  (G= the debug graphix)
# Observed: refused ("... catch it inside the module") without the catch; 2 with it.
G=${G:-/home/eric/tmp/review-2026-11-16/bin/graphix}
D=$(mktemp -d)
printf '%s\n' 'let mk = |t: i64| -> i64 { mod a; a::f(t) };' \
  'sys::exit(sys::time::after_idle(duration:100.ms, 0));' 'mk(1)' > "$D/main.gx"
echo 'val f: fn(x: i64) -> i64' > "$D/a.gxi"
printf '%s\n' 'let g = |x: i64| -> [i64, Error<`E>] select x { 0 => error(`E), n => n };' \
  'let n = g(1)?;' 'let f = |x: i64| -> i64 x + n' > "$D/a.gx"
echo "uncaught:"; timeout -s KILL 30 "$G" --no-cache "$D/main.gx" 2>&1 | tail -1
printf '%s\n' 'let g = |x: i64| -> [i64, Error<`E>] select x { 0 => error(`E), n => n };' \
  'catch(e) println("[e]");' 'let n = g(1)?;' 'let f = |x: i64| -> i64 x + n' > "$D/a.gx"
echo "caught inside:"; timeout -s KILL 30 "$G" --no-cache "$D/main.gx" 2>&1 | tail -1
rm -rf "$D"
