#!/bin/sh
# Title: sibling_first_written_by_a_later_input_fires_alone cannot see `b` fire
# The REPL test writes `a` while the arm holding `count(b)` sleeps, wakes the
# arm and asserts count(b) == 1. Writing `b` itself while the arm sleeps
# leaves count(b) at 1 too, so the assertion holds whether or not `b` fires
# with `a`; only a write while the arm is awake is counted.
# Command: sh tests-modules-sibling-asleep-vacuous.sh  (G= the debug graphix)
# Observed:
#   test shape (a written asleep)        -> (5, 1)
#   b also written asleep                -> (5, 1)   <- b fired, not counted
#   b written while the arm is awake     -> (5, 2)
G=${G:-/home/eric/tmp/review-2026-11-16/bin/graphix}
run() {
  (printf 'let (a, b) = (1, 2); let gate = true; let r = select gate { true => (a, count(b)), false => (-1, -1) }\n'
   sleep 1
   for l in "$@"; do printf '%s\n' "$l"; sleep 1; done
   printf 'r\n'; sleep 2) | timeout -s KILL 40 "$G" --no-cache 2>&1 | tail -1
}
echo "test shape:";       run 'gate <- false' 'a <- 5' 'gate <- true'
echo "b written asleep:"; run 'gate <- false' 'a <- 5' 'b <- 9' 'gate <- true'
echo "b written awake:";  run 'a <- 5' 'b <- 9'
