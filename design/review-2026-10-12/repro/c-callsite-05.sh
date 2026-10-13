#!/bin/sh
# c-callsite-05: a failed run-time bind unregisters its statement from a
# variable it still reads.
#
# The bind compiles the default `#e = n` (pending ref (n, stmt)), then
# fails at `#d` (c-callsite-02's hidden default). CallSite::bind's Err
# arm runs drop_deferred, which clears that pending ref while the default
# node is still held in self.args; CallSite::rebind's Err arm then
# discards it (clear_prepared_bind) and a second drop_deferred deletes
# it, so its Ref::delete reaches rt.unref_var and removes the statement's
# OWN registration of n (from `n + 1`). GRAPHIX_DBG_VARS=1 shows the
# final statement's single `REF_VAR n by stmt` followed by
# `UNREF_VAR n by stmt` at the failed bind.
#
# Command: sh c-callsite-05.sh path/to/graphix
# Expected: all three lines count up (17 18 19 ...).
# Observed: line 1 prints "17" and stops; line 2 (no failing call) and
#   line 3 (the same failing bind, but the default reads no variable,
#   `#e: i64 = 7`) keep counting.
G=${1:-graphix}
run() {
  (printf 'let n = 0\n'; sleep 0.5
   printf 'let c = sys::time::timer(duration:100.ms, true)\n'; sleep 0.5
   printf 'n <- c ~ n + 1\n'; sleep 0.5
   printf '%s\n' "$1"; sleep 0.5
   printf 'let s: {h: fn(x: f64) -> f64} = {h: f}\n'; sleep 0.5
   printf '%s\n' "$2"; sleep 2) | timeout -s KILL 8 "$G" --no-cache 2>&1 | tail -n +4 | tr '\n' ' '
  echo
}
run "let f = 'a: Number |#e: i64 = n, #d: 'a = 5, x: 'a| x + d" '{ let a = s.h(1.5); n + 1 }'
run "let f = 'a: Number |#e: i64 = n, #d: 'a = 5, x: 'a| x + d" '{ let a = 0; n + 1 }'
run "let f = 'a: Number |#e: i64 = 7, #d: 'a = 5, x: 'a| x + d" '{ let a = s.h(1.5); n + 1 }'
