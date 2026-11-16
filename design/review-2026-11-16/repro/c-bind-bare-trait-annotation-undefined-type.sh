#!/bin/sh
# Title: `let x: T = ..` above `trait T` is refused as "undefined type T",
# not by the refusal written for a trait named above its declaration
#
# Command: sh design/review-2026-11-16/repro/c-bind-bare-trait-annotation-undefined-type.sh
#
# design/traits.md: a `let`'s annotation is met once every name its block
# declares is registered, and a name that means a trait is refused, "trait Sh
# used as a type above its declaration".
#
# Expected: 1 and 2 are refused by that message; 3 checks (a `type` declared
# below is in force above).
# Observed: 2 is; 1 is refused "undefined type T at line: 1, column: 8",
# though the same line declares T. 4 and 5 add an unrelated refusal as the
# second statement: 4 still reports "undefined type T" and 5 reports "nosuch
# not defined", so the bare annotation is refused by the let's compile,
# before the block's names are registered and before the deferred check
# (defer_written_names) that carries the message runs.
BIN=${BIN:-/home/eric/tmp/review-2026-11-16/bin/graphix}
D=$(mktemp -d)
n=0
while IFS= read -r line; do
  n=$((n+1))
  printf '%s\n' "$line" > "$D/p.gx"
  if out=$(timeout -s KILL 30 "$BIN" --check "$D/p.gx" 2>&1); then
    echo "$n ok: $line"
  else
    echo "$n refused: $line"
    echo "$out" | tail -1
  fi
done <<'GX'
let x: T = 1; trait T { val m: fn(self) -> i64 }; 0
let x: (T, i64) = (1, 2); trait T { val m: fn(self) -> i64 }; 0
let x: T = 1; type T = i64; x
let x: T = 1; let y = nosuch(1); trait T { val m: fn(self) -> i64 }; 0
let x: (T, i64) = (1, 2); let y = nosuch(1); trait T { val m: fn(self) -> i64 }; 0
GX
rm -rf "$D"
