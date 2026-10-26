#!/bin/sh
# Title: an implementation declaring 'a: Any is refused under an interface
#        that declares the plain variable ('a: Any is vacuous).
# Command: sh design/review-2026-10-26/repro/t-contains-sig-any-bound.sh
# Expected: exit 0 (the check passes; `fn<'a: Any>` and `fn(x: 'a)` are one type).
# Observed: "val id is declared fn(x: 'a) -> 'a but implemented
#   fn<'a: Any>(x: 'a: unbound within Any) -> 'a: unbound within Any: the
#   implementation requires 'a: Any, which the signature does not declare"
# Writing the gxi as `val id: fn<'a: Any>(x: 'a) -> 'a` passes.
G=${G:-/home/eric/tmp/review-2026-10-26/bin/graphix}
d=$(mktemp -d)
printf 'mod inner;\ninner::id(1)\n' > "$d/main.gx"
printf "val id: fn(x: 'a) -> 'a\n" > "$d/inner.gxi"
printf "let id = 'a: Any |x: 'a| -> 'a x\n" > "$d/inner.gx"
timeout -s KILL 30 "$G" --check "$d/main.gx"; echo "exit $?"
rm -r "$d"
