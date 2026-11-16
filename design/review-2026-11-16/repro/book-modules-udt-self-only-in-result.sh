#!/bin/bash
# Title: book/src/udt/traits.md says a trait method naming `self` only in its result is
# accepted (declaration and impl) and only its CALL is refused ("called without its self
# argument"); the check refuses the trait's declaration itself.
# Command: bash book-modules-udt-self-only-in-result.sh
# Expected per the book: the first program (no call) checks; the second is refused
#   "called without its self argument".
# Observed: both refused at the declaration: "trait method mk takes no `self` argument, so no
#   call can dispatch it: only a core trait's method, which the runtime calls, may return
#   `self` alone" (graphix-compiler/src/node/traits.rs, method_decl).
G=/home/eric/tmp/review-2026-11-16/bin/graphix
T=$(mktemp -d /home/eric/tmp/review-2026-11-16/work/book-modules-udt/selfonly.XXXX)
printf 'trait T { val mk: fn(x: i64) -> self }; impl T for i64 { let mk = |x| x }; 1\n' > $T/a.gx
printf 'trait T { val mk: fn(x: i64) -> self }; impl T for i64 { let mk = |x| x }; T::mk(1)\n' > $T/b.gx
for f in a b; do timeout -s KILL 20 $G --no-cache --check $T/$f.gx 2>&1 | tail -1; done
