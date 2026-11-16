#!/bin/sh
# Title: `with(e: T)` has only a refusal pin, and that pin is refused for two reasons.
# Command: sh design/review-2026-11-16/repro/tests-seq-typed-with-unpinned.sh
# seq_try.rs `refusals` pins `with(e: Error<`Wrong>)`. `Error<`Oops>` (the right tag without the
# ErrChain the binder carries) is refused too, so the pin holds whether or not the binder is
# typed `Error<ErrChain<..>>`; and nothing pins that a covering annotation checks and runs.
# Expected/observed: Error<`Wrong> refused; Error<`Oops> refused; Error<ErrChain<`Oops>> checks;
# Error<ErrChain<[`Oops, `Other]>> checks; the covering form runs and prints 7.
G=${G:-/home/eric/tmp/review-2026-11-16/bin/graphix}
D=$(mktemp -d)
for T in 'Error<`Wrong>' 'Error<`Oops>' 'Error<ErrChain<`Oops>>' 'Error<ErrChain<[`Oops, `Other]>>'; do
  printf '{ let bad = |v| -> [i64, Error<`Oops>] error(`Oops); seq { try { bad(1)? } with(e: %s) { 0 } } }\n' "$T" > $D/c.gx
  if timeout -s KILL 30 $G --no-cache --check $D/c.gx > $D/out 2>&1; then echo "$T: checks"; else echo "$T: refused: $(tail -1 $D/out | sed 's/.*: type mismatch/type mismatch/')"; fi
done
cat > $D/r.gx <<'EOF'
let bad = |v| -> [i64, Error<`Oops>] error(`Oops);
let r = seq { try { bad(1)? } with(e: Error<ErrChain<`Oops>>) { select (e.0).error { `Oops => 7 } } };
sys::exit(sys::time::after_idle(duration:100.ms, 0));
r
EOF
echo "run: $(timeout -s KILL 30 $G --no-cache $D/r.gx 2>&1 | tail -1)"
rm -rf $D
