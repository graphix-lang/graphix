#!/bin/sh
# sum/product/divide return `Number` ([Int, Real]), which arithmetic and a typed
# let refuse, so the result is usable only through a cast; mixed numeric
# arguments are accepted and silently pick a type at run time.
# Command: sh gx-stdlib-sum-returns-number.sh   (GRAPHIX = the graphix binary)
# Observed:
#   sum(1, 2) + 1                -> refused: cannot compute [Int, Real] + i64
#   let x: i64 = sum(1, 2); x    -> refused at sum(1, 2)
#   sum(1, 2.0)                  -> 3.0 (accepted)
#   a non-variadic stand-in for 'a: Number + Singleton |x: ['a, Array<['a, Array<'a>]>]| -> 'a
#   checks s(1) + 1, s([1, 2]) + 1, s([[1.0], 2.0]) + 1.0 and refuses s(1, 2.0)
GRAPHIX=${GRAPHIX:-/home/eric/tmp/review-2026-11-16/bin/graphix}
F=$(mktemp --suffix .gx)
for p in 'sum(1, 2) + 1' 'let x: i64 = sum(1, 2); x'; do
  printf '%s\n' "$p" > "$F"; echo "== $p"; timeout -s KILL 30 "$GRAPHIX" --check "$F" 2>&1 | tail -1
done
printf 'sys::exit(sys::time::after_idle(duration:100.ms, 0));\nsum(1, 2.0)\n' > "$F"
echo "== sum(1, 2.0)"; timeout -s KILL 30 "$GRAPHIX" --no-cache "$F"
printf "let s = 'a: Number + Singleton |x: ['a, Array<['a, Array<'a>]>], y: ['a, Array<['a, Array<'a>]>]| -> 'a never(); (s(1, [2]) + 1, s([[1.0], 2.0], 3.0) + 1.0)\n" > "$F"
echo "== typed stand-in"; timeout -s KILL 30 "$GRAPHIX" --check "$F" && echo ok
printf "let s = 'a: Number + Singleton |x: ['a, Array<['a, Array<'a>]>], y: ['a, Array<['a, Array<'a>]>]| -> 'a never(); s(1, 2.0)\n" > "$F"
echo "== typed stand-in, mixed"; timeout -s KILL 30 "$GRAPHIX" --check "$F" 2>&1 | tail -1
rm -f "$F"
