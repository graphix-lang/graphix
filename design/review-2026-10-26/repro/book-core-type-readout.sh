#!/bin/bash
# Title: the book's function-type readouts (`fn<'a: unbound: Number + Singleton>(x: 'a: unbound ..)`,
#   `fn<'e: unbound: _>`) are not what the shell prints
# Command: bash design/review-2026-10-26/repro/book-core-type-readout.sh
# Expected per book/src/functions/polymorphism.md:
#   -: fn<'a: unbound: Number + Singleton>(x: 'a: unbound, y: 'a: unbound) -> 'a: unbound
#   -: fn<'e: unbound: _>(x: 'a: unbound, f: fn(x: 'a: unbound) -> 'b: unbound throws 'e: unbound) -> ...
#   and per reading_types.md, for |x, y| x + y:
#   -: fn<'_2067: unbound: Number + Singleton>(x: '_2067: unbound, ...) -> '_2067: unbound
# Observed:
#   -: fn<'a: Number + Singleton>(x: 'a: unbound within Number & Singleton, y: 'a: unbound within Number & Singleton) -> 'a: unbound within Number & Singleton
#   -: fn(x: 'a: unbound, f: fn(x: 'a: unbound) -> 'b: unbound throws 'e: unbound) -> 'b: unbound throws 'e: unbound
#   -: fn(x: '_7757: unbound within Number & Singleton, y: '_7757: unbound within Number & Singleton) -> '_7757: unbound within Number & Singleton
#   and `{"a" => 1}` is `-: Map<string, i64>`, not `Map<'_1893: string, '_1895: i64>`
G=${G:-/home/eric/tmp/review-2026-10-26/bin/graphix}
( for l in "let f = 'a: Number |x: 'a, y: 'a| -> 'a x + y" "f" \
    "let apply = |x: 'a, f: fn(x: 'a) -> 'b throws 'e| -> 'b throws 'e f(x)" "apply" \
    "let g = |x, y| x + y" "g" '{"a" => 1}'; do
    printf '%s\n' "$l"; sleep 0.7; done; sleep 1 ) \
  | timeout -s KILL 30 "$G" --no-cache 2>&1 | grep '^-:'
