#!/bin/bash
# Title: the REPL's "-: type" line prints inferred variables by name, not what they are bound to
# Command: bash design/review-2026-10-19/repro/book-core-repl-type-display.sh
# Expected (the book's transcripts: getting_started.md, fundamental_types.md "Map Literals",
#   reading_types.md "Auto Parameters", polymorphism.md, recursion.md):
#   -: Array<i64>   /  -: Map<string, i64>  /  -: Array<'a: i64>  /  fn<'a: unbound: Number + Singleton>..
# Observed: -: Array<'_7721>  /  -: Map<'_7721, '_7722>  /  -: Array<'a>  /  -: Error<'a>
#   graphix-shell/src/lib.rs prints the type under PrintFlag::ReplacePrims alone; the
#   DerefTVars flag that shows a variable's binding was dropped from that call.
G=/home/eric/tmp/review-2026-10-19/bin/graphix
for input in '[1, 2]' '{"a" => 1}' 'error(`Foo)' \
  "let rec add_pairs = 'a: Number |a: Array<'a>| -> Array<'a> select a { [e0, e1, tl..] => array::push_front(add_pairs(tl), e0 + e1), a => a }
add_pairs([1, 2, 3, 4, 5])"; do
  printf '%s\n' "$input" | timeout -s KILL 4 $G --no-cache 2>&1 | grep -- '^-:'
done
