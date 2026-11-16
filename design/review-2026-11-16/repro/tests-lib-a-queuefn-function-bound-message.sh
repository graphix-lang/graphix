#!/bin/sh
# queuefn's Function bound refuses a defaulted label or a variadic with the bare
# mismatch text, which names no reason and no fix, and which a non-function gets too.
#
# run: sh tests-lib-a-queuefn-function-bound-message.sh
# expected: the refusal of a defaulted-label / variadic function says why (a defaulted
#           label or a variadic argument) and what to write (wrap it in a lambda that
#           calls it), as Ordered/Discernible refusals do (Type::not_discernible)
# observed: "type mismatch 'a: unbound within Function does not contain fn(?#scale: i64, x: i64) -> i64"
#           and for `3`: "type mismatch 'a: unbound within Function does not contain i64" --
#           both match core.rs's pin refused("within Function does not contain")
G=${G:-/home/eric/tmp/review-2026-11-16/bin/graphix}
d=$(mktemp -d)
for p in 'queuefn(#trigger: never(), |#scale: i64 = 10, x: i64| -> i64 x * scale)(5)' \
         'queuefn(#trigger: never(), max)(1, 5)' \
         'queuefn(#trigger: never(), 3)'; do
  echo "let result = $p;" > "$d/q.gx"
  echo "== $p"
  timeout -s KILL 30 "$G" --check "$d/q.gx" 2>&1 | tail -1
done
rm -rf "$d"
