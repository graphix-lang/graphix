#!/bin/sh
# An input's end binds a `Concrete` cell to bottom where a call's terminal
# settle refuses it: a later REPL input calls the function and gets a value
# at type bottom (typ::settle::TVar::settle_built).
#
# command:
#   sh design/review-2026-11-16/repro/t-settle-input-end-settles-concrete-to-bottom.sh
#
# expected: the call refused as it is when the same statements are one script
#   (`let fs = [|| str::parse("42"), || str::parse("43")]; fs[0]$()` in a
#   file: "the type 'b must be fully known here, a type-directed operation
#   reads it: annotate it"), or the first input refused, naming `fs`.
#
# observed (tail of the output):
#   -: Array<fn() -> ['b: _, Error<`ParseError(string)>]>
#   [Abstract(|| str::parse("42")), Abstract(|| str::parse("43"))]
#   -: Error<`ParseError(string)>
#   42
#   -: `ParseError(string)
#   null
#   -: string
#   select fs[0]$() { error as x => "an error" }: no arm matches i64:42: a coverage hole in the check
G=${GRAPHIX:-/home/eric/tmp/review-2026-11-16/bin/graphix}
printf '%s\n' \
  'let fs = [|| str::parse("42"), || str::parse("43")]' \
  'fs' \
  'let e: Error<`ParseError(string)> = fs[0]$()' \
  'e' \
  'e.0' \
  'select fs[0]$() { error as x => "an error" }' \
  | timeout -s KILL 30 "$G" --no-cache 2>&1
echo "--- the same statements as one script:"
d=$(mktemp -d)
printf '%s\n' 'let fs = [|| str::parse("42"), || str::parse("43")]; fs[0]$()' > "$d/one.gx"
timeout -s KILL 30 "$G" --check --no-cache "$d/one.gx" 2>&1 | tail -n 2
rm -rf "$d"
