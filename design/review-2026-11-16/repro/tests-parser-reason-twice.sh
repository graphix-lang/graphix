#!/bin/sh
# tests-parser-reason-twice: a parse refusal made by Input::refuse prints its
# reason twice (as the headline and again as a note at the same point); one
# made by Input::refuse_from leads with the generic "the parser could not
# continue past this point" and gives the reason only as a note. Only a type
# list's refusal is pinned to lead with its reason and say it once
# (a_refused_type_list_leads_with_its_reason); the other refusal tests check
# `msg.contains(why)`, which both shapes pass.
# Command: sh design/review-2026-11-16/repro/tests-parser-reason-twice.sh
# Expected: each program's reason once, right under "Parse error at ..".
# Observed: COUNT=2 for the Input::refuse sites (one-tuple, list-pattern
# suffix, flush in a seq, a duplicate fn-type argument, a one-expression
# block); a generic headline for the refuse_from sites (duplicate struct
# field, labeled argument after a positional one).
G=/home/eric/tmp/review-2026-11-16/bin/graphix
D=$(mktemp -d)
run() {
    printf '%s' "$1" > "$D/t.gx"
    out=$(timeout -s KILL 30 "$G" --check --no-cache "$D/t.gx" 2>&1)
    echo "== $1"
    echo "   COUNT=$(printf '%s' "$out" | grep -c -F "$2")  headline: $(printf '%s\n' "$out" | grep -A1 'Parse error at' | tail -1 | sed 's/^ *//')"
}
run 'let t = (1,); t' 'tuples must have at least 2 elements'
run 'let l = [<1, 2>]; select l { [<a.., b>] => b, _ => 0 }' 'list patterns have no suffix form'
run 'let r = seq t; flush(t) { x }; r' 'flush empties a queue'
run 'let f = |g: fn(#a: i64, #a: string) -> string| 1; f' 'arguments must have unique names'
run 'let s = { f(x) }; s' 'a block must contain at least 2 expressions'
run 'let s = {x: 1, x: 2}; s' 'struct fields must be unique'
run 'let g = |#a = 1, x| x; g(1, #a: 2)' 'labeled arguments come before positional ones'
run 'let x: {a: i64, a: string} = never(); 0' 'struct field names must be unique'
rm -rf "$D"
