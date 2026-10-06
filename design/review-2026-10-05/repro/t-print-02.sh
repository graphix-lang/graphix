#!/usr/bin/env bash
# t-print-02: the seq-head printer (print.rs trigger_needs_parens) adds
# parens the head parser does not need and keeps, so `graphix fmt` (and
# LSP formatting, the same format_source) refuses valid seqs.
#
# Two causes. (a) A `let` trigger's value is read by arith(false) after
# `let p =`, where the head's `{`/clause check no longer applies, but
# the printer still parenthesizes a brace-led value (block, struct,
# functional update, map) or a `flush(..)` call. (b) reads_bare has no
# arm for the prefix operators `*` `!` `-` `&`, which arith(false) reads
# bare (parser/test.rs a_prefix_trigger_leaves_the_body_alone).
# The parens reparse as ExplicitParens, a different AST.
#
# command: GRAPHIX=/path/to/graphix bash design/review-2026-10-05/repro/t-print-02.sh
#
# expected: every program passes --check and formats (fmt exit 0).
# observed (HEAD c722befe, debug build): every --check exits 0, every
# fmt exits 1 with "formatter bug: the formatted text says something
# else", e.g. for line 1 "was ...: r / now ...: *r" (it printed
# `seq (*r) { 1 }`), for the struct "was ...: { a: go, b: 2 } / now
# ...: ({ a: go, b: 2 })". The same seqs written with the parens
# (`seq (*r) { 1 }`) format unchanged, exit 0.
set -u
GRAPHIX=${GRAPHIX:-graphix}
dir=$(mktemp -d)
trap 'rm -rf "$dir"' EXIT
while IFS= read -r src; do
    printf '%s\n' "$src" > "$dir/p.gx"
    "$GRAPHIX" --no-cache --check "$dir/p.gx" > /dev/null 2>&1
    check=$?
    "$GRAPHIX" fmt < "$dir/p.gx" > "$dir/out" 2>&1
    fmt=$?
    printf 'check=%s fmt=%s  %s\n' "$check" "$fmt" "$src"
    [ "$fmt" = 0 ] || sed 's/^/    /' "$dir/out"
done <<'EOF'
let r = &1; seq *r { 1 }
let x = true; seq !x { 1 }
let x = 1; seq -x { 1 }
let x = 1; seq &x { 1 }
let r = &1; let go = 1; seq go ~ *r { 1 }
let r = &1; seq let v = *r { v + 1 }
let go = 1; seq let c = { a: go, b: 2 } { c.a }
let go = 1; seq let c = { go; go + 1 } { c }
let s = { a: 1, b: 2 }; seq let c = { s with a: 3 } { c.a }
let go = "k"; seq let c = { go => 1 } { c }
let flush = |x| x; let go = 1; seqq let v = flush(go) { v }
EOF
