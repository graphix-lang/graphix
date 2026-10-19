#!/bin/bash
# Title: `regress` cannot tell which refusal a `// expect: reject` pin gets;
#        2 pins pass on a PARSE error and 14 more on a value-position rule
#        unrelated to the bug they document.
# Command: bash design/review-2026-10-19/repro/fuzz-lib-b-reject-pin-reasons.sh
# Expected: each reject pin is refused for the reason its header documents
#           (e.g. refparam-occurs: "cannot infer a finite type here";
#           annotation-names: an undefined type name; rec-rtype-writeback:
#           not rejected at all, its header says both engines now run it).
# Observed (HEAD 9445a1d21): the innermost cause per pin; among them
#   nested-bind-stmt-dead-elim-aug2026/05_outer_literal_bind.gx   Parse error: Unexpected `,` (a bare `never`)
#   rec-rtype-writeback-aug2026/00_fn_value_reads_as_i64.gx       Parse error: Unexpected `/` (trailing `// minimized:` comment)
#   refparam-occurs-aug2026/00_flatmap_typedef_wedge.gx           a type definition is not an expression
#   annotation-names-sep2026/00_undefined_name_in_a_callback_let.gx  a let binding is not an expression
#   ... 11 x "a let binding is not an expression", 3 x "a type definition is not an expression"
# The outcome manifest records only `reject`, and run_regression trusts a
# recorded reject, so every one of these passes.
FUZZ=${FUZZ:-/home/eric/tmp/review-2026-10-19/bin/graphix-fuzz}
REPO=${REPO:-/home/eric/proj/graphix}
export XDG_CACHE_HOME=${XDG_CACHE_HOME:-$(mktemp -d)}
awk -F'\t' '$1=="reject"{print $2}' "$REPO/graphix-fuzz/outcome.manifest" | while read -r p; do
    o=$(timeout -s KILL 60 "$FUZZ" run "$REPO/graphix-fuzz/findings/$p" 2>&1 | grep -m1 '^Interp/InLanguage:')
    case "$o" in
        *"Parse error"*) why="PARSE: $(echo "$o" | sed 's/ | /\n/g' | grep -m1 'Unexpected')" ;;
        *) why=$(echo "$o" | awk -F' \\| ' '{print $NF}' | cut -c1-150) ;;
    esac
    printf '%s\t%s\n' "$p" "$why"
done | grep -E 'PARSE|not an expression'
