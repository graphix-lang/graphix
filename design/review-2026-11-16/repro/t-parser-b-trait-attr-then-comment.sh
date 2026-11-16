#!/bin/sh
# Title: a `//` line between a trait method's `#[..]` attribute and its `val`
#   is a parse error ("Unexpected `/` / Expected val"), where the same line
#   between an expression's attribute and the expression parses and fmt moves
#   it above the attribute. traitexp::trait_method reads the comments, then
#   the attributes, with no comment after them; leading_decorations
#   interleaves the two.
# Command: sh design/review-2026-11-16/repro/t-parser-b-trait-attr-then-comment.sh
# Expected: both format, the comment printed above the attribute.
# Observed: the trait fails to parse; the expression formats.
G=${G:-/home/eric/tmp/review-2026-11-16/bin/graphix}
D=$(mktemp -d)
printf 'trait Show {\n    #[pure]\n    // above show\n    val show: fn(self) -> string\n};\n1\n' > $D/trait.gx
printf 'let f = |x| x;\n#[serial]\n// above\nf(1)\n' > $D/expr.gx
for f in trait expr; do
    echo "== $f"
    timeout -s KILL 30 $G fmt --stdout $D/$f.gx 2>&1
    echo "exit $?"
done
rm -rf $D
