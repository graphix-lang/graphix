#!/bin/sh
# t-print: the formatter refuses seq triggers whose leading postfix source needs parens.
# command: sh t-print-seq-trigger-parens.sh
# expected: each program formatted (all pass `graphix --check`)
# observed: "formatter bug: the formatted text says something else" for each:
#   was flush(1) / now (flush(1)); was 1 / now { a: 1 }; was {..}.a + 1 / now ({..}.a + 1)
# trigger_needs_parens wraps the whole trigger, adding an ExplicitParens the parser
# dropped (parens followed by a postfix are not kept).
G=${G:-/home/eric/tmp/review-2026-10-19/bin/graphix}
d=$(mktemp -d)
printf 'let flush = |x| x;\nlet r = seqq (flush)(1) { 1; 2 };\nr\n' > $d/a.gx
printf 'let r = seq ({a: 1}).a { 1; 2 };\nr\n' > $d/b.gx
printf 'let s = {a: 1};\nlet r = seq ({s with a: 2}).a + 1 { 1; 2 };\nr\n' > $d/c.gx
for f in a b c; do $G --check $d/$f.gx && echo "$f: check ok"; $G fmt --stdout $d/$f.gx; done
rm -rf $d
