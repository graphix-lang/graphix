#!/bin/bash
# ide-tooling: the regex highlighters' keyword lists miss words the parser reserves
# (graphix-types/src/expr/parser/mod.rs KEYWORDS) or the grammar uses as keywords.
#
# command: bash design/review-2026-10-19/repro/ide-tooling-regex-keyword-drift.sh
# expected: each highlighter names every word below (as book/theme/graphix-highlight.js does)
# observed (HEAD 9445a1d21):
#   never/pub/trait/impl/mut/val/for/flush: missing from all three
#   try/abort: missing from vim and vscode (emacs has them)
#   vim and vscode also list `where` (not a Graphix word) as a keyword.
REPO="$(cd "$(dirname "$0")/../../.." && pwd)"
E="$REPO/ide/editors"
for w in mod let select type fn cast never if use rec catch try pub trait impl seq seqq \
         until mut val with as throws for abort flush any; do
    v=$(grep -c "^syntax keyword graphixKeyword.*\b$w\b" "$E/vim/syntax/graphix.vim")
    t=$(grep -cE "\\\\\\\\b\(.*\b$w\b.*\)\\\\\\\\b" "$E/vscode/syntaxes/graphix.tmLanguage.json")
    e=$(sed -n '/(let ((keywords/,/))/p' "$E/emacs/graphix-mode.el" | grep -c "\"$w\"")
    [ "$v$t$e" = 111 ] || [ "$v" -ge 1 -a "$t" -ge 1 -a "$e" -ge 1 ] || echo "$w: vim=$v vscode=$t emacs-fallback=$e"
done
