#!/bin/sh
# t-parser-b-01: impl<..> Trait: the trait name's recorded position is taken
# before the whitespace after `>` (traitexp.rs impl_decl), so LSP
# references/definition for the impl site land on the wrong column (or line).
# Run: sh design/review-2026-10-12/repro/t-parser-b-01.sh
# Expected: every impl site reads 'Show'.
# Observed (run 1, t-parser-b-01.gx): line 4 reads '   S', line 5 reads ''
#   (empty, on the `impl<'a>` line instead of line 6).
# Observed (run 2, ordinary `impl<'a> Show for ..`): the site reads ' Sho';
#   definition with the cursor right after `Show` (4:13) is null, while the
#   param-less `impl Show` at the same relative cursor (5:9) answers.
D=$(mktemp -d); cp "$(dirname "$0")/t-parser-b-01.gx" "$D/main.gx"
timeout -s KILL 60 python3 -I "$(dirname "$0")/t-parser-b-lsp.py" "$D" main.gx \
  '[["textDocument/references",1,7]]'
cat > "$D/one.gx" <<'GX'
type Counter<'a> = Abstract<'a>;
trait Show {
    val show: fn(self) -> string
};
impl<'a> Show for Counter<'a> { let show = |c| "x" };
impl Show for Array<i64> { let show = |c| "y" };
Show::show(Counter(1))
GX
timeout -s KILL 60 python3 -I "$(dirname "$0")/t-parser-b-lsp.py" "$D" one.gx \
  '[["textDocument/references",1,7],["textDocument/definition",4,13],["textDocument/definition",5,9]]'
