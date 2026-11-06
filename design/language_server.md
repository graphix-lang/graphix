# The language server

Status: current principle.
Pins: `graphix-shell/tests/lsp/`.

Roots, lazy checks, the last successful check, queries by target.

**Language server.** A ROOT is a project's root file (a `.gx` no other
file's `mod` reaches; `graphix-lsp/src/workspace.rs` scans) or an open
file outside every project. A root at `<crate>/src/graphix/mod.gx` of a
`graphix-package-<x>` crate is a package root, checked as the body of
`mod <x>` over the copy registered at startup: `Env::
unbind_scope_subtree` must drop everything a package registers, so a new
global registry is cleared there (the block-type registry, shared by every
copy of the env, is not: each load names its blocks apart). `--check` and the server share one
path (`GXRt::check`), which runs the check alone (`CFlag::CheckOnly`:
no elaboration, no fusion, so `#[native]`, `#[parallel]` and the def
assertions are verified by a build; `--expand` builds) and checks a script's file as one block, as it
runs, with its names at the root (`compile_script`); a root loads
through `RootFile::load`, open
buffers first, paired with its `.gxi`: a script's interface is spliced
in and its `val`s and impls checked over the script's names, by the
run as by the check (`check_script_sig`, right after the check); and under buffer overrides a path
is never canonicalized (the editor's names rule). Checks are lazy and
coalesced: a change marks its roots dirty and `ServerState::flush`
checks them when the client has nothing queued; a request will not wait
for that quiet moment and first checks the roots of the document it
names (`ServerState::flush_document`, one check at most), and one that
names no document checks nothing new; nothing is checked at startup.
The last SUCCESSFUL check of a root (`Checked`: the env and the
`Ide` sinks, `ide.rs`) answers every query, so a buffer that does not
compile still answers, stale; bind ids mean something only within one
`Checked`. A query resolves the cursor to a target (bind id, canonical
module, canonical type) from the sinks and never by looking a word up:
what the check did not record has no answer. Positions come from the
AST: a declared name is an `expr::Name` (identifier + `WrittenAt`), a
`use` item carries the position of each path segment (`WrittenPath`),
every parsed `Expr` has an `end`, and a bind, typedef, trait or `mod`
site is recorded AT THE NAME. All three decide nothing and pack to
nothing, so a name or an end out of a packed AST or an image is
`NOWHERE` (`Name::pos_or`). A parser site that builds an `Expr` gives
it its end (`Expr::ending`; `expr()`, the postfix loop and
`shunting_yard`'s reduce cover what passes through them); pin: `graphix-compiler/tests/
expr_spans.rs` (every node's `[pos, end)` parses back to the node, over
the examples and the stdlib). A package under development is not in the binary that checks it, so
under `lsp_mode` a package root's name is a `package::` root for the
check and an unknown builtin is a WARNING at its `'name`
(`node/lambda.rs::UnknownBuiltIn` stands in: typed by the declared
signature like any builtin, never produces); without `lsp_mode` it
stays an error. Warnings go through `Env::warn`: to `Ide.warnings`
under a check with a sink, else to stderr as before. A check ends by
warning on each import it added that no lookup went through
(`Env::unused_imports_since`; `Env::lookup_at` marks them). The server
publishes the warnings of a root's last SUCCESSFUL check and, when the
current one failed, the error beside them. An error's position is its `ErrorSite`, the innermost wrap: contexts are
attached with `.at(&spec)` (`expr::At`), `wrap!` or `bailat!`, never
`ErrorContext(..)` by hand. Pins: `graphix-shell/tests/lsp/` (the real
server over `Connection::memory()`; positions are `"let y = |x + 1"`
markers).
