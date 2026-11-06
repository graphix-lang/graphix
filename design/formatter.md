# The formatter and the printer

Status: current principle.
Pins: `graphix-types/src/expr/parser/test.rs`, `expr/format.rs`'s tests.

`graphix fmt`: parse, print, reparse. Canonical printing, what rides beside the AST, comments and layout.

**Formatter** (`graphix fmt`, `expr/format.rs`): parse → the one
printer (`PrettyDisplay`, `expr/print.rs`) → reparse; a result whose
syntax, comments, attributes or string delimiters differ from the input
is refused, never written. The AST stays canonical (struct fields, union
members and a use statement's names are held sorted); what the author
chose rides beside it as metadata that decides nothing: `WrittenAt` (a
position that is always equal, hashes to nothing, packs to nothing) on
struct pattern binds, struct type fields, variant types, every declared
name (`Name`) and every expression's end, `Expr::pos`
for struct literal fields, `Expr::str_form` for a string's delimiters,
`Comments` and `Eol` (`Aside`: always equal) for the `//` lines above,
after and below an interface item or a trait method. Lambda formals carry
`ItemComments`, with the variadic formal and empty-list comments on
`LambdaExpr`. Use-tree comments carry their prefix depth (`UseComment`)
beside the sorted imports; a single path's comments follow its imported
name and a group's follow its prefix. Ending comments merged at one
prefix become leading lines (`design/module_system.md`). A type
definition's comments ride on it, keyed by where each field or member
was written (`TypeNotes`, which packs to nothing, printed through
`typ::Noted`), and the guard reads them in canonical order. Every
comment list above and `Decorations` (not an `Eol`, which is one line)
holds `Comment`s: a line, and whether a blank line stands between it and
the line before (`Comment::spaced`), so a run of blank lines inside a
block is one and one before a block's first line or after its last is
not the block's.
**Printing is canonical unless `PrintFlag::AsWritten` is set, and only
the formatter sets it**: printed types and expressions reach
program-visible values (a cast error, a null error), so comments print
only under it; `WrittenAt` and
`str_form` are not part of a session image (`Expr::pos` and comments
are, but not a typedef's field and member comments, `TypeNotes`), and a type is shared by content, so whose written order it carries
is incidental. A tree holding a comment,
an attribute or a doc has no single-line form. What the formatter
does normalize: `i64`/`f64` literals print bare and an `f32` in its
shortest form; a datetime prints in UTC and a duration in its largest
whole unit; a value literal's string (`error:".."`) prints as netidx
prints it, a control character with no name as `\u{hex}` and any other
character as itself, where a graphix string keeps them raw; a run of adjacent
undecorated `use` statements merges into one per root and visibility, a
sorted tree with every shared prefix written once, where no item reads or
binds what another binds or reads (a glob merges with nothing); a blank line stands
around every file-level item that spans lines; primitives in a union
print first, in canonical order, and the other members a position does
not record (all but variants and named types) next, in canonical order; an item's comments print above its
attributes, and a comment under an arm's `=>` or a field's `name:`
moves above the arm or the field. Layout: a value follows its head
(`let x =`, `<-`, `name:`, `=>`) on the head's line when it fits or
opens with a bracket whose first line fits, else it moves under the
head; a lambda head that does not fit lists one argument to a line; a
lone argument that opens with a bracket hugs the brackets around it, in
expressions, patterns and types alike (`f({`, `` `Tag({ ``, `` `A(`B([ ``,
`Array<{`); two arguments list. After a printer change
run the corpus harness over every `.gx`/`.gxi` here and in `../netidx`:
`cargo run -p graphix-compiler --example gxfmt -- <files>` (many files:
round trip + idempotence; one file: prints it; `GXFMT_UNCHECKED=1`
skips the reparse). The print round-trip proptests are randomized: a
printer bug can pass several runs. Width and indent are
`format::FormatConfig` (defaults 90 and 4), discovered by
`FormatConfig::discover`: the nearest `graphixfmt.json` at or above the
source file, else `dirs::config_dir()/graphix/graphixfmt.json`, else the
defaults; a malformed file is an error; `--width`/`--indent` override.
A new layout setting is a field there, never a constant in the printer
(`PrettyBuf::nested` is the one indent step). The LSP serves
`textDocument/formatting` from the same `format_source`
(`graphix-lsp/src/formatting.rs`): one whole-document edit,
no edit for a document that does not parse, an error response only for
`format::Refused` (the formatter declined its own output) or a malformed
`graphixfmt.json`.

- **Comments** are legal above an expression, a select arm, an impl or
  trait method, a struct-literal field, a lambda formal, a use-tree item
  or an interface item, at the end
  of a list item's line after its separator (a statement, an arm, a
  field, an element, an argument: `Decorations::eol`; an interface item
  or a trait method: `Eol`; a formal: `ItemComments`; an import:
  `UseComment`) and below a list's last item or a file's
  (`Decorations::below`, on a trailing `;`'s empty statement when there
  is one; `Sig::below`, `TraitExpr::below`, `LambdaExpr::below`, and an impl with no
  method's `ImplExpr::below`); none inside a string's
  splice or an attribute's arguments, which print on one line, and no
  `///` doc or attribute in a splice either, anywhere in what it holds
  (judged over the formatter guard's own list, `expr/ornament.rs`); `///`
  docs only in an interface. A type definition's struct fields and union
  members take all three, through the fields and members of their types
  (not a type argument, a payload or a function type); a union member
  takes one only as a variant or a named type, which print as written,
  and the lines below the last go under the last of those; anywhere
  else in a type a comment is refused (`expr/parser/typexp.rs`). A blank
  line between two lines of one comment block stays, one for a run of
  them. Parse errors report the furthest point
  reached with the source line and a caret, what was expected there,
  and a refusal its reason.
