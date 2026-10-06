# Book Examples

This directory contains executable code examples from the Graphix book.

## Structure

<!-- CR claude for claude: [doc-drift] This README is stale. It lists only tui/ (gui/,
net/ and collection/ also exist) and says examples may reference undefined names and
need only stay syntactically valid, but graphix-shell/tests/examples_compile.rs
typechecks every example in the plain cargo test gate; CLAUDE.md:839-842 (and so
AGENTS.md) states the same wrong rule. Nine examples are included by no book page, so
nothing reads or runs them: gui/data_table_dummy.gx, gui/data_table_scrolling.gx,
gui/mandelbrot.gx, net/args.gx, net/sum.gx, tui/browser_commands.gx,
tui/color_palette.gx, tui/layout_nested_focus.gx and tui/text_styled.gx. They have gone
stale: color_palette.gx never shows colour 255 (array::group passes the length after the
push, so `n == 255` emits indices 0..254), mandelbrot.gx's header says to write iterate
as a Rust builtin although iterate now fuses to a native loop, and net/sum.gx needs a
/local/bench publisher it never mentions. Include each one in its chapter or delete it,
and state the real gate here and in CLAUDE.md. probe:
design/review-2026-10-05/repro/examples.r2-11.gx (examples.r2-11) -->
- `tui/` - Terminal UI widget examples referenced in the book's TUI chapter

## Purpose

These examples serve two purposes:

1. **Documentation**: They are included in the mdbook via `{{#include ...}}` directives
2. **Verification**: They can be manually tested to ensure documentation stays accurate

## Testing Examples

Since these are visual TUI examples, they need to be tested manually:

```bash
# From the repository root
cargo run --bin graphix -- book/src/examples/tui/barchart_basic.gx
```

Some examples are code snippets that reference undefined variables (like `content`).
These are meant to illustrate specific concepts within a larger context and may not
run standalone. When updating the compiler, review these examples to ensure they
remain syntactically valid.

## Adding New Examples

When adding examples to the book:

1. Create the `.gx` file in the appropriate subdirectory under `book/src/examples/`
2. Reference it in the markdown using `{{#include ../../examples/.../filename.gx}}`
3. If the example should be runnable, test it manually with the graphix shell
4. If it's a code snippet, ensure it's syntactically valid for the current compiler
