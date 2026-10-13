# Book Examples

Every program here is included by a page of the book
(`{{#include ../../examples/<dir>/<file>.gx}}`), so a change to one is a
change to the documentation.

- `tui/`: the terminal UI chapters
- `gui/`: the GUI chapters (`icon.gx` is a module the gui examples share)
- `net/`: the `sys::args` page
- `collection/`: the Collection trait chapter

## The gate

<!-- CR claude for claude: [doc-drift/low] examples_compile builds every example, and
every bench program, the way `graphix --expand` does (check, elaboration and fusion,
under CFlag::ExpandSeq). It does not only check, as `graphix --check` would. That build
is what verifies the #[native] in mandelbrot.gx; a check alone skips #[native]. Say
"builds every example (check, elaboration, fusion) as `graphix --expand` does" here and
in CLAUDE.md's Working conventions bullet ("typechecks every one in the plain gate"),
then regenerate AGENTS.md. (examples-gui-06) -->
`graphix-shell/tests/examples_compile.rs` typechecks every example
against the full shell environment in the plain `cargo test`, as
`graphix --check` would, so an example must check, not only parse. The
UI examples are run by hand:

```bash
cargo run --bin graphix -- book/src/examples/tui/barchart_basic.gx
```

## Adding an example

1. Put the `.gx` file in the subdirectory of its chapter.
2. Include it in the chapter with `{{#include ../../examples/.../file.gx}}`.
3. Run it, and run `cargo test -p graphix-shell --test examples_compile`.
