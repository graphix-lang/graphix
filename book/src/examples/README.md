# Book Examples

Every program here is included by a page of the book
(`{{#include ../../examples/<dir>/<file>.gx}}`), so a change to one is a
change to the documentation.

- `tui/`: the terminal UI chapters
- `gui/`: the GUI chapters (`icon.gx` is a module the gui examples share)
- `net/`: the `sys::args` page
- `collection/`: the Collection trait chapter

## The gate

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
