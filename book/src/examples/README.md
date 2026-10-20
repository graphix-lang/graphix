# Book Examples

Every program here is included by a page of the book
(`{{#include ../../examples/<dir>/<file>.gx}}`), so a change to one is a
change to the documentation.

- `tui/`: the terminal UI chapters
- `gui/`: the GUI chapters (`icon.gx` is a module the gui examples share)
- `net/`: the `sys::args` page
- `collection/`: the Collection trait chapter
- `stdlib/`: the standard library pages

## The gate

`graphix-shell/tests/examples_compile.rs` builds every example (check,
elaboration and fusion) against the full shell environment in the plain
`cargo test`, as `graphix --expand` does, so an example must build, not
only parse: a `#[native]` in one is verified there. The same test
parses every ```` ```graphix ```` fence written inline in a page, and each
`〉` input of a REPL transcript; tag a schematic fragment
```` ```graphix,ignore ```` and a program meant to be refused
```` ```graphix,refused ````, which the check must refuse. The tui
package's `examples_test` renders every TUI example and drives some with
keys; any UI example also runs by hand:

```bash
cargo run --bin graphix -- book/src/examples/tui/barchart_basic.gx
```

## Adding an example

1. Put the `.gx` file in the subdirectory of its chapter.
2. Include it in the chapter with `{{#include ../../examples/.../file.gx}}`.
3. Run it, and run `cargo test -p graphix-shell --test examples_compile`.
