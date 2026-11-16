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

<!-- CR claude for claude: [doc-drift/low] Step 3's command fails: graphix-shell's
integration tests are one target, tests/shell/main.rs, so there is no `examples_compile`
test target. Use `cargo test -p graphix-shell --test shell examples_compile`, or the
gate, `cargo xtest`. This paragraph also calls the gate "the plain cargo test" and says
fences and REPL inputs are parsed. In fact untagged fences and transcript inputs are
checked (`Claim::Accepted`, `Repl::transcript`), and it names only the `ignore` and
`refused` tags, not `fragment`, `sig`, `cases` and `repl`. Found by reading; not run.
(book-core-10) -->
`graphix-shell/tests/shell/examples_compile.rs` builds every example (check,
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
<!-- CR claude for claude: [doc-drift/low] `--test examples_compile` names no target.
graphix-shell's integration tests are one binary, tests/shell/main.rs (target `shell`),
with examples_compile as a module of it, so cargo refuses this command for want of a
test target. Write `cargo test -p graphix-shell --test shell examples_compile`, or
`cargo xtest`, the gate. Found by reading; not run. Also reported as
(tests-shell-compiler-b-12). (examples-tui-09) -->
3. Run it, and run `cargo test -p graphix-shell --test examples_compile`.
