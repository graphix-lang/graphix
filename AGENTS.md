# Repository Guidelines

## Project Structure & Module Organization
- `graphix-compiler/`: compiler (parser, type checker, graph generation).
- `graphix-rt/`: runtime for executing dataflow graphs.
- `graphix-stdlib/`: standard library (Rust + Graphix sources).
- `graphix-shell/`: REPL, CLI, and TUI widget library.
- `book/`: mdbook source for the language documentation.
- `docs/`: generated HTML documentation (build output).
- `book/src/examples/`: runnable `.gx` examples used in the book.

## Build, Test, and Development Commands
- `cargo build --release`: build the full Rust workspace.
- `cargo test`: run all Rust tests across crates.
- `cargo run --bin graphix -- path/to/file.gx`: run a Graphix program from the workspace without installing.
- `cd book && mdbook build -d ../docs`: rebuild the HTML documentation into `docs/`.

## Coding Style & Naming Conventions
- Rust code is formatted with `rustfmt` (`rustfmt.toml` in repo). Run `cargo fmt` before submitting.
- Use Rust conventions: `snake_case` for modules/functions, `CamelCase` for types/traits, `SCREAMING_SNAKE_CASE` for constants.
- Graphix source files use the `.gx` extension; keep examples small and focused for documentation.

## Testing Guidelines
- Use Rust’s built-in test framework (`#[test]`) inside crate modules or `tests/` for integration tests.
- Prefer adding tests alongside the crate you change (compiler/runtime/stdlib/shell).
- No explicit coverage target is documented; add regression tests for bug fixes.

## Commit & Pull Request Guidelines
- Recent commit messages are short, lowercase, and imperative (e.g., `fix many parser problems`).
- PRs should include a concise summary, testing notes (commands run), and links to related issues.
- If you update docs or examples, rebuild the book and include that in the PR notes.

## Documentation & Generated Assets
- Treat `docs/` as build output; edit sources in `book/` and regenerate with `mdbook`.
- When updating the language or standard library, check for related book updates.
