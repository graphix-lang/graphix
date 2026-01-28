# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository. You should keep this file up to date! Whenever you have a significant conversation with the user about the project you should summarize it in this file as part of completing the assigned task.

## What is Graphix?

Graphix is a dataflow programming language particularly well suited for building UIs and network programming with netidx. Programs are compiled to directed graphs where operations are nodes and edges represent data flow paths. The language is reactive at the language level - when dependent values change, the entire graph updates automatically.

Key language features: lexically scoped, expression-oriented, strongly statically typed with type inference, structural type discipline, parametric polymorphism, algebraic data types, pattern matching, first-class functions and closures.

## Project Structure

This is a Rust workspace with 4 main crates:

- **graphix-compiler**: The compiler that parses and compiles Graphix expressions into node graphs. Entry point is `compile()` in `lib.rs` which calls `compiler::compile()` then typechecks the resulting node.
- **graphix-rt**: A general-purpose runtime that executes the compiled node graphs. The runtime runs in a background task and is interacted with via `GXHandle`. Supports custom extensions via the `GXExt` trait.
- **graphix-stdlib**: Standard library implementation (both Rust and Graphix code). Graphix source files are in `src/*.gx`, Rust implementations are in corresponding `.rs` files.
- **graphix-shell**: REPL and CLI tool. The binary is named `graphix`. Includes TUI widget implementations in `src/tui/`.

Additional directories:
- **book/**: mdbook documentation source
- **docs/**: Compiled HTML documentation

The compiler depends on netidx (a networked publish-subscribe system) which is expected to be at `../netidx/` (sibling directory).

## Building and Testing

Build the workspace:
```bash
cargo build                          # Debug build
cargo build --release                # Release build (optimized, LTO enabled)
```

Build specific crate:
```bash
cargo build -p graphix-shell         # Build shell
cargo build -p graphix-compiler      # Build compiler
```

Run tests:
```bash
cargo test                           # Run all tests in workspace
cargo test -p graphix-compiler       # Test specific crate
cargo test pattern                   # Run tests matching name
```

Run the Graphix shell:
```bash
cargo run --bin graphix                    # Start REPL
cargo run --bin graphix file.gx         # Execute file
cargo run --bin graphix --check file.gx # check that a file compiles and type checks
cargo run --bin graphix --help          # See all options
```

Build documentation:
from the graphix/book directory
```bash
mdbook build -d ../docs              # Build language docs to docs/
mdbook serve ../docs                    # Serve docs locally
```

## Architecture

### Compilation Pipeline

1. **Parsing** (`graphix-compiler/src/expr/parser/`): Text → `Expr` AST with position info
2. **Compilation** (`graphix-compiler/src/node/compiler.rs`): `Expr` → `Node<R, E>` graph
3. **Type Checking**: Each node implements `typecheck()` to verify type correctness

Key types:
- `Expr`: Immutable AST representation with `ExprKind` variants
- `Node<R, E>`: `Box<dyn Update<R, E>>` - compiled graph node
- `ExecCtx<R, E>`: Execution context holding builtins, environment, runtime
- `Scope`: Lexical and dynamic module path information

### Node Graph Execution

Nodes implement either:
- `Update` trait: Regular graph nodes (most built-in nodes)
- `Apply` trait: Function applications (called by `CallSite` nodes)

The `Update` trait requires:
- `update()`: Process events and return output value
- `delete()`: Clean up node and children
- `typecheck()`: Verify types
- `refs()`: Populate referenced bind IDs
- `sleep()`: Put node to sleep (for unselected branches)

### Runtime System

The runtime (`graphix-rt`) implements the `Rt` trait which handles:
- Netidx subscriptions and publications
- Variable references and updates
- RPC calls
- Timer events

Event processing is batch-based: the runtime collects all simultaneous events into an `Event` struct and delivers them to the graph in one cycle. Multiple updates to the same variable in one cycle must be queued for the next cycle.

### Type System

Located in `graphix-compiler/src/typ/`:
- `Type`: Structural types including primitives, tuples, structs, variants, functions, refs
- `TVar`: Type variables for inference (bound via `TVal`)
- `FnType`: Function signature (args, return type, throws, constraints)

Types are structural - compatibility is based on structure, not names. Type inference uses constraint solving with type variables.

### Built-in Functions

Built-ins implement the `BuiltIn<R, E>` trait:
- `NAME`: Function name constant
- `TYP`: Lazy-initialized function type
- `init()`: Returns initialization function

Register built-ins with `ExecCtx::register_builtin::<T>()`.

Stdlib built-ins are in `graphix-stdlib/src/*.rs` with corresponding Graphix implementations in `*.gx` files.

## Code Review Process

When doing code review, follow the CR/XCR comment system defined in AGENTS.md:

1. Add comments as: `// CR <your-name> for <addressee>: comment text` to the relevant file near the relevant code
2. When issues are addressed, the comment becomes: `// XCR ...`
3. Review XCRs - delete if resolved, convert back to CR with explanation if not

This project maintains very high code quality standards - no shortcuts, careful consideration of all implications.

## Common Patterns

### Working with Types

Use `format_with_flags()` to control type variable formatting:
```rust
format_with_flags(PrintFlag::DerefTVars, || {
    // Type printing code here
})
```

### Error Handling

Use the `wrap!` macro to add expression context to errors:
```rust
wrap!(node, some_result())
```

For creating error values:
```rust
err!(tag, "error message")           // Static message
errf!(tag, "format {}", args)        // Formatted message
```

### Node Implementation

When implementing nodes:
1. Store spec (`Arc<Expr>`) for error reporting
2. Implement all trait methods (update, delete, typecheck, refs, sleep)
3. Use `Refs` to track bound and referenced BindIds
4. Call `ctx.set_var()` when setting variables (handles caching)

## Testing

The purpose of writing tests is not for them to pass, it's to find
bugs in the main code. Never work around a problem with a test that
you think should work. Even if it isn't related to the purpose of the
test you are writing, every failure is an opportunity to learn about a
bug and fix it. If you find such an "off topic" bug, discuss it with
me before trying to fix it yourself.

The parser includes it's own dedicated tests:
- `graphix-compiler/src/expr/test.rs`: The round trip test of the
  parser pretty printer with random expressions generated by
  proptest. Whenever we change the syntax we must update this test and
  it must run successfully (preferably overnight)
- `graphix-compiler/src/expr/parser/test.rs`: A selection of specific
  tests for the parser. Mostly obsoleted by the expr test.

The stdlib includes test infrastructure for the compiler:
- `graphix-stdlib/src/test/langtest.rs`: Language feature tests
- `graphix-stdlib/src/test/libtest.rs`: Library function tests

## Book Examples

The book's code examples are stored as separate `.gx` files in `book/src/examples/` and included into the documentation via mdbook's `{{#include ...}}` syntax. This serves two purposes:

1. Keeps documentation and code in sync
2. Allows manual testing of examples when the compiler changes

TUI examples in `book/src/examples/tui/` are visual and must be tested manually:
```bash
cargo run --bin graphix -- book/src/examples/tui/barchart_basic.gx
```

Some examples are code snippets that reference undefined variables and are meant to illustrate concepts within a larger context. These should remain syntactically valid but may not run standalone. When updating the compiler, review these examples to ensure they still compile.

## Development Notes

- The compiler is optimized for dev builds (opt-level="s", lto="thin")
  to reduce compile times. If you need to debug something you can turn
  this optimization off, however the parser will often overflow the
  stack without at least some optimization.
- Release builds use full optimization (opt-level=3, codegen-units=1, lto=true)
- Rust edition 2024 is used throughout
- The project uses `triomphe::Arc` instead of `std::sync::Arc` for better performance
- Pooling is used extensively (`poolshark`, `immutable-chunkmap`) to reduce allocations

## Recent Changes

### Filesystem Module Refactoring (Nov 2025)

The `graphix-stdlib/src/fs/` module was refactored to split functionality into separate files:
- `mod.rs`: Core tempdir and join_path functionality
- `file.rs`: Read/write operations (read_all, read_all_bin, write_all, write_all_bin)
- `watch.rs`: Filesystem watch functionality

**fs::tempdir enhancements:**
- Added `#in` parameter to specify parent directory for temp directory creation
- Added `#name` parameter with two variants:
  - `Prefix(string)`: Add prefix to temp directory name
  - `Suffix(string)`: Add suffix to temp directory name
- All combinations of `#in` and `#name` are supported
- The temp directory is kept alive via `Arc<Mutex<Option<TempDir>>>` and automatically cleaned up when replaced or dropped

**Documentation improvements:**
- Updated `fs.gx` to clarify that read/write operations are queued and executed in order
- Previous documentation suggested results arrived in order but execution could be concurrent
- New documentation accurately reflects that operations complete sequentially in queue order

**Test coverage:**
- Created comprehensive test suite in `graphix-stdlib/src/test/lib/fs/tempdir.rs` covering:
  - Basic tempdir creation
  - Parent directory specification
  - Prefix and suffix naming
  - Error handling for invalid paths
  - Integration with read/write operations
- All fs tests passing (40 tests total as of Nov 2025)
