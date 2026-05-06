# 0.8.0

## Editor tooling

- Built-in LSP server — `graphix lsp` runs a Language Server Protocol
  implementation over stdio with diagnostics, completion, hover, go-to-definition,
  find-references, and document/workspace symbols
- Tree-sitter grammar — `ide/tree-sitter-graphix/` provides syntax highlighting,
  indents, and locals queries
- Editor configurations for VS Code, Zed, Helix, Neovim, Vim, and Emacs under
  `ide/editors/`
- New "Editor Setup" chapter in the book

## New widgets and packages

- `gui::data_table` — a high-performance virtual data table widget with
  resizable columns, sortable/filterable rows, calculated columns, sparkline
  cells, editable cells, netidx subscription columns, and per-row defaults
- `core::math` — full f64 math module: trig, hyperbolic, exp/log/pow, rounding,
  sign/abs, NaN/inf checks, degrees/radians, and constants (pi, e, tau, sqrt_2,
  ln_2, ln_10, infinity, nan)
- `core::opt` — option helpers: `is_some`, `is_none`, `contains`, `or_never`,
  `or_default`, `or`, `and`, `xor`, `ok_or`/`ok_or_else`, `or_else`,
  `zip`/`unzip`, `map`, `flat_map`, `filter`, `is_some_and`, `is_none_or`

## Standard library additions

- `core::queuefn(#trigger, #count, f)` — wrap a function so its invocations
  queue and release on an external trigger; optional writable `#count` ref reports
  queue depth
- `array::dedup` — return a copy with duplicates removed, preserving
  first-occurrence order (hash-based, no sort required)
- `map::change` — update the value at a key by applying a function, with a
  default for missing keys
- `map::get_or` — return the value at a key, or a fallback if missing
- `str::row_col` — split a path's last two components into `(row, col)` in a
  single builtin call

## Language and compiler

- **Function-type parameters now require names** — `fn(x: i64, y: i64) ->
  i64` instead of `fn(i64, i64) -> i64`. Names are documentation only (used for
  hover/completion popups); calls remain positional. The unnamed form is no longer
  accepted.
- The shell's `-: <type>` printout no longer dereferences type variables, so
  polymorphic types display with their original `'a`/`'b` names instead of `_<n>`
- Type inference picks the narrowest binding that unifies when merging sets
  containing unbound type variables
- Structured error positions — compile and parse failures attach `ErrorContext`
  with `Origin` and `SourcePosition` to the returned error, so IDE tooling can
  locate failures via `downcast_ref` instead of scraping message strings
- `core::filter` is now fire-and-forget: a new input arriving while the
  predicate is still working replaces the pending value rather than enqueuing it
  (use `core::queue` for strict pairing)

## Runtime

- `GXHandle::check` now returns a `CheckResult` carrying the post-compile env
  plus reference sites, module references, scope map, and (in LSP mode) sig→impl
  links and per-module env snapshots — buffers are pooled so per-keystroke
  recompiles are allocation-free in steady state
- `GXHandle::check` accepts optional per-call resolver overrides and an
  `initial_scope` for compiling package crate sources under their own module path
- `GXHandle::subscriber` exposes the runtime's netidx subscriber

## Bug fixes

- Fix function arg spec propagation
- Fix sorting stability
- Fix bug in `is_a`
- Fix type-directed printing of abstract types
- Fix many cases where ratatui panicked on invalid input
- Make chart types public

# 0.7.0

## Standard library reorganization

The separate `fs`, `net`, and `time` packages are now submodules of a
unified `sys` package with a shared `Stream` type for all I/O.

- `sys` — new umbrella package with `args()`, `join_path`, and submodules:
  - `sys::io` — unified stream abstraction (`Stream<'a>`) across files, TCP, TLS, and stdio
  - `sys::fs` — filesystem operations (was `fs`)
  - `sys::tcp` — TCP client and server sockets (new)
  - `sys::tls` — TLS streams over TCP connections (new)
  - `sys::net` — netidx publish/subscribe (was `net`)
  - `sys::time` — timers and current time (was `time`)
  - `sys::dirs` — platform-aware standard directory paths (new)
- `http` — HTTP client and server (new)
  - `http::rest` — JSON-aware REST helpers with bearer auth

## New standard library packages

- `json` — JSON serialization/deserialization with type-directed deserialization
- `toml` — TOML serialization/deserialization with type-directed deserialization
- `pack` — native binary serialization via netidx Pack format with type-directed deserialization
- `xls` — read xlsx, xls, ods, and xlsb spreadsheets (via calamine)
- `sqlite` — SQLite database access with type-directed query deserialization
- `db` — embedded key-value database (sled) with ACID transactions, typed trees, cursors, and reactive subscriptions
- `list` — immutable singly-linked lists with structural sharing
- `args` — command-line argument parsing with subcommands, options, and flags
- `hbs` — handlebars template rendering with partials and strict mode

## Language and compiler

- Type-directed deserialization — `json::read`, `toml::read`, `pack::read`, `sqlite::query`, and `str::parse` infer the target type from annotations
- Bitwise operations — `bit_and`, `bit_or`, `bit_xor`, `bit_not`, `shl`, `shr` for all integer types
- Binary encode/decode — `core::buffer` module for flexible binary serialization with endianness control and varint/zigzag encoding
- `stdin`, `stdout`, `stderr` — stdio streams via the unified IO framework
- Resolved types for built-ins — `BuiltIn::init` now receives the resolved `FnType`, enabling type-dependent behavior
- Remove `deftype!` macro — types are now defined directly in `.gxi` files
- `str::parse` returns `Result<'b, \`ParseError(string)>` instead of `Result<PrimNoErr, Any>`

## Bug fixes

- Fix type checker bug with HOF builtins not propagating concrete types through deferred checks
- Fix `Type::diff` producing incorrect results for certain type combinations
- Fix callsite bug with type resolution
- Fix second typecheck pass not running deferred checks
- Fix `contains` when testing identical sets
- Fix standalone builds
- Fix watch tests on macOS
- Fix json typecheck not rejecting missing concrete return types
- Fix uuid collision in node IDs

# 0.6.0

- Add checked arithmetic operators (`+?`, `-?`, `*?`, `/?`, `%?`) that return `Result` instead of logging errors on overflow/div-by-zero
- Unchecked operators (`+`, `-`, `*`, `/`, `%`) now log errors and return bottom on failure instead of propagating checked exceptions
- Add `array::init(n, f)` — create an array of n elements where element i is `f(i)`
- Fix `browser` widget type signature (no longer throws `ArithError`)
- Simplify `Error` type — `ArithError` is now `Error<\`ArithError(string)>` instead of `Error<ErrChain<\`ArithError(string)>>`
- Add mandelbrot example (GUI canvas)

# 0.5.1

- Fix graphix-package templates
- Fix gui tests

# 0.5.0

- Fix type checker bug with multiple parameterized type refs in a set
- Add universally quantified type variables to type aliases
- Add graphix-package-gui using iced
- Gate gui behind a feature so it can be disabled in tui only projects
- Changes to graphix-package to allow packages to run a closure on the main thread (Thanks Apple)

# 0.4.0

- Add packages
- Refactor the standard library as multiple packages

# 0.3.3

- Fix the operator precedence of ~

# 0.3.2

- Fix confusing printing of lambdas
- Fix suprious error message when printing lambdas
- Clean up abstract type registration

# 0.3.1

- Add abstract types to interfaces

# 0.3.0

- Implement interfaces (see the book for details)
- Upgrade to ratatui 0.30

# 0.2.2

- fix windows build

# 0.2.1

- support netidx local only resolver with zero configuration
- fix a bug that prevented tracking checked exceptions from call sites
- fix a bug that caused dbg to potentially use the wrong type when printing

# 0.2.0

- Add i8, u8, i16, and u16 to the language
- Initial file and filesystem api in the standard library `fs` module
- Refactor the graphix-shell interface a bit

# 0.1.13

- fix a bunch of type checker and runtime bugs found while writing docs

# 0.1.12

- delay call site function type resolution until after type checking for more
  accurate type inference

# 0.1.11

- add map built-in type, O(log(N)) lookup, insert, remove. Based on a memory
  pooled immutable-chunkmap

- introduce try catch. ? will now send errors to the nearest catch in dynamic
  scope.

- introduce or never operator $, which will return the non error value or never

- a lot of type checker and compiler bugs fixed
