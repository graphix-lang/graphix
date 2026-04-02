# 0.6.0

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
