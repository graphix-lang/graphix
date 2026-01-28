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
