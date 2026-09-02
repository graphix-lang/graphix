# Place references: `&a[i]`, `&s.f`, `&t.0`, `&m{k}`

Status: **built 2026-09-02** (Eric's call, same day: "not having this
changed the way you wrote an API in tui; that qualifies as a now
change").

## The hole

A reference names a binding's value channel: `&x` mints a cell, the
byref chain maps the cell to `x`, `*r` reads `x` through it and
`*r <- v` writes `x`. `&e` for any other expression made a *derived*
channel — readable, but a write into it went nowhere. So a reference
into a value (`&vals[i]`) was readable and unwritable, and no widget
API written over `&State` could reach a state held in a collection.
The admin TUI's form over nine editors could not use
`line_edit::handle(&st, e)` and grew a pure `step` and an
array-rebuilding twin — an API shaped by the hole.

## The rule

A reference whose expression is an accessor chain — array index, tuple
index, struct field, map key, nested — over a variable (or a
dereference) is a **place**: the root binding plus a path. It types as
a reference to the element (`&a[i]` is `&T` for `a: Array<T>`, the
access's own `[T, Error<ArrayIndexError>]` minus the error, the way
`$` types). A read applies the path to the root's value. A write
rebuilds the root's value along the path and delivers it to the root.
A dynamic key (`&vals[focus]`) makes a moving reference: it points at
whatever the key names when it fires, reads and writes there, and
re-fires its readers when the key moves.

Failures are runtime facts, as for indexing: a read of a place that
does not exist bottoms (warned); a write into one is dropped and
logged (`error!`), the root untouched. Arrays are immutable values, so
a write is a copy along the path — O(n) for an array, fine for a form,
not for a hot loop.

## Two writes to one root in one cycle

Each write is queued as a **patch** — the path and the value — and
resolved against the root's value *as it stands when the patch is
delivered*, in the runtime's delivery loop (`push_var_event!`). The
same-variable-same-cycle rule already defers the second delivery to
the next cycle; resolving late means it lands on the first patch's
result, never on the stale whole both writers read. `Rt::patch_var`
beside `Rt::set_var`; `VarUpdate::{Set, Patch}` in the queue.

## Mechanics

- `node::place`: `Step::{Index, Field, Key}`, `Path`, `read_path`,
  `write_path` (a struct is its sorted `[name, value]` pairs; a map
  insert is the immutable map's).
- `ByRef`: detects the chain at compile (`Place::of`), compiles the
  root and the dynamic keys beside the whole access (the mirror the
  cell still carries, so embedders keep reading it), registers the
  cell's place with the runtime (`Rt::set_ref_path`, re-registered when
  a key moves) and re-fires when it moves. Plain references are
  unchanged: same cell, same chain, `Value::U64(cell)` on the wire.
- `Deref`: a cell with a registered place reads the root through the
  path, interest on the root.
- `ConnectDeref`: a cell with a place patches the root.
- The JIT: references de-fuse as before.

Not built: a place rooted at an expression that is neither a variable
nor a dereference (`&f(x)[0]` stays a derived channel, as before);
slices as places; a patch that grows an array. Pins: `lang/byref.rs`
`place_*`.
