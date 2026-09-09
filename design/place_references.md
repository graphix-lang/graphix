# Place references: `&a[i]`, `&s.f`, `&t.0`, `&m{k}`

Status: built 2026-09-02
Pins: `stdlib/graphix-tests/src/lang/byref.rs` (`place_read_write`,
`place_move_siblings_bad`, `place_through_param`).

## The rule

A reference whose expression is an accessor chain — array index, tuple
index, struct field, map key, nested — over a variable (or a
dereference) is a **place**: the root binding plus a path. It types as
a reference to the ELEMENT (`&a[i]` is `&T` for `a: Array<T>`: the
access's own `[T, Error<ArrayIndexError>]` minus the error, the way `$`
types). `*r` reads the root through the path. `*r <- v` PATCHES the
root: the value is rebuilt along the path and delivered to the root. A
dynamic key (`&vals[focus]`) makes a moving reference: it points at
whatever the key names when it fires, reads and writes there, and
re-fires its readers when the key moves.

Failures are runtime facts, as for indexing: a read of a place that
does not exist bottoms (warned); a write into one is dropped and logged
(`error!`), the root untouched. Arrays are immutable values, so a write
is a copy along the path — O(n) for an array, fine for a form, not for
a hot loop.

### Why

`&x` names a binding's value channel: it mints a cell, the byref chain
maps the cell to `x`, `*r` reads and `*r <- v` writes `x`. `&e` for any
other expression made a DERIVED channel — readable, but a write into it
went nowhere. So a reference into a value was unwritable, and no widget
API written over `&State` could reach a state held in a collection: the
admin TUI's form over nine editors could not use
`line_edit::handle(&st, e)` and grew a pure `step` and an
array-rebuilding twin, an API shaped by the hole. Eric: "not having
this changed the way you wrote an API in tui; that qualifies as a now
change."

## Two writes to one root in one cycle

Each write is queued as a **patch** — path and value — and resolved
against the root's value AS IT STANDS WHEN THE PATCH IS DELIVERED, in
the runtime's delivery loop (`push_var_event!`). The same-variable-
same-cycle rule already defers the second delivery to the next cycle;
resolving late means it lands on the first patch's result, never on the
stale whole both writers read. `Rt::patch_var` beside `Rt::set_var`;
`VarUpdate::{Set, Patch}` in the queue.

## Mechanics

- `node::place`: `Step::{Index, Field, Key}`, `Path`, `read_path`,
  `write_path` (a struct is its sorted `[name, value]` pairs; a map
  insert is the immutable map's).
- `ByRef` (`node/bind.rs`, `Place::of`): detects the chain at compile,
  compiles the root and the dynamic keys beside the whole access — the
  cell still mirrors the element, so embedders keep reading it —
  registers the cell's place with the runtime (`Rt::set_ref_path`,
  re-registered when a key moves) and re-fires when it moves. Plain
  references are unchanged: same cell, same chain, `Value::U64(cell)`
  on the wire.
- `Deref`: a cell with a registered place (`Rt::ref_path`) reads the
  root through the path, with interest on the root.
- `ConnectDeref`: a cell with a place patches the root.
- References still de-fuse.

Not built: a place rooted at an expression that is neither a variable
nor a dereference (`&f(x)[0]` stays a derived channel); slices as
places; a patch that grows an array.
