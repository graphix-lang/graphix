# Place references: `&a[i]`, `&s.f`, `&t.0`, `&m{k}`

Status: built 2026-09-02
Pins: `stdlib/graphix-tests/src/lang/byref.rs` (`place_read_write`,
`place_move_siblings_bad`, `place_through_param`, `place_payload`,
`place_root_bottom_mirror`).

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

## `&T` and `&mut T`

A reference is read-only (`&e`, typed `&T`) or writable (`&mut e`, typed
`&mut T`); `Type::ByRef(Mutability, T)`, one runtime representation (a
cell id). `*r <- v` requires every reference `r` may hold to be `&mut`,
and `v` to fit each one's referent (`ConnectDeref::typecheck0_with`).

- `&T ⊇ &U` and `&T ⊇ &mut U` iff `T ⊇ U` (a read-only reference is
  covariant); `&mut T ⊇ &mut U` iff `T = U`; `&mut T ⊉ &U`.
- A union merges two `&` into one over the union of their referents;
  two `&mut` merge only when equal.
- `&mut x` and a `&mut` place over a binding are pinned to the binding's
  type. A `&mut` place rooted at `*r` requires `r: &mut`. `&mut e` over
  any other expression mints a fresh cell only the reference reaches:
  its type is a variable with the deferred lower bound `⊇ typeof(e)`,
  decided by the uses (`&mut null` into `&mut [i64, null]`).
- An optional writable argument is `[&mut T, null]` (queuefn's
  `#count`): `&mut [T, null]` would refuse `&mut x` for `x: T`.

### Why

Covariance with writes let `&a` (`a: i64`) be typed `&[i64, string]`
and `*r <- "s"` store a string in `a`; the JIT then panicked on the
slot type (c-node-mod-01, t-fntyp-01). Plain invariance refused the
optional props every widget takes (`&x` into `&[T, null]`). Most
references are only read, so the split keeps those covariant, makes
the writable ones exact, and shows the grant at the call site. Eric:
"a &T and &mut T is perfect, most of our ref usage is not for writing
anyway, and it gives an enhanced guarantee to the programmer."

## Equality and order

A reference's value is the cell its `&` minted, so two references to
one binding (`&x` written twice, or `&x` and a parameter holding
another `&x`) have different values. `==` and `!=` compare what they
name: `bind::ref_target` resolves a cell to its registered place (root
and path), else the binding at the end of its byref chain, else the
cell itself (`&mut` over a value is its own place). A comparison whose
operand type holds a reference (`Type::compares_refs`) rewrites each
reference in both values to `[root, step..]` (`ExecCtx::ref_targets`
over `Type::map_refs`, by the
type, through unions by runtime test) and compares the results; it is
decided at the node's first update and never fuses. A moving reference
compares by where it points now. Paths are compared as written, so
`&a[-1]` and `&a[1]` of a two-element array differ.

References have no order: cells are numbered in whatever order
compiling made them. The orderings, sorts, `min`/`max`, `dedup` and
map keys take the `Ordered` bound, which refuses a reference
(`design/tvar_constraints.md`). `uniq` compares as `==` does
(`ExecCtx::ref_targets`, which `==` uses too).

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
  insert is the immutable map's; `Index(0)` is also an error's or an
  abstract value's payload, which `&e.0` reaches where `e.0` types).
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
