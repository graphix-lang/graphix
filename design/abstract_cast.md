# Abstract types across data: the `Cast` trait

Status: proposed 2026-10-18; direction agreed with Eric (both
directions, one trait), details awaiting his ruling.

## The problem

`T(v)` constructs a nominal abstract only where its definition is
visible, so the defining module decides what a `T` is. Data skips that
rule in both directions:

- **In.** `cast<T>(data)` builds a `T` anywhere, and so does every
  type-directed read into a type that holds one (`json::read`, `pack`'s
  reads, `str::parse`, `sys::net::subscribe`). Since e0616b758 the
  payload must fit the representation, but none of the module's own
  checks run (a sorted array, a validated handle, a range).
- **Out.** `json::write`, `toml`, `pack`'s writes and publishing
  serialize the representation, which the module may mean to keep
  private and to change.

A Rust-backed abstract's payload is Rust's: it never reads from data,
and its writer is Rust's.

## The rule

Core gains:

```graphix,ignore
trait Cast {
    val to_value: fn(self) -> Any;
    val from_value: fn(v: Any) -> Result<self, `InvalidCast(string)>
}
```

- **`from_value` is dispatched on the target type.** `cast<T>(v)` and
  every type-directed read call it when the target type holds `T`.
  The cast operator is its only dispatcher, as `==` is `Eq`'s.
- **`to_value` is dispatched on the value.** A writer walks a runtime
  value and, at a Graphix abstract, calls `to_value` by the value's tag
  (as printing calls `Display`), then writes what it returns.
- **No impl refuses both ways.** The check refuses a cast or a read
  whose target holds a Graphix abstract with no impl, as it refuses a
  target holding a reference (`Type::holds_ref`). The writer builtins
  take a bound that refuses the same of their argument. Where only an
  instance knows the type, the cast yields its `InvalidCast` and the
  writer its error.
- **A held value is not cast.** A value that is already a `T` (its tag,
  `is_a`) is held, and `from_value` is not called. Narrowing
  (`cast<T>(x)` where `x: [T, i64]`) needs no impl.
- **A bodiless `impl Cast for T`** (in the defining module, where the
  representation is visible) is the representation, unchecked both
  ways. This is today's behaviour, now chosen by the module.
- **Rust-backed abstracts take no impl.** A `Cast` impl for a
  Rust-backed abstract is refused, as the core traits are
  (`traits::check_target`).
- **A cast from an abstract needs no rule.** An abstract value is
  opaque at run time, so `cast<i64>` of one is already an `InvalidCast`.

A typical impl, in the defining module:

```graphix,ignore
impl Cast for Sorted {
    let to_value = |s| s.0;
    let from_value = |v| {
        let a = cast<Array<i64>>(v)?;
        select array::is_sorted(a) {
            true => Sorted(a),
            false => error(`InvalidCast("not sorted"))
        }
    }
}
```

`from_value(to_value(x)) == x` is the impl's duty, as a consistent
order is `Ord`'s.

## Mechanism

- Both methods ride the core traits' loan
  (`coretraits::with_value_hooks`, `design/traits.md` §6). The registry
  is keyed `(method, AbstractId)`, with a per-key site pool so that a
  re-entrant call (an impl that casts to its own type, or writes a
  value holding one) gets a fresh site. The methods are implicitly
  `#[sync]`.
- The cast walk (`typ/cast.rs`, with no `ExecCtx`) calls `from_value`
  through the loan at `Type::Abstract`. The sites that arm the loan:
  - the `TypeCast` node,
  - the kernel's cast call site,
  - the type-directed builtins,
  - the writer builtins.
- With no loan (another thread, or a context with no impl), the call
  refuses. That is the conservative answer, and the check has already
  refused every static case.
- A bottoming method reads as `InvalidCast`, or as the writer's error.
- A `to_value` whose result has the same abstract at its head is
  refused, never written again: `|s| s` would loop forever.
- The payload cast of e0616b758 goes away. A bodiless impl, or
  `cast<Rep>(v)` inside a written one, does what it did. Inside the
  module the representation is visible, so `holds_ref` refuses a
  representation holding a reference statically.

## Not affected

Session images encode through Rust `Pack`. They restore state exactly
and never run user code, so they are not affected. Only the `pack`
package's reads and writes go through the trait.

## Pins to change

- `pack_abstract_read_checks_the_payload` and
  `pack_abstract_read_refuses_a_reference` become a refusal with no
  impl, a bodiless impl's unchecked read, and a written impl's check.
- New pins:
  - a json round trip through a written impl;
  - a write refused with no impl;
  - the `to_value` self-loop refusal;
  - narrowing a held value with no impl.
