# Abstract types across data: the `Cast` trait

Status: built 2026-10-18 (Eric's rulings of that day).

## The rule

A Graphix abstract type (`type T = Abstract<rep>`) crosses data only
through its implementation of the core trait

```graphix,ignore
trait Cast {
    val to_value: fn(self) -> Any;
    val from_value: fn(v: Any) -> Result<self, `InvalidCast(string)>
};
```

- **In.** A cast to a type holding `T` (`cast<T>(v)`) and every
  type-directed read (`json`, `toml`, `pack`, `sqlite`, `str::parse`,
  `sys::net::subscribe`/`call`, `db` trees) make a `T` with
  `from_value`. A value that is already a `T` at the same params is
  held.
- **Out.** Every writer (`json`, `toml`, `hbs`, `pack`,
  `sys::net::write`/`publish`/rpc replies and arguments, `db` keys and
  values) writes what `to_value` returns, through
  `abstract_value::to_data`.
- **Only the defining module implements it.** The impl must sit where
  the representation is visible (`Env::abstract_rep`). A method the
  impl leaves out is the representation itself, so `impl Cast for T {}`
  means "the representation, unchecked both ways". Because only that
  module can write the impl, choosing transparency is the module's
  call. An impl that leaves a method to a representation holding a
  function or a reference is refused, since no data carries either.
- **No impl, no crossing.** The check refuses all of these:
  - a cast whose target holds an abstract type with no impl
    (`TypeCast::typecheck0`, `Type::holds_uncastable`);
  - a read whose target does (`Concrete` now includes the rule);
  - a write whose value does (the `Data` bound below).
  A Rust-backed abstract takes no impl (`traits::check_target`, as
  for every core trait), so it never crosses data.
- **A program never calls the methods.** Naming `Cast::to_value` or
  `Cast::from_value` is refused (`Bind` ref compile). `from_value` has
  no `self` argument: a method may take `self` only in its result, and
  then no call can dispatch it.

`from_value(to_value(x)) == x` is the impl's duty, as a consistent
order is `Ord`'s. A `from_value` reports its refusal through its
`` `InvalidCast(why) ``, which the cast's error repeats.

## The `Data` bound

`'a: Data` (`Type::Data`, a bound like `Concrete`): whatever binds the
cell holds no reference, no function and no abstract type without a
`Cast` impl (`Type::data_holds`). An open cell holds, and the binding
hands the conjunct to its open cells (`require_data`). Unlike
`Concrete`, a cell that settles open is fine, so `json::write([])`
checks. The writers declare it on their value parameter. A writer whose
parameter was `Any` (`pack::write_bytes`, `sys::net::write`/`publish`)
now takes a bounded variable. A value typed `Any` is still judged when
it is written: `to_data` refuses an abstract value with no impl,
Rust-backed ones included.

`contains.rs::uncastable_refusal` names the type instead of printing a
generic mismatch: "Box has no Cast implementation, so no read, cast or
writer reaches it", and "fn(..) is not data".

## Mechanism

- **The hooks.** The impl is reached through the core traits' value
  seam (`coretraits::with_hooks`, design/traits.md §6), keyed by method:
  `ValueHookDispatch` gained `to_value` (dispatched on the value's tag)
  and `from_value` (dispatched on the target `Type::Abstract`). Each
  answers `None` (no impl), `CastHook::Representation` (the method is
  left out) or `CastHook::Ran(v)`. The methods are implicitly
  `#[sync]`.
- **Reads.**
  - The cast walk's `Type::Abstract` arm (`typ/cast.rs`) holds a value
    of the same id and params. Otherwise it asks `from_value`; for the
    representation it casts the data against `rep` at the params and
    wraps the result.
  - The loans are armed by the cast node, `TypedRead::map_value`, the
    `sys::net` reads, typed fast functions and kernels.
- **Writes.**
  - `to_data` rewrites every abstract value in a value into its data,
    allocating only where something changed. It refuses past 1024
    nested abstract values, so a `to_value` that writes its own type
    is refused rather than followed forever.
  - json and toml walk the result along the static type. At an
    abstract position written as its representation
    (`Env::written_as_representation`), they use the representation's
    type, so a struct stays an object. A written `to_value` returns
    `Any`, so its result is written by its shape.
- **Async builtins.**
  - `EvalCachedAsync::DATA_ARGS` lists the arguments that leave as data;
    they are converted before `prepare_args`.
  - `DATA_RESULT` casts the answer to the call's resolved success type,
    where it holds an abstract type (`data_target`, `read_back`). A
    refusal answers with the builtin's own declared error.
  - db's `blocking_op` builtins, the cursor reads and the subscription
    events use these. db keys of an abstract type are stored as their
    data, so a tree orders them by that data, not by the type's `Ord`.
- **Decoding.** An abstract value decodes only under an image session
  (`GxAbstract::decode`). Writers never emit one, so bytes holding one
  are forged or stale. Session images still carry abstract values
  exactly, through Rust `Pack`.
- **Declared impls.** A declared impl (`impl Cast for T;` in a `.gxi`)
  is narrowed by the module check to the methods the implementation
  writes, so the hook sees the representation for the rest.

## Pins

- `lang::cast_trait::*`:
  - `from_value` and the representation, json and pack/toml round
    trips;
  - every refusal: cast, read, write, function, direct call,
    non-abstract, reference representation, sibling module;
  - the self-nesting write, and an impl declared in an interface.
- `lib_tests::db::db_abstract_round_trip_through_cast`.
- `lib_tests::net::net_abstract_through_cast`.
- `lib_tests::pack::pack_abstract_read_checks_the_payload` and
  `pack_abstract_too_deep_is_refused_by_the_writer`.
- `abstract_value::test::abstract_value_decodes_only_under_an_image`.
