# Compiler review

Review of `84cda11d`. The source contains 27 `CR codex for eric` comments,
identified below. This change records findings; it does not implement fixes.

The review covered parsing and AST traversal, module loading, sequence lowering,
types and traits, node evaluation, fusion/JIT boundaries, images, environment
lifecycle, allocation patterns, and the formatter harness. Source inspection was
supplemented by the compiler suite and focused reproductions. “Inspection” below
means a finding was established from the implementation, without a dedicated
runtime test; memory-safety probes were not executed.

## Findings

| CR | Location | Finding | Evidence |
|---|---|---|---|
| 01 | [fusion/emit_helpers.rs:193](src/fusion/emit_helpers.rs#L193) | Public safe helpers dereference or free arbitrary pointer bits. | Inspection: `graphix_valarray_len(1)`, `free_slot_chain`, and block-tree helpers. |
| 02 | [image/mod.rs:541](src/image/mod.rs#L541) | Forgetting or dropping borrowed TLS session guards out of order can leave dangling pointers. | Inspection; also affects the public shared-map session guards. |
| 03 | [image/mod.rs:1455](src/image/mod.rs#L1455) | Expression deduplication dereferences unpinned expressions from earlier Pack calls. | Inspection: measure a clone, drop it, then measure another expression with the same id. |
| 04 | [shared_map.rs:171](src/shared_map.rs#L171) | Nested codecs create overlapping mutable references to the encode table and its containing encoder. | Inspection of map-of-map callbacks and `image::encoding`. |
| 05 | [typ/cast.rs:91](src/typ/cast.rs#L91) | Conversion through a recursive union can overflow the stack and abort the process. | Reproduced with both engine configurations. |
| 06 | [node/bind.rs:1284](src/node/bind.rs#L1284) | Dereferencing a bottom reference keeps reading its previous target. | Reproduced with both engine configurations. |
| 07 | [node/bind.rs:1317](src/node/bind.rs#L1317) | Changing a reference to a standing target loses the address expression's fire. | Reproduced with both engine configurations. |
| 08 | [node/select.rs:123](src/node/select.rs#L123) | Unconsulted guards after arm 63 affect selection bottomness and firing. | Reproduced at the 63/64 boundary with both engine configurations. |
| 09 | [node/callsite.rs:1566](src/node/callsite.rs#L1566) | A bottom dynamic callee still invokes its previously bound function. | Reproduced with both engine configurations. |
| 10 | [node/coretraits.rs:199](src/node/coretraits.rs#L199) | Runtime core-trait dispatch erases abstract type arguments, bypassing generic implementations. | Reproduced with both engine configurations. |
| 11 | [node/place.rs:89](src/node/place.rs#L89) | Map places use structural ordering when the map was built using a custom `Ord`. | Read failure reproduced with both engine configurations; write ordering follows from inspection. |
| 12 | [typ/cast.rs:144](src/typ/cast.rs#L144) | Casting a two-element array to a list silently discards its second element. | Reproduced with both engine configurations. |
| 13 | [typ/cast.rs:232](src/typ/cast.rs#L232) | Struct casts reject convertible fields before attempting recursive conversion. | Reproduced with both engine configurations. |
| 14 | [expr/resolver.rs:832](src/expr/resolver.rs#L832) | A second, incomplete AST walker leaves modules unresolved in several expression positions. | Accessor and labeled-default examples fail; lambda-body control succeeds. |
| 15 | [expr/resolver.rs:299](src/expr/resolver.rs#L299) | Interface read errors silently remove the interface contract. | `--check` accepts a source beside an invalid-UTF-8 `.gxi`. |
| 16 | [node/traits.rs:416](src/node/traits.rs#L416) | Aliasing populates the map later used to reject undeclared impl variables, making that check ineffective. | `--check` accepts an impl with an undeclared `'a`. |
| 17 | [env.rs:1479](src/env.rs#L1479) | Scope removal leaves abstract representations and bind-associated registry entries behind. | Focused Rust test for abstract representations; other registries inspected. |
| 18 | [node/coretraits.rs:130](src/node/coretraits.rs#L130) | Core-trait cache entries outlive changes to the implementation registry. | Inspection of positive/negative entries and registry mutation paths. |
| 19 | [shared_map.rs:289](src/shared_map.rs#L289) | The advertised standalone Pack mode cannot decode its own output; its documentation describes a different protocol. | Focused Rust round-trip test fails with `InvalidFormat`. |
| 20 | [fusion/kernel.rs:191](src/fusion/kernel.rs#L191) | Kernel updates duplicate parameter state across parallel buffers and allocate on every spill. | Inspection: the wire-slot buffer spills at seven parameters. |
| 21 | [typ/normalize.rs:86](src/typ/normalize.rs#L86) | Normalization boxes every iterator and copies nested sets despite using a pooled traversal stack. | Inspection; no allocation benchmark claimed. |
| 22 | [node/data.rs:92](src/node/data.rs#L92) | Struct image codecs clone field-name vectors in both passes instead of using the existing borrowed-slice codec. | Inspection. |
| 23 | [fusion/intern.rs:54](src/fusion/intern.rs#L54) | Interner collection may never revisit strings whose final kernel owner was dropped after a scan. | Inspection of insertion-counter gating; existing tests bypass the scheduler. |
| 24 | [examples/gxfmt.rs:45](examples/gxfmt.rs#L45) | The formatter corpus harness reports bugs but exits successfully. | Inspection of `bad`, error classification, and `main`'s return path. |
| 25 | [node/coretraits.rs:405](src/node/coretraits.rs#L405) | Core-trait callbacks reconstruct mutable context references while the enclosing safe closure still borrows the context. | Inspection of `with_value_hooks` and its dispatch callbacks. |
| 26 | [expr/seq.rs:308](src/expr/seq.rs#L308) | Skipping lambda bodies performs quadratic traversal and builds an unnecessary exclusion set. | Inspection: each nested lambda walks its entire subtree again. |
| 27 | [shared_map.rs:213](src/shared_map.rs#L213) | Nested shared maps violate exact image lengths, invalidating length-prefixed framing. | Focused Rust test measures 60 bytes but writes 42. |

## Validation

- `cargo test -p graphix-compiler`: **204 passed**, one slow test ignored.
- `cargo build -p graphix-shell`: passed. CLI reproductions below were repeated
  against that rebuilt shell, with `--no-cache`, in both the default and
  `--no-fusion` configurations for runtime cases. Agreement does not establish
  correctness when both configurations use the same faulty path.
- Focused temporary Rust tests reproduced CR17, CR19, and CR27. They asserted the
  intended behavior and failed; they were removed after recording the results.
- `cargo fmt -p graphix-compiler --check`: passed.
- `cargo fmt --all --check`: reports existing formatting differences outside
  this crate in sibling `netidx` files: `graphix-package-netidx-admin/src/lifecycle.rs`,
  `netidx-admin/src/lib.rs`, and `netidx-tools/src/admin/tui_old/remote.rs`.
- The full workspace suite and slow-test release gate were not run for this
  comment-only review.

## CLI reproductions

For each runtime body below, append:

```graphix
sys::exit(sys::time::after_idle(duration:0.01s, 0))
```

Run using `~/tmp/target/debug/graphix --no-netidx --no-init --no-cache case.gx`,
then repeat with `--no-fusion`. Each snippet is a separate program.

### CR05: recursive conversion

```graphix
type Loop = [i64, Loop];
println(cast<Loop>("not-a-number"));
```

The program passes `--check`, then aborts with a stack overflow (SIGABRT).
The conversion should return an invalid-cast result. Core dumps were disabled
for this reproduction.

### CR06: a bottom address

```graphix
let x = 10;
let r: [&i64, null] = &x;
r <- null;
println(sys::time::timer(duration:0.002s, false) ~ *(r$));
```

Prints `10` after the reference has become null; the dereference should be bottom.

### CR07: a changed address

```graphix
let x = 10;
let y = 20;
let choose = false;
choose <- true;
let r = select choose { false => &x, true => &y };
println(*r);
```

Prints only `10`; the fired reference change should also produce `20`.

### CR08: consultation beyond the mask

Generate a select with arms `0 => 0` through `63 => 63`, followed by
`64 if never<bool>() => 64` and `_ => -1`, and print its result for scrutinee `0`.
It prints nothing. Moving that never-produced guard to arm index 63 restores
the expected `0`. This generator produces both complete files:

```python
from pathlib import Path

for index in (63, 64):
    arms = ",\n".join(f"{i} => {i}" for i in range(index))
    Path(f"guards{index}.gx").write_text(
        f"let x = 0; let r = select x {{ {arms},\n"
        f"{index} if never<bool>() => {index}, _ => -1 }};\n"
        "println(r);\n"
        "sys::exit(sys::time::after_idle(duration:0.01s, 0))\n"
    )
```

### CR09: a bottom function

```graphix
let f: [fn(x: i64) -> i64, null] = |x| x + 1;
f <- null;
println((f$)(sys::time::timer(duration:0.002s, false) ~ 10));
```

Prints `11`; the invocation should be bottom because there is no callee.

### CR10: a generic core implementation

```graphix
type Box<'a> = Abstract<'a>;
impl<'a> Eq for Box<'a> { let eq = |a, b| true };
println(Box(1) == Box(2));
```

Prints `false`; the declared implementation returns `true`.

### CR11: custom ordering through a place

```graphix
type Rev = Abstract<i64>;
impl Ord for Rev {
    let cmp = |a, b| select (a.0, b.0) {
        (x, y) if x > y => `Less,
        (x, y) if x < y => `Greater,
        _ => `Equal
    }
};
let m = {Rev(1) => 10, Rev(2) => 20, Rev(3) => 30};
println(m{Rev(1)});
println(*(&m{Rev(1)}));
```

Only the direct lookup prints `10`; both lookups should find the same entry.

### CR12 and CR13: composite casts

```graphix
println(cast<List<i64>>([1, 2])?);
println(cast<List<i64>>([1, 2, 3])?);
println(cast<{x: i64}>({x: "1"}));
println(cast<(i64, i64)>(("1", "2")));
```

The two-element array becomes `[<1>]`, while the three-element array retains
all three values. The struct conversion reports `InvalidCast`, while the
equivalent scalar conversions in the tuple produce `(1, 2)`.

### CR14: unresolved children

Create `helper.gx` containing `let x = 2`. These separate source files fail
`--check` with “external modules are not allowed in this context”:

```graphix
({mod helper; {x: helper::x}}).x
```

```graphix
let f = |#x = {mod helper; helper::x}, y| x + y;
f(1)
```

This control succeeds:

```graphix
let f = |y| {mod helper; helper::x + y};
f(1)
```

### CR15: an unreadable interface

Create `case.gx` containing `let x = 1`, and `case.gxi` containing the single
byte `0xff`. `--check case.gx` exits successfully. The interface's decoding
error should be reported instead of treating it as absent.

### CR16: undeclared impl variable

```graphix
trait Mark { val mark: fn(self) -> i64 };
impl Mark for Array<'a> { let mark = |x| 1 }
```

`--check` accepts this despite the missing `impl<'a>` declaration.

## Rust reproduction details

For CR17, start with `Env::default()` and register `Key` in a child scope through
`Env::deftype` with `TypeDefBody::Abstract(Some(Type::Any))`, no parameters, and
public visibility. Call `unbind_scope_subtree` on that scope, then register
`Key` again with `TypeDefBody::Abstract(None)`. For the same
`AbstractId::of(scope, "Key")`, `abstract_minted` still returns true and the
old representation remains available. The test's assertion that the new
opaque type is not Graphix-minted fails.

CR19's complete test body:

```rust
use bytes::BytesMut;
use graphix_compiler::{env::Map, shared_map::SharedMap};
use netidx_core::pack::Pack;

let mut map = Map::new();
map.insert_cow(1_u64, 2_u64);
let map = SharedMap(map);
let mut bytes = BytesMut::new();
map.encode(&mut bytes).unwrap();
assert_eq!(SharedMap::<u64, u64>::decode(&mut bytes.freeze()).unwrap(), map);
```

Encoding succeeds; the decode unwrap fails with `InvalidFormat`.

CR27's test body:

```rust
use graphix_compiler::{
    env::Map,
    image::{EncodeImage, ImageBuf, ImageEncoder},
    shared_map::SharedMap,
};
use netidx_core::pack::Pack;

let mut inner = Map::new();
inner.insert_cow(1_u64, 2_u64);
let inner = SharedMap(inner);
let mut outer = Map::new();
outer.insert_cow(1_u64, inner.clone());
outer.insert_cow(2_u64, inner);
let outer = SharedMap(outer);
let mut enc = ImageEncoder::new();
let _session = EncodeImage::new(&mut enc);
let len = outer.encoded_len();
let mut bytes = ImageBuf::with_capacity(len);
outer.encode(&mut bytes).unwrap();
assert_eq!(len, bytes.len());
```

The assertion fails with `len == 60` and `bytes.len() == 42`.

## Disposition

Every finding reproduced (CR05–CR16 by the CLI cases above, CR17/19/27 by
the Rust tests, the rest from the source). Each `CR` in the tree is now an
`XCR` beside its fix; the semantics fixes carry pins in `graphix-tests`.

| CR | Resolution |
|---|---|
| 01 | Every helper that reinterprets bits or dereferences a raw pointer is `unsafe fn` (`va_ref`/`va_owned` too); the block-tree frees are `unsafe` with their ownership contract. |
| 02 | `EncodeImage`/`DecodeImage` are closure-scoped (`::with`); the guards are private, so a session cannot be leaked or dropped out of order. shared_map's own guards are gone (see 04). |
| 03 | The encoder keeps a clone of the first expression seen per id; the address is only a key. |
| 04, 19, 27 | Map nodes are ordinary image objects (`object_len`/`object_encode`/`object_decode`, ordinal references, trailer offsets): exact lengths, no second thread-local table, and the session requirement is the stated contract (a decode outside one is `InvalidFormat`, pinned). |
| 05 | `cast_value_int` borrows the value and refuses a type name re-entered on the current path over the same value, under the stack guard. Pin: `types::cast_recursive_no_progress`. |
| 06 | A bottom address bottoms the deref; the subscription stays. Pin: `byref::deref_null_ref_is_bottom`. |
| 07 | The address's fire joins the referent's tag. Pin: `byref::deref_fires_on_address`. |
| 08 | `ArmMask`: one bit per arm. Pin: `select::guard_beyond_sixty_four_arms`. |
| 09 | A bottom dynamic callee is a consumed bottom input: the site produces bottom and the instance is not dispatched. Pin: `functions::null_callee_is_bottom`. |
| 10 | Runtime dispatch finds implementations by the abstract id (through a `Type::Ref` target) and checks a generic target's payload type against the value. Pin: `traits::core_eq_generic_impl`. |
| 11 | `read_path`/`write_path` run under `with_key_ord_hooks` at the deref and at the runtime's patch delivery. Pin: `traits::core_ord_map_place`. |
| 12 | The whole spine decides whether a value is a list. Pin: `types::cast_pair_to_list`. |
| 13 | Field names decide the match; each field converts. Pin: `types::cast_struct_fields`. |
| 14 | The resolver walks through `for_each_child`/`map_children`; only a module node is special. Pin: `modules::mod_in_every_expression_position`. |
| 15 | `read_optional`: only a missing file is absent; every other read failure is an error with the path (resolver and `RootFile::load`). |
| 16 | Undeclared variables are checked before aliasing, in the target and in constraints. Pin: `traits::impl_undeclared_tvar`. |
| 17 | `unbind_scope_subtree` removes the abstract reps of removed typedefs and the removed binds' `poly_binds`/`byref_chain` entries. Pin: `env::test::unbind_scope_drops_abstract_reps`. |
| 18 | A hook-site entry carries the identity of the implementation list it was resolved against and is rebuilt when it changes. |
| 20 | `polled` is the one per-param record; the wire-slot buffer is sized for the same parameter count as the others. |
| 21 | `flatten_set` iterates the input as itself and keeps nested sets as pooled `(members, index)` frames. |
| 22 | `image::slice_len`/`slice_encode` for the field names. |
| 23 | The interner sweeps every interval; no insertion counter. |
| 24 | `gxfmt` exits non-zero on a read error or a `Refused` (matched by type). |
| 25 | Accepted as the seam's design: `Value`'s own `Eq`/`Ord`/`Debug` carry no context, so the hook must re-enter the context the caller is inside. `with_value_hooks` is now `unsafe fn` with the contract stated (no reference derived from `ctx`/`event` may be live across a `Value` operation). Known stretch: the typed printers hold `&ctx.env` while printing. |
| 26 | The walk prunes at a lambda. |
