# Compiler CR re-evaluation

Rechecked all 27 findings against `9bbd471d`, including the fixes in
`5d0c1d80`. **21 are resolved and their comments have been removed. Six remain
open**, with updated `CR codex for eric` comments beside the remaining problems.
This re-evaluation changes review comments and this report only.

## Open findings

| CR | Location | Remaining problem | Evidence |
|---|---|---|---|
| 01 | [fusion/emit_helpers.rs:841](src/fusion/emit_helpers.rs#L841) | `graphix_variant_tag_eq` remains a safe public function that dereferences an unchecked raw pointer. | Source inspection; invalid-pointer calls were not executed. |
| 03 | [image/mod.rs:1460](src/image/mod.rs#L1460) | Retaining an expression clone prevents the dangling dereference, but an input address reused for a different expression still hits the old identity. | Rust reproduction decodes `1, 1` after encoding `1, 2`. |
| 10 | [node/coretraits.rs:186](src/node/coretraits.rs#L186) | Core-trait dispatch still lacks the concrete abstract type arguments. The only candidate bypasses payload validation, and payload shape cannot identify phantom parameters. | Both reproductions below fail with fusion enabled and disabled. |
| 18 | [node/coretraits.rs:91](src/node/coretraits.rs#L91) | The cache version is an unpinned allocation address, which can be reused after registry removal or clearing. | Source inspection; no allocator-dependent runtime failure claimed. |
| 20 | [fusion/kernel.rs:197](src/fusion/kernel.rs#L197) | Three temporary SmallVec buffers still allocate on every update above 16 parameters; the tag/optional-value representation remains redundant. | Source inspection; increasing the inline capacity moves the spill threshold. |
| 25 | [node/coretraits.rs:475](src/node/coretraits.rs#L475) | The new unsafe contract is violated by existing typed-printing callers, which hold `&ctx.env` while a hook reconstructs and uses `&mut ExecCtx`. | Source inspection of interpolation, core printers, and `dispatch_fmt`; no undefined-behavior probe executed. |

## Validation

- `cargo test -p graphix-compiler -p graphix-tests`: **2,972 passed, two ignored**
  (207 compiler tests and 2,765 language/package tests).
- Rebuilt the shell and repeated the original CLI reproductions with
  `--no-netidx --no-init --no-cache`, using both default and `--no-fusion`
  configurations for runtime cases. The original runtime failures now produce
  the expected results, including the original generic `Box<'a>` equality case.
- The stronger CR10 cases below still fail in both configurations.
- A temporary Rust test reproduced CR03. It asserted the intended result and
  failed, then was removed after recording the reproduction here.
- The formatter corpus harness now exits with status 1 for a nonexistent file.
- `cargo fmt -p graphix-compiler --check` and `git diff --check`: passed.
- `cargo fmt --all --check` still reports existing differences in sibling
  `netidx` files: `graphix-package-netidx-admin/src/lifecycle.rs`,
  `netidx-admin/src/lib.rs`, and `netidx-tools/src/admin/tui_old/remote.rs`.

## CR03: expression address reuse

This safe Rust test keeps the input address constant while replacing its
contents with a freshly parsed expression. The first expression is encoded
before replacement, so retaining it is the encoder's responsibility.

```rust
use graphix_compiler::{
    expr::{Expr, parser::parse_one},
    image::{DecodeImage, EncodeImage, ImageBuf, ImageDecoder, ImageEncoder},
};
use netidx_core::pack::Pack;

let mut slot = Box::new(parse_one("1").unwrap());
let expected = parse_one("2").unwrap();
let mut encoder = ImageEncoder::new();
let bytes = EncodeImage::with(&mut encoder, || {
    let mut bytes = ImageBuf::with_capacity(0);
    slot.encode(&mut bytes).unwrap();
    *slot = expected.clone();
    slot.encode(&mut bytes).unwrap();
    bytes.freeze()
});
let mut decoder = ImageDecoder::new(encoder.counts());
decoder.set_image(bytes.clone());
decoder.set_offsets(encoder.take_offsets());
let actual = DecodeImage::with(&mut decoder, || {
    let mut remaining = &bytes[..];
    Expr::decode(&mut remaining).unwrap();
    Expr::decode(&mut remaining).unwrap()
});
assert_eq!(actual, expected);
```

The assertion reports `Constant(I64(1))` instead of `Constant(I64(2))`.
`expr_alias` returns by address before checking the new expression's id or
contents. The retained clone protects the old contents but does not reserve
its original input address.

## CR10: concrete abstract parameters

Run each complete program separately with the rebuilt shell, then repeat with
`--no-fusion`.

```graphix
type Box<'a> = Abstract<'a>;
impl Eq for Box<i64> { let eq = |a, b| true };
println(Box(1) == Box(2));
println(Box("a") == Box("b"));
sys::exit(sys::time::after_idle(duration:0.01s, 0))
```

Actual output is `true`, `true`. The second comparison has no matching
implementation and should use structural equality, producing `false`.
`SiteEntry::choose` accepts the sole candidate without checking its payload.

Checking that payload is necessary but cannot recover all type arguments:

```graphix
type Marker<'a> = Abstract<i64>;
impl Eq for Marker<i64> { let eq = |a, b| true };
impl Eq for Marker<string> { let eq = |a, b| false };
let a: Marker<string> = Marker(1);
let b: Marker<string> = Marker(1);
println(a == b);
sys::exit(sys::time::after_idle(duration:0.01s, 0))
```

Actual output is `true`; the declared `Marker<string>` implementation returns
`false`. Both candidate payload types are `i64`, so payload inspection cannot
select between them. Dispatch and its cached sites need the actual type
instantiation.

## Other open findings

**CR01:** the JIT helper macro expands `safe fn graphix_variant_tag_eq` into a
public safe `extern "C" fn`. Its `expected: *const ArcStr` argument is immediately
read through `&*expected`. The ValArray and ownership helpers have been corrected;
this remaining entry also needs an unsafe boundary and pointee contract.

**CR18:** `impls_version` takes `Arc::as_ptr(list).addr()`, but `SiteEntry` stores
only the integer. Neither its candidates nor its hook sites guarantee retaining
that exact list allocation. A later list at the same address passes the version
check despite representing a different registry. Retaining the list Arc in the
cache entry, or using a mutation generation, avoids address reuse.

**CR20:** `polled` and `staged` each hold 16 parameters inline; `slots` holds 35
words (three header words plus two per parameter). At 17 parameters, all three
allocate again on each invocation and free their storage at return. The parallel
parameter arrays were removed, but pooled or retained spill storage is still
needed. A `TagValue` also avoids independently representing bottomness and
optional presence.

**CR25:** `with_value_hooks` is now unsafe, but `StringInterpolate::update` and
the typed core printers call it while constructing `TVal { env: &ctx.env, ... }`.
That reference remains live throughout formatting. A user Display callback
reconstructs `&mut ExecCtx`, and constructing its hook site can mutate `ctx.env`.
The ordinary safe evaluation path still violates the new contract; the mutable
hook state needs separation from the context borrowed by the printer.

## Closed findings

| CRs removed | Verification |
|---|---|
| 02 | Encoder/decoder constructors are private; public closure-scoped entry points prevent leaked and out-of-order guards. |
| 04, 27 | Shared maps use the common object machinery with short encoder borrows and planned lengths. Exact-length, nested-sharing, round-trip and image tests passed. |
| 19 | The codec explicitly requires an image session. Its standalone-decoding claim and separate session guards are gone, and the session requirement is tested. |
| 05 | Recursive conversion returns `InvalidCast` instead of overflowing; recursion is guarded. |
| 06, 07 | Bottom references produce bottom; switching a reference between standing targets prints both values. |
| 08 | The 63/64-arm boundary reproductions both produce the selected value. Consultation has a bit for every arm. |
| 09 | A bottom dynamic callee suppresses invocation while retaining its instance. |
| 11 | Direct and reference map reads agree for reversed custom ordering; patch delivery installs ordering hooks too. |
| 12, 13 | Two-element array-to-list conversion retains both items; struct fields convert recursively. |
| 14 | Both the accessor and labeled-default module cases now compile; resolution uses the canonical child enumeration. |
| 15 | Invalid UTF-8 in the interface now fails checking with the interface path. |
| 16 | An undeclared impl type variable is rejected before aliasing. |
| 17 | The scope-removal regression test passes; abstract representations and bind-associated registry entries are removed. |
| 21 | Normalization uses a concrete input iterator and pooled slice/index frames without boxed iterators. |
| 22 | Struct images use the borrowed-slice codecs in both passes. |
| 23 | Every collection interval sweeps the interner, independent of new insertions. |
| 24 | The corpus harness uses a failure exit status, counts read failures, and recognizes Refused by type. |
| 26 | Sequence traversal prunes lambda children directly, eliminating the exclusion-set traversal. |

## Disposition of the re-evaluation

| CR | Resolution |
|---|---|
| 01 | `graphix_variant_tag_eq` is `unsafe fn` (the one multi-line signature the first sweep missed; no other safe helper takes a raw pointer). |
| 03 | An expression is keyed by the address of the session's own clone of the first tree seen with its id and contents (`exprs_by_id: ExprId → [Box<Expr>]`); the caller's address is never consulted, so a reused one names nothing. Pin: `image::tests::reused_address_is_not_an_alias` (the review's case). |
| 10 | A value carries the type arguments it was constructed at (`GxAbstract::params`, from the `Construct` node's checked type, in both engines) and a candidate is consulted only when its target `could_match`es that type, a declared variable open. Pins: `traits::core_eq_other_instantiation_is_structural`, `traits::core_eq_phantom_parameter` (the review's two cases). |
| 18 | The entry holds the implementation list's `Arc` and compares by `ptr_eq`; a cleared context has no list, which mismatches too. |
| 20 | `polled` is a pooled `Vec<TagValue>` (presence is the tag), `staged` and `slots` pooled `Vec`s: no spill at any arity. |
| 25 | The printers (`StringInterpolate`, `dbg`, `print`/`println`/`log`) and `uniq` no longer take a context under the loan: `with_display_hooks` gives the printer an `Env` that is a snapshot when a hook can fire (persistent maps make the clone a few `Arc` bumps) and `with_key_ord_hooks` covers the comparison alone. The kernel run reads a snapshot the same way. `with_value_hooks` stays `unsafe` for the two callers that must have the context under the loan (`==`-family ops, which compare after their child updates, and `CachedArgs::eval`, whose builtins read their arguments from `CachedVals`); its contract is stated at the signature. |
