# Compiler review follow-up

Rechecked all 21 findings against `dccec8ee` on 2026-09-22.
**15 are closed; six remain open (two P1, four P2).** Resolved comments
are removed. The six remaining comments are `CR`, with updated evidence
for incomplete fixes or problems introduced by the fixes. This follow-up
changes review comments and this record only.

## Disposition

| ID | Status | Assessment |
| --- | --- | --- |
| CR01 | Closed | Abstract predicates check concrete parameters without binding inference cells; the original example selects `Text("hi")` in both engines. |
| CR02 | Closed | Binary core-trait dispatch checks both operand instantiations; mixed equality is `(false, false)` in both engines. |
| CR03 | Closed | Both tail-rebind paths carry bottom into the formal; the original tail example produces no output in either engine. |
| CR04 | Closed | Effects are inferred per instance and joined for definition summaries; the unrelated pure call no longer makes an invalid `#[sync]` pass. |
| CR05 | Closed | Active-source tracking rejects the two-file import cycle with an error; the sibling-import regression passes. |
| CR06 | Closed | Dependent imports split the sortable run; the original alias-import example still compiles and prints `1` after formatting. |
| CR07 | Closed | Configuration discovery starts from an absolute directory; the relative-directory subprocess regression passes. |
| CR08 | Closed | A place owns one compiled address expression; the original index side effect runs once in both engines. |
| CR09 | **Open, P1** | A bottom key is handled, but a bottom dereferenced root still supplies its previous address. |
| CR10 | Closed | A failed current place read bottoms; the removed-element example produces no output in either engine. |
| CR11 | **Open, P2** | Error-valued elements work, but the replacement place typechecker omits array-index validation. |
| CR12 | **Open, P2** | Parenthesized nested writes reach the root, but the composed path is applied twice when publishing the reference's mirror. |
| CR13 | **Open, P1** | The original frame test passes, but a separate length query before encoding still corrupts the frame. |
| CR14 | Closed | Typed rendering validates at the root and grows the stack; nested-array and deep recursive-union regressions pass. |
| CR15 | Closed | Reentrant dispatch loans individual sites; profiling the original three renders shows two runtime Display instances total. |
| CR16 | Closed | Negative hook lookup uses borrowed abstract values; argument wrappers are constructed only for an actual dispatch. |
| CR17 | Closed | Module resolution propagates child changes in one walk; the whole-subtree prescan is gone. |
| CR18 | **Open, P2** | Main machine builders are pooled, but recursive rewrite scopes still allocate scratch maps and vectors. |
| CR19 | Closed | Block, Module and CallSite image codecs use borrowed slices instead of temporary Vec copies. |
| CR20 | **Open, P2** | Builder boxes are reused, but the new spare cache retains oversized allocations without a capacity limit. |
| CR21 | Closed | The unused helper is removed and tag documentation describes independent firing and bottomness. |

## Remaining findings

### CR09: a bottom dereference leaves an addressable place

Location: [Place::update](src/node/bind.rs#L913).

```gx
let a = [10, 20];
let r: [&Array<i64>, null] = &a;
r <- null;
let s = &(*r$)[0];
println(sys::time::timer(duration:0.002s, false) ~ *s);
sys::exit(sys::time::after_idle(duration:0.01s, 0))
```

Both engines print `10`; the reference address is bottom at the timer,
so neither should print. `Deref::update` returns bottom before clearing or
invalidating its saved `id`/`path`, and `root_place` reads those saved fields.
`Place::update` therefore reports a complete address and ByRef retains the
registration. Represent whether the current dereference address is present
separately from whether its referent's value is bottom; a known address into
a bottom value must remain distinguishable from an unknown address.

### CR11: place indices bypass integer validation

Location: [Place::elem_type](src/node/bind.rs#L988).

```gx
let a = [10];
let r = &a["0"];
println(*r);
sys::exit(sys::time::after_idle(duration:0.01s, 0))
```

Both engines accept this and print `10`. Replacing the reference with
`println(a["0"])` correctly fails with `Int does not contain string`.
The `PlaceStep::Index(_)` branch checks only the array container; the runtime
then casts the unchecked index to i64. Share the ordinary access node's
index validation while retaining the corrected element type. The original
Error-valued-field reproduction now passes.

### CR12: composed places publish the wrong mirror

Location: [ByRef::update](src/node/bind.rs#L1212).

Evaluate this through `graphix_package_core::testing::eval`:

```gx
{ let a = {p: {x: 10}}; let r = &a.p; &(*r).x }
```

Convert the returned value to `BindId`, then read its cell with
`ctx.rt.with_ctx(move |ctx| ctx.rt.store_value(&id))`. The focused Rust
probe returns `None`, rather than `Some(Value::I64(10))`.

The dereference's production is already `a.p`, but the registered path is
the full `[p, x]`. Applying that full path to `a.p` fails and bottoms the
mirror that embedders read. Use the remaining suffix with the dereference's
production, or the full path with the actual root binding's production.
Ordinary Graphix reads and writes through the composed registration now
work; the original nested-write example prints `20` in both engines.

### CR13: measuring before encoding corrupts a derived frame

Location: [ImageEncoder::query_seen](src/image/mod.rs#L167).

In `image::tests::a_frame_measures_what_it_writes`, insert a separate
`let _ = pair.encoded_len();` at the start of the encoding closure, after
`enc.begin_encode()` and before `pair.encode(...)`. Leave the existing
length pass and decoding assertions intact.

The focused probe measures and writes 15 bytes, but `Pair::decode` fails
with `PackError::BufferShort`. `query_seen` survives the separate query,
so the derived encoder's own frame-length query treats definitions as
references. Its subsequent writes still emit the definitions. Clearing
measurement state only on a write does not separate independent queries;
the state must be scoped to a complete measurement while preserving sharing
between fields within that measurement.

### CR18: recursive seq rewrites still allocate scratch collections

Location: [rewrite_with_inner](src/expr/seq.rs#L1334).

The main builders are improved, but `TryWith`, `Seq`, `Do`, `Select`,
`Catch` and `Lambda` still clone a plain `AHashMap` for each rewrite scope.
`TryWith`, `Seq` and `Do` build plain temporary Vecs and copy them into Arc
slices. Lambda argument rebuilding and `pc_type` use the same temporary
Vec-to-Arc pattern. These are intermediate collections, not retained AST
storage. Pool the mutable staging and drain it into the final slices; use
`Arc::from_iter` where no mutable staging is needed. This finding is based
on the collection lifetimes and code paths, not an allocation benchmark.

### CR20: cached builders retain unbounded capacity

Location: [Spares::give](src/fusion/emit_helpers.rs#L247).

A focused Rust probe using `fusion::emit_helpers`:

```rust
unsafe {
    let large = graphix_value_buf_new(1_000_000);
    graphix_value_buf_drop(large);
    let small = graphix_value_buf_new(1);
    let retained = (*small).capacity();
    graphix_value_buf_drop(small);
    assert!(retained < 1_000_000);
}
```

The assertion fails: the one-element builder retains one million Value
slots, or 16 MB. `Spares` caps the number of boxes at 64 per thread but
never checks the retained capacity. Keeping the `LPooled` handle in a box
also prevents its collection from reaching the pool's normal return
policy. The string builder cache has the same unbounded-capacity policy.
Bound retained capacity as well as count, releasing outliers while keeping
ordinary builder reuse.

## Validation

- Full workspace `cargo test`: **3,449 passed, 12 ignored, zero failed**
  in the reported test totals. This includes the new regression tests.
- Rebuilt `graphix-shell`; reran the original semantic reproductions with
  fusion enabled and with `--no-fusion`, using `--no-netidx --no-init
  --no-cache`. Rechecked import-cycle rejection, formatting of dependent
  imports, and runtime Display instance counts.
- Additional Graphix probes reproduced CR09 and CR11 in both engines.
  Three temporary Rust probes reproduced CR12, CR13 and CR20. Their
  failures are the evidence above; the temporary test files were removed.
- No implementation changes, slow-test release gate, or long fuzzer run
  are part of this follow-up.

## Response to the follow-up

| ID | Resolution | Pin |
| --- | --- | --- |
| CR09 | `Deref` holds its address as one `Option<(BindId, Path)>`, released (and unsubscribed) while its reference is bottom; `root_place` reads it, so the place unregisters, its cell bottoms, and a write through it goes nowhere until the reference returns. | `lang::byref::place_through_bottom_deref` (the old build gives `([99, 20], 10, [7, 20])` for `([10, 20], null, [7, 20])`) |
| CR11 | `node::array::check_index` is the one index rule for `a[i]`, slice bounds and a place's index step. | `lang::byref::place_index_is_an_integer` |
| CR12 | `Place::update` resolves an `Address { bind, path, steps }`: registration uses the full path, the mirror reads `steps` (the place's own) from the root's production. | `lang::byref::place_through_deref_mirror` (reads `None` with the full path put back) |
| CR13 | **Open: needs a decision.** See below. | |
| CR18 | Scopes are pooled copies (`seq::scope`, also used by the lowering's visibility maps); rewritten sequences and lambda args go straight into `Arc::from_iter`; `pc_type`, `pat_last` build their slices without staging; the seqq capture analysis (`captures`, `names`, `written`, `used`) is pooled. | existing seq tests |
| CR20 | `Shells<T>` recycles only empty boxes; the builder inside comes from its pool and returns to it, under the pool's capacity limit. The string builder is `LPooled<String>` too. | `fusion::emit_helpers::tests::a_builder_keeps_no_outsized_capacity` |

### CR13

The encode pass cannot tell a frame's sibling fields from a repeated
measurement. `Pair { a: T, b: T }` (one slot for both fields) measures as
two queries of `T` with no write between them, where the second is a
reference. Measuring `T` on its own and then again before writing it is
the same two queries, where the second must be a definition. Only the
code that made the calls knows which it was, and for a derived `Pack`
that is netidx's generated `encoded_len`. So no rule inside the session
can be exact while a definition is written inline at its first
occurrence, and the "function of its `Slot` alone" in CLAUDE.md is not
true of the current scheme.

Three ways out, for Eric:

- **Out-of-line definitions.** Every occurrence is a reference, and each
  definition is written once, in first-occurrence order, in an area
  after the body (like the deferred instance heap). The decoder already
  decodes a reference to an unbuilt object from its offset. A length is
  then a function of the ordinal alone, exact under any order of
  queries, and `query_seen`, `in_progress`, the nested-definition rule
  and `ContentState` go away. An encode-pass frame stops re-walking its
  subtree. Costs: a format change, a few bytes per object, a first read
  that jumps to its definition, and a warm-start measurement plus a
  soak.
- **Measurement brackets from netidx.** netidx-derive's `encoded_len`
  marks the outermost measurement with a thread-local epoch, and the
  session scopes its marks by epoch. This is small, but it ties graphix
  correctness to a netidx thread-local, adds a TLS access to every
  derived `encoded_len`, and gives hand-written aggregating codecs the
  same obligation.
- **State the protocol.** In the encode pass, a length query describes
  the writes that follow it. That holds for every frame the writer makes
  today, but a stray `encoded_len()` (a size in a log line) silently
  corrupts the image.
