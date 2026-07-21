# The Unified Value ABI

Status: BUILT (2026-07-19, Eric's ruling). One changeover, no staged
migration — the `*mut ValArray` box currency was a mistake; unwound
whole. Gates at landing: workspace 2355/0 (including new
string-cross-kernel coverage fixtures), regress 202/0, detcheck
242/0, selfcheck clean, composite bench ~10% faster than the box
baseline (bench/composite_seams.gx), leak probe flat (peak RSS
95.7MB at 100 rounds vs 95.9MB at 200 — valgrind SIGILLs in this
box's ld.so, so the leak gate is the differential RSS slope).

## The invariant

At every kernel seam — region entry, region return, cross-kernel call
arguments and returns, DynCall returns — a datum is a two-word
`(disc, payload)` pair where BOTH words are exactly the netidx
`Value` encoding:

- `disc` = the one-hot `Value` discriminant (bits 0–31) OR'd with the
  graphix tag bits (STALE = bit 61, TAINT = bit 62; `tval.rs`
  `TAG_MASK` reserves the top byte). Disc 0 is never a real value —
  it is the pending sentinel, and `(0, 0)` is the uniform abort
  return.
- `payload` = the genuine `Value` payload word for that disc:
  - scalars: the value bits, widened per `pack_value_to_u64`'s rules
    (signed ints sign-extend, unsigned zero-extend, floats bit-cast;
    `scalar_to_payload_i64` is the CLIF twin).
  - arrays / tuples / structs: the `ValArray` bits — a
    `#[repr(transparent)]` one-word thin-Arc handle. There is NO box:
    the word IS the handle, identical to `Value::Array`'s payload.
  - strings: the `ArcStr` bits (identical to `Value::String`'s
    payload — this was already true).
  - variant / nullable / bare value shapes: the `Value` payload word
    (already true).

Because every seam word pair IS a (tagged) `Value`, the runtime
decodes every kernel result through one arm (`TagValue::from_raw`),
callee TAINT/STALE travels in-band in the disc (the old
`CALLEE_RESULT_FLAGS` side channel is deleted), and a producer whose
static type disagrees with a consumer's (inference widening a call
node's type to a param union — the jul17a/jul18d crash class) can no
longer manufacture a mis-encoded word: the encoding is the same on
both sides by construction.

## Register classes are not encoding

Interior to a kernel, scalars live in native CLIF registers (F64 in
FP regs, I32 as I32). Cross-kernel CLIF calls keep native register
classes for scalar payload words — the BITS are the Value encoding
either way; only memory seams (the wrapper's flat `u64` slot buffer,
the runtime packer) see the widened form. The wrapper widens scalar
results on store (`scalar_to_payload_i64`) and narrow-loads scalar
params on entry (sound because the packer stores the sign/zero-
extended Value form).

## Ownership discipline (bits currency)

The one-word composite/string handle follows exactly the discipline
the box had, minus the allocation:

- OWNED bits: produced by finalize / clone / consuming helpers.
  Dropped exactly once (`graphix_valarray_drop(bits)`) or transferred
  (a consuming helper, a return, a `CallArgDrop`).
- BORROWED bits: a read of an existing local/param/slot — the same
  word, no refcount bump; the borrower must not drop. Tracked at
  emit time by `CompositeSource::{Owned,Borrowed}` exactly as
  before; `ensure_owned_composite_src` converts by a bare refcount
  bump (`graphix_valarray_clone(bits) -> bits`).
- Helper extern signatures use `u64` for handle words (the
  `graphix_arcstr_drop` pattern): `ValArray`/`ArcStr` have NonNull
  niches, so a typed parameter holding the 0 sentinel would be UB at
  the boundary — helpers assert non-zero and transmute internally.
- Placeholders (the #219 taint channel's shape-safe stand-ins) are
  owned clones of a static EMPTY array / empty `ArcStr` — dropping
  them is a refcount decrement on a static that never reaches zero.
- Checked narrowing stays: `graphix_value_into_array(_borrowed)`
  (Value pair → array bits) still PANICS (defined abort) on a
  non-Array — the tainted `Value::Null` placeholder is reachable on
  `?`-unwrap paths and must never be reinterpreted as array bits.

## What this deleted

- The `*mut ValArray` box: every `Box::into_raw`/`from_raw` site in
  emit_helpers, `graphix_valarray_empty_boxed`,
  `graphix_value_new_from_array` (a `(ARRAY|flags, bits)` pair is
  constructed inline now).
- `AbiReturn::One`/`Two` — every kernel returns two words.
- `CALLEE_RESULT_FLAGS`, `emit_flag_not_fresh`,
  `graphix_callee_flags_{set,take}`, and the caller-side disc
  synthesis after cross-kernel calls.
- The DynCall 5-arm `ret_kind` return protocol — every dispatch
  returns the Value pair; the call site adapts per its static type.
- The cross-kernel String/bare-Value ARG gate and the lowering-side
  String RETURN gate (they existed only because of the asymmetry).
- The reconciliation layers' composite/string arms
  (`widen_result_to_value`, the jul17a call-arg normalization, the
  GXLambda collection-hook widening) — reduced to their scalar
  widening arms plus debug assertions.

## Why

Six-plus soak findings (FoldAcc::Value seam, jul17a probe p2, jul18d
crash_000000, `fold_slice_init_disc`) were all the same defect: the
payload word's encoding was chosen from a static type, and producer
and consumer could hold DIFFERENT static types for the same word
(inference widens a call expression's type to the consumer's union).
With one encoding there is nothing to disagree about. The box also
cost an allocation at every composite entry clone, cross-kernel
composite arg, and borrowed→owned conversion; all of those are now
bare refcount bumps.
