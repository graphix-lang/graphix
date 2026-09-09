# The unified Value ABI: one (disc, payload) encoding at every seam

Status: built 2026-07
Pins: `graphix-fuzz/findings/value-shape-seam-jul2026/`, `graphix-fuzz/findings/abort-seam-jul2026/`, `stdlib/graphix-tests/src/lang/fusion.rs`

## The rule

At every kernel seam — region entry, region return, cross-kernel call
arguments and returns, fast-fn calls — a datum is a two-word
`(disc, payload)` pair where BOTH words are exactly the netidx `Value`
encoding:

- `disc` = the one-hot `Value` discriminant (bits 0–31) OR'd with the
  graphix tag bits (`STALE`, `TAINT`; `tval.rs` reserves the top byte
  as `TAG_MASK`). Disc 0 is never a real value — it is the pending
  sentinel, and `(0, 0)` is the uniform abort return.
- `payload` = the genuine `Value` payload word for that disc:
  - scalars: the value bits, widened per `pack_value_to_u64` (signed
    ints sign-extend, unsigned zero-extend, floats bit-cast;
    `scalar_to_payload_i64` is the CLIF twin);
  - arrays / tuples / structs: the `ValArray` bits — a
    `#[repr(transparent)]` one-word thin-Arc handle. There is no box:
    the word IS the handle, identical to `Value::Array`'s payload;
  - strings: the `ArcStr` bits (identical to `Value::String`'s
    payload);
  - variant / nullable / bare-value shapes: the `Value` payload word.

Every kernel returns two words (`AbiReturn::Pair`). Because every seam
pair IS a tagged `Value`, the runtime decodes every kernel result
through one arm (`TagValue::from_raw`), a callee's TAINT/STALE travels
in-band in the disc, and a producer whose static type disagrees with a
consumer's cannot manufacture a mis-encoded word: the encoding is the
same on both sides by construction.

## Why

Six soak findings were one defect: the payload word's encoding was
chosen from a static type, and producer and consumer could hold
DIFFERENT static types for the same word — inference widens a call
expression's type to the consumer's union, so a callee froze a String
return one way and the caller read it another. Each fix patched one
seam (a widening layer here, a callee-result flag side channel there,
a five-arm dispatch return protocol) and the next finding was the same
bug at the next seam. With one encoding there is nothing to disagree
about, and the reconciliation layers reduced to their scalar widening
arms plus debug assertions.

The box the composite handle replaced (`*mut ValArray` behind
`Box::into_raw`) also cost an allocation at every composite entry
clone, cross-kernel composite arg, and borrowed→owned conversion; all
of those are now bare refcount bumps.

## Register classes are not encoding

Interior to a kernel, scalars live in native CLIF registers (F64 in FP
registers, I32 as I32). Cross-kernel CLIF calls keep native register
classes for scalar payload words — the BITS are the Value encoding
either way; only memory seams (the wrapper's flat `u64` slot buffer,
the runtime packer) see the widened form. The wrapper widens scalar
results on store and narrow-loads scalar params on entry, which is
sound because the packer stores the sign/zero-extended Value form.

## Ownership discipline

The one-word composite/string handle follows the discipline the box
had, minus the allocation:

- OWNED bits are produced by finalize / clone / consuming helpers, and
  dropped exactly once (`graphix_valarray_drop`, `graphix_arcstr_drop`)
  or transferred (a consuming helper, a return, a call-arg drop).
- BORROWED bits are a read of an existing local/param/slot — the same
  word, no refcount bump; the borrower must not drop. Tracked at emit
  time by `CompositeSource::{Owned, Borrowed}`;
  `ensure_owned_composite_src` converts by a bare refcount bump
  (`graphix_valarray_clone(bits) -> bits`).
- Helper extern signatures use `u64` for handle words: `ValArray`/
  `ArcStr` have NonNull niches, so a typed parameter holding the 0
  sentinel would be UB at the boundary — helpers assert non-zero and
  transmute internally.
- Placeholders (the taint channel's shape-safe stand-ins) are owned
  clones of a static EMPTY array / empty `ArcStr`; dropping one is a
  refcount decrement on a static that never reaches zero.
- Checked narrowing stays: `graphix_value_into_array(_borrowed)`
  (Value pair → array bits) PANICS (a defined abort) on a non-Array —
  the tainted `Value::Null` placeholder is reachable on `?`-unwrap
  paths and must never be reinterpreted as array bits.
