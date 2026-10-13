# The JIT helper ABI: nothing wider than a word crosses by value

Status: built.

Pins: every `jit` and `jit_par` fixture (each fused kernel calls
helpers through this seam); `fastcall_tags_follow_the_arg_discs`
(`fusion/emit_helpers.rs`) calls a pair-returning helper the way a
kernel does; the language suite on the lab's `win11` guest (a
cross-built `graphix-tests` for `x86_64-pc-windows-gnu`, run over ssh).

## The rule

A fused kernel calls Rust helpers (`fusion/emit_helpers.rs`) through
Cranelift `call` instructions. A two-word datum (`TagValue`,
`ChildBlock`) never crosses that seam by value, because the C ABIs
disagree on a 16-byte struct: System V x86_64 and AAPCS64 use two
registers, Win64 passes a pointer to a copy and returns through a
hidden pointer, and others differ again. The one shape every C ABI
agrees on is a machine word, so:

- **A pair parameter** is a pointer to the call site's two words. The
  call site stores them in a stack slot and passes its address; the
  helper reads them (`HelperArg::from_wire`) and owns them, or, for a
  `ManuallyDrop<TagValue>`, borrows them.
- **A pair return** is written through a trailing out-pointer. The call
  site passes the address of a two-word stack slot and loads the pair
  after the call; the helper returns nothing.

`AbiTy::Pair` marks such a slot in a helper's registered shape;
`helper_signature` turns it into one pointer parameter, and a pair
return into a trailing pointer parameter with no return slots.

## Where it is enforced

- **One call path.** `HelperRefs::call` (`emit/lower.rs`) is the only
  way to call a helper: it spills pair arguments, passes the out-slot,
  and returns the result words, a pair's included. `BodyCx::call_helper`
  and `LowerCtx::call_helper` delegate to it; there is no other way to
  get a helper's `FuncRef`.
- **The macro writes the seam.** `jit_helpers!` declares each helper's
  `extern "C"` definition from an ordinary `fn`: each parameter crosses
  as its `HelperArg::Wire`, and a `-> TagValue` or `-> ChildBlock`
  helper gets the out-pointer. Helper bodies are written against the
  plain types. A pair type has no by-value `HelperArg` impl, so a
  struct cannot cross by value.

The other seams were already portable: kernel entry is
`WrapperFn = unsafe extern "C" fn(args: *const u64, out: *mut u64)`
(`emit/jit.rs`), a loop chunk is scalars and a pointer
(`par_loop.rs`), and kernel→kernel calls are CLIF on both ends.

Cost: one 16-byte stack slot and two stores per pair argument, one
slot and two loads per pair return, beside a helper call that clones an
`Arc`, allocates or dispatches a builtin.

## Hosts

Fusion runs on every host Cranelift supports. The value encoding the
two engines share is little-endian (narrow scalars are read from a
value's low bytes), so a big-endian host is not supported
(`f-jit-10`).

The registration image cache is keyed by the executable's GNU build-id
note; a PE executable has none and is keyed by its COFF link stamp and
length (`graphix-shell/src/cache.rs`). An entry that fails to decode is
discarded with a warning and the session compiles cold
(`graphix-rt/src/gx.rs`): a cache is never fatal.
