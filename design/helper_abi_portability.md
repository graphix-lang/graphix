# The JIT helper ABI: two words is System V, not C

Status: found 2026-09-22 on x86_64 Windows; fusion is off there
(`cfg!(not(windows))` in `graphix_compiler::compile`) until the helper
seam is made portable as below
Pins: the netidx admin TUI on the lab's `win11` guest died with
`0xC0000005` before its first frame (netidx `lab/VERIFY-graphix-tui-REPORT.md`,
F27); no graphix test yet runs a fused kernel on a Win64 host

## The finding

A fused kernel calls Rust helpers (`fusion/emit_helpers.rs`) through
Cranelift `call` instructions whose signatures come from each helper's
`HelperSpec` (`emit/lower.rs`, `helper_signature`). The spec spells a
parameter or return as a list of machine slots, and two types are two
slots wide:

```rust
TagValue   => &[AbiTy::I64, AbiTy::I64];   // (disc, payload)
DynCallRet => &[AbiTy::I64, AbiTy::I64];   // (word0, word1)
```

On the Rust side the same helper is `pub extern "C" fn` taking or
returning the `#[repr(C)]` 16-byte struct itself. The two descriptions
agree only under the System V x86_64 ABI, where a 16-byte struct of two
integer words is classified INTEGER×2: passed in two consecutive
argument registers, returned in `RAX:RDX`. That is what the CLIF
signature says, so on Linux the two sides line up and the seam works.

Under the Win64 ABI they do not:

- an argument larger than 8 bytes is never in registers — the caller
  makes a copy and passes its **address** in the parameter's one
  register slot;
- a return larger than 8 bytes goes through a hidden **sret pointer**
  the caller passes in `RCX`, shifting every real argument one slot
  right; `RAX` returns that pointer.

Cranelift's `WindowsFastcall` convention does hand two `i64` returns
back in `RAX:RDX` (`isa/x64/abi.rs`, "the Rust ABI for i128s needs
this") and two `i64` parameters in `RCX, RDX`; that is a correct
implementation of the CLIF signature we asked for. The Rust callee is
simply not that function on Windows. So `graphix_value_buf_push_value`
receives the disc word where it expects a pointer to a `TagValue` and
dereferences it; `graphix_valarray_index` is called without the sret
slot its prologue writes through. Either is an access violation on the
first kernel that runs, which in the admin TUI is the first cycle.

AArch64 is not affected by luck rather than design: AAPCS64 passes a
composite of up to 16 bytes in two consecutive general registers and
returns it in `x0:x1`, which coincides with the two-slot CLIF
description. Nothing pins that.

### What the seam covers, and what it does not

Only helper calls cross a Rust `extern "C"` boundary with a struct by
value. The other seams are already portable:

- kernel entry from Rust is `WrapperFn = unsafe extern "C" fn(args:
  *const u64, out: *mut u64)` — two pointers, and the two-word result
  is written through `out` (`emit/jit.rs`);
- kernel→kernel and thunk calls are CLIF functions on both ends; the
  ISA's convention applies to both and Cranelift keeps them consistent;
- every other helper parameter is one slot — `u64`, `i64`, `usize`,
  `f64`, `f32`, narrow ints with an extension flag, `*mut T`, and
  `ArcStr` (a single pointer word). Win64 assigns floats to `XMM`
  registers by absolute position rather than by class, and Cranelift
  does that for a declared `F64` slot, so those agree.

The affected helpers today (`emit_helpers.rs`):

| takes a `TagValue` | returns a `TagValue` | returns a `DynCallRet` |
|---|---|---|
| `graphix_value_buf_push_value` | `graphix_value_clone_from_static` | `graphix_fastcall` |
| `graphix_value_buf_push_value_borrowed` | `graphix_valarray_into_list` | `graphix_typedcall` |
| | `graphix_valarray_into_cmap` | |
| | `graphix_valarray_index` | |
| | `graphix_valarray_get_value` | |
| | `graphix_struct_get_value` | |

Ten helpers. `fast_dispatch` also returns a `DynCallRet` but is a plain
Rust function called by the two dispatch helpers, not a seam.

## The rule to adopt

**Nothing crosses the helper seam by value that is wider than a machine
word.** A two-word datum is two scalar parameters going in, and an
out-pointer coming back. That is the one shape every C ABI agrees on,
it needs no per-platform code, and it is what the wrapper entry already
does.

Concretely:

1. **Arguments.** A helper that takes a `TagValue` takes `disc: u64,
   payload: u64` and rebuilds it with `TagValue::from_raw`. The
   `HelperArg` table keeps `TagValue => [I64, I64]` as the *CLIF* shape
   of the pair a call site already has in two SSA values; the Rust
   signature just stops pretending it is one argument. Two helpers.

2. **Returns.** A helper that returns a `TagValue` or a `DynCallRet`
   takes an extra trailing `out: *mut [u64; 2]` and returns nothing.
   The emitter allocates a two-word stack slot at the call site,
   passes its address as the last argument, and loads the pair after
   the call. `HelperRet` for the two pair types becomes "one pointer
   parameter, no return slots", so `helper_signature` grows the
   parameter list by one and `spec.ret` is empty for them; the call
   sites in `emit/call.rs` / `emit/scalar.rs` that today read two
   return values read two loads. Eight helpers.

3. **The macro enforces it.** `jit_helpers!` today accepts any type
   with a `HelperArg` impl. Remove the `HelperArg` impls for `TagValue`
   and `DynCallRet` (keep them as `HelperRet` in the out-pointer form),
   so a helper declared with a struct by value fails to compile rather
   than compiling into a SysV-only seam. The registry test
   `fastcall_tags_follow_the_arg_discs` already exercises the pair
   helpers; a second test should call each pair-returning helper
   through a JIT'd kernel so the load-after-call path is covered on
   every host the suite runs on.

4. **Lift the Windows gate** in `compile` once the ten helpers are
   converted, and run the fusion suite on `x86_64-pc-windows-gnu`
   before merging — with `wine` as the cargo runner
   (`CARGO_TARGET_X86_64_PC_WINDOWS_GNU_RUNNER=wine`) that is a normal
   `cargo test --target`, and the netidx admin TUI on the lab's `win11`
   guest is the integration check.

Cost: one 16-byte stack slot per pair-returning helper call and two
loads, next to a helper call that allocates, clones an `Arc`, or
dispatches a builtin. Not measurable against those.

### The alternative, and why not

Rust has `extern "sysv64"`, and Cranelift will emit a call under
`CallConv::SystemV` on any x86_64 host. Declaring the ten helpers
`extern "sysv64"` and building their signatures with `SystemV` instead
of `default_call_conv()` fixes x86_64 Windows in a dozen lines and
leaves every helper body alone. It is the wrong fix: it only exists on
x86_64 (`extern "sysv64"` is a compile error elsewhere, so the macro
would need `cfg_attr` per target), it keeps a struct-by-value seam whose
correctness depends on which ABI classifies `{u64, u64}` how — the
thing that just bit — and AArch64 would stay on its coincidence. The
out-pointer form has no such dependency and is the shape the rest of
the JIT already uses.

## Also found on Windows in the same run

Not ABI, but the same session and the same fix commit (`9bbd471d`):
the registration image cache is keyed by the executable's GNU build id
note (`graphix-shell/src/cache.rs`), and a PE executable has none, so
every Windows build shared the key `v<version>` and a rebuilt binary
read an earlier build's image (`UnknownTag`). A PE executable is now
keyed by its COFF link stamp and length. A registration image that
fails to decode is still fatal rather than discarded and rebuilt; that
is worth doing on its own, since a cache is a cache.
