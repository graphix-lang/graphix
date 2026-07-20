# Compiler code review — 2026-07-19

Single-context review of `graphix-compiler` (Claude), focused per
Eric's charter on design, simplification, readability, and overall
quality — not bug hunting (the differential fuzzer owns that). Scope:
all of `fusion/`, the churn-heavy `node/` files (collection, callsite,
lambda, select, mod), the `typ/` core (fntyp — including the
previously-unreviewed `quantifiers` field — tvar, mod), `tval.rs`,
plus outline-level passes of `analysis.rs`, `lib.rs`, `op.rs`,
`error.rs`, and `expr/print.rs`. Roughly 30k of the crate's 55k lines
read line-by-line.

**Overall verdict:** the code is in much better shape than the pace of
change suggested. Comment quality is exceptional — nearly every
decision carries its soak-finding provenance, and the semantic
invariants (bottom/taint, firing, ownership) are documented where they
are enforced. The findings below are almost entirely *structural*:
duplication that accreted arc-by-arc, docs the unified-ABI changeover
orphaned (fixed in this commit), and a handful of design-level
consolidations that would make the next arc cheaper.

The commit accompanying this review fixes only the unambiguous drift:
stale docs describing deleted machinery (kind-grouped layout,
`CALLEE_RESULT_FLAGS`, `string_params`/`value_params`, `eval_kernel`,
`build_kir_kernel`, GIR-era milestone names), dead code
(`value_new_prim_helper`, `KernelStrings`/`KernelValues` pre-walk
fields, `FusedKernel::_phantom`, no-op function-local `use`s, the
unused `_original_scope`), and a few garbled comments. Everything else
is proposed here for adjudication.

## A. Headline proposals (each needs a ruling)

### A1. One declarative registry for the JIT helpers
**DONE (7ebbeca6):** `jit_helpers!` in emit_helpers.rs is the single
source; CLIF signatures derive from the Rust types; the legacy table
was verified byte-identical for all 124 helpers, then deleted.

The ~90 runtime helpers are registered in THREE hand-synchronized
places joined only by string names: the definition
(`fusion/emit_helpers.rs`), the name→pointer table
(`emit_helpers.rs::all_symbols`, ~170 lines where every symbol name is
typed twice), and the name→CLIF-signature table
(`fusion/emit.rs::helper_signature`, ~480 lines of string-matched
signature construction). A missed entry is a compile-time error path,
but a *wrong signature* is silent UB at the call boundary. A single
macro that takes the helper definition and derives symbol + CLIF
signature from the Rust parameter types (i64→I64, f64→F64,
TagValue→(I64,I64), ArcStr/u64-bits→I64, u8→uext I8, i16→sext I16 …)
would delete ~600 lines across the two files and eliminate the drift
class. The "explicit is clearer" comment at `emit.rs:2017` predates
the current scale. Related: the `#[unsafe(no_mangle)]` attributes are
inconsistently applied (the value-arith/eq/bytes/map/slice helpers
lack them) and are all redundant — symbols are registered by pointer
via `JITBuilder::symbol`, never resolved by name — so the macro can
drop them uniformly.

### A2. Split `fusion/emit.rs`
**DONE (cadda57d):** emit/ split per area (jit/lower/abi/body/nodes/
flow/select/call/scalar + scaffold); external paths stable via mod.rs
re-exports; CLIF verified instruction-identical across the split.

At 10.2k lines it is 18% of the compiler in one file. Its own section
markers already delineate coherent modules: the JIT module/wrapper
pipeline (~94–2500), the disc/taint algebra + `JitEnv`/`LowerCtx`
(~2503–3100), body/statement/tail emission (~3089–5850), the per-node
emitters (~4561–7270), select (~7278–8860), lambda calls/marshalling
(~8860–9640), and scalar plumbing (tail). A mechanical split along
those seams (emit/{jit,abi,body,nodes,select,call,scalar}.rs) is
low-risk and makes the emit contracts per-area reviewable. This is
the legibility motivation of `design/fusion_lowering_split.md` applied
where it bites hardest.

### A3. Deduplicate the collection-intrinsic ops (`node/collection.rs`)
**DONE:** `Flavor` enum (Array/List/CMap source + result hooks) +
one `emit_*_kind` fn per loop kind; the 21 ops delegate; the
`Slot::new`/`FoldSlot::new` reference construction hoisted. CLIF
verified instruction-identical across the refactor (4 programs, 88
kernels).

The 19 `MapFn`/`FoldFn` impls are template clones: every List/Map op
differs from its Array twin ONLY in a flatten helper
(`graphix_list_to_valarray` / `graphix_cmap_to_pairs` / none) and a
result-convert helper (`graphix_valarray_into_list` / `_into_cmap` /
`array_result`), with the eligibility gates (predicate-is-bool,
output-nullable, unit-or-null) repeated verbatim. Worst case:
`ArrayFold::emit_clif` and `emit_collection_fold` duplicate the entire
~70-line accumulator-shape classification, comments included.
Parameterizing the op trait with `SOURCE_FLATTEN: Option<&str>` and a
result-convert hook collapses ~900 lines to ~300 and single-sources
the gates. Also: `Slot::new`/`FoldSlot::new` duplicate the
`genn::reference` construction in both branches of their
prototype/live `if`.

### A4. Shared `Type` child-walkers (`typ/`)
**DONE:** `Type::try_for_each_child`/`for_each_child` (query) and
`Type::cow_children` (rebuild) in typ/mod.rs are the single child
enumerations; `FnType::try_for_each_type`/`for_each_type` +
`for_each_sig_constraint` (the 3×-copied guarded constraint walk) on
the FnType side. Converted: the eight tvar.rs walks,
`would_cycle_seen`'s recursion arms, mod.rs `tvar_free` /
`scope_refs` (now COW) / `any_as_tvar` (now COW) / `record_ide_refs`
(picking up its missing `vargs` coverage), and lowering.rs
`privatize_d` / `resolve_abstract_node` boring arms; lowering.rs's
duplicate `tvar_free` deleted in favor of `Type::tvar_free`. Every
Ref-arm skip is now an explicit override, behavior preserved verbatim
— the POLICY question is C8 below.

`typ/tvar.rs` alone hand-rolls eight exhaustive structural walks
(`unfreeze_tvars`, `alias_tvars`, `collect_tvars`,
`check_tvars_declared`, `has_unbound`, `bind_as`, `unbind_tvars`,
`unbind_open_tvars`); `lowering.rs` adds `privatize_d`,
`resolve_abstract_node`, and `tvar_free`; `would_cycle_seen` is
another. Two generic walkers — a read-only
`Type::for_each_child(&self, f)` and the existing COW pattern promoted
to `Type::cow_children` — would reduce each walk to its interesting
arms (TVar/Ref/Abstract) and put the exhaustive match in ONE place per
kind, so a new `Type` variant produces two compile errors instead of
fifteen scattered ones. Crucially, the current walks *silently
disagree on the `Ref` arm*: `alias_tvars`/`collect_tvars`/
`unfreeze_tvars` walk `Ref.params`, while `unbind_tvars`/
`unbind_open_tvars`/`has_unbound` skip them entirely (`has_unbound` on
a Ref with unbound params answers `false`). If each divergence is
deliberate it should be visible as an explicit override; if not, some
of these are latent bugs. The same treatment fits `FnType`, where nine
methods hand-walk args/vargs/rtype/throws and the guarded
cell-constraint walk is copy-pasted three times
(`alias_tvars`/`unfreeze_tvars`/`collect_tvars`).

### A5. One definition of "tail position"
Four places encode the same tail-position walk (root, Block last
child, ExplicitParens, Select arms): `analysis.rs::mark_tail_sites`,
`analysis.rs::body_has_self_tail_call`,
`lowering.rs::body_has_self_tail_call` (a near-twin keyed by BindId
instead of instance id), and the emitter's `emit_body_tail` dispatch.
Their agreement is semantics-bearing — the JIT's tail set must match
the analysis's or a TailCall emits without its loop — and today it is
maintained by hand. A shared `for_each_tail_position(node, f)` (or a
tail-position predicate) makes the agreement structural.

### A6. `LowerCtx` / `BodyEmitter` shape (`fusion/emit.rs`)
`LowerCtx` has ~35 fields; the per-instance state channel and the
per-call-site channel are structural twins
(ptr/enabled/next/replay/anchors) begging to be two instances of one
sub-struct, with the tail machinery (loop_head/param_mark/
tail_call_slots/tail_scrut_stale/tail_sel_path) a third. Separately,
the `BodyEmitter` trait has ten methods, nine of which are trivial
field getters that its SOLE implementor (`NodeBodyEmitter`) overrides
one-for-one (~80 lines of boilerplate + dead defaults); it could be
`fn emit()` plus a plain `BodySpec` struct of the nine data fields.
And `define_kernel_body` returns a 7-tuple destructured positionally
at its call site — a named struct (the fields mirror `CachedKernel`'s)
would name them once.

## B. Smaller simplifications (worth doing, low risk)

- `fusion/kernel.rs` staged arg-packer (~1158–1297): the first map
  returns `(disc, staged Value, scalar_payload, is_scalar)`, a second
  map folds the scalar payload into a `Value::U64` carrier, and the
  push loop re-matches the param kind with an `unreachable!` for the
  carrier. Staging `(disc: u64, payload: u64, keepalive: Value)` in
  one pass is equivalent (payload words are stable across the Value's
  move into the smallvec) and deletes the sentinel-carrier dance.
- `fusion/kernel.rs::dispatch_typed` (~528): drained the pooled
  `LPooled<Vec<Value>>` args into a fresh `Vec` per DynCall —
  allocating on the hot dispatch path AND forfeiting the pool return
  (FIXED in this commit: the pooled vec is now used directly).
  Remaining: `Kernel::update`'s `param_opts` is `vec![None; n]` per
  cycle where its siblings are smallvecs.
- `fusion/emit.rs` repetition: the per-kind drop match appears 4×
  (`emit_scope_drops`, `drop_owned_composites`, the tail-rebind
  non-slot drops, the select merge scrut drop) — one
  `emit_drop_local(kind, vv)`; `emit_bottom_abort` and
  `emit_interrupt_check` both inline the lazy pending-exit-block
  creation that `pending_exit_block()` already provides; the 4-instr
  STALE re-fold appears 3× in `emit_kernel_return`; `emit_qop_node`'s
  three shape arms repeat the bad-path/deliver/placeholder block
  nearly verbatim.
- `node/callsite.rs::resolve_static` inlines the body of its own
  `refresh_static_ftype` helper twice (~1110–1116, 1128–1134).
- `fusion/mod.rs::for_each_node`: if the 18 binary-op `NodeView` arms
  share a view type, collapse to or-patterns.
- `node/mod.rs::StringInterpolate::update` uses a raw
  `thread_local! RefCell<String>` buffer — the project's own guidance
  says `LPooled<String>`.
- `node/lambda.rs::Lambda::compile`: the closure-capture clones are
  underscore-named (`_scope`, `_env`, `_typ`, `_argspec`, `_spec`)
  despite being USED — inverting the convention and suppressing the
  compiler's unused-warning (which is how the genuinely-dead
  `_original_scope` survived; removed in this commit). Rename to
  `scope_for_init` etc., or capture one small struct.
- `typ/fntyp.rs`: `Display` and `PrettyDisplay` disagree on
  throws-suppression — `Display` suppresses `Type::Bottom`
  unconditionally while `PrettyDisplay` honors `explicit_throws`. One
  of them is wrong; they should share the suppression predicate.
- `fusion/scaffold.rs`: fine as-is — the seven loop emitters share a
  visible skeleton, but the file's instruction-stability contract
  justifies the explicitness. Optional `LoopFrame` prologue/epilogue
  helper only if it provably preserves emission order.

## C. Questions / inconsistencies needing a ruling

1. **`scalar_to_payload_i64` vs `pack_value_to_u64`** (`emit.rs`):
   the design doc says scalar payloads widen "per pack_value_to_u64's
   rules (signed ints sign-extend)", but the CLIF twin `uextend`s ALL
   narrow ints, signed included. Harmless today (every consumer
   truncates; `Value` equality compares typed payloads), but it
   violates the ABI doc's "the payload word IS the Value encoding"
   invariant — a future raw-word comparison or memo key would diverge.
   Either make the emitter `sextend` signed prims (one arm) or amend
   `design/unified_value_abi.md` to say upper payload bits are
   unspecified-but-unread. I lean toward the former: the invariant is
   the ABI's whole point.
2. **Dead-code warnings** (pre-existing, 4): `node/bind.rs:31
   Bind.scope`, `node/callsite.rs:278 Callee::Static.def` (if it's a
   deliberate keep-alive of the LambdaDef Value it deserves a comment
   or `_def`; if not it's droppable), `node/lambda.rs:1017
   Lambda.top_id/flags`, `node/op.rs:583 Op::base_op`.
3. **`Kernel::sleep` reachability**: `FusedKernel::sleep` deliberately
   does not delegate to `Kernel::sleep` (comment added this commit);
   as far as I can tell nothing else calls it, making `Kernel::sleep`'s
   input-slot clearing dead. Confirm and delete, or name its caller.
4. **Outstanding review comments**: `expr/print.rs:117` has an open
   `CR codex for eric` (the pretty-fit check compares total bytes
   written, not line width — mid-line/indented starts can exceed the
   column limit); `tval.rs:132` has a resolved `XCR estokes` awaiting
   your delete/re-open pass.
5. **Runtime `.expect` on bind failure** (`callsite.rs` ~1467, ~1501):
   `bind()` failures panic the runtime. Presumably unreachable after
   typecheck, but a dynamically-dispatched wrong-arity lambda value
   would be a user-input-triggered abort. Worth a deliberate
   log-and-bottom instead?
6. **`FnType::eq/cmp/hash` cost**: each computes `constraint_view`
   (a sig-tvar walk + sort) per call, and the kernel cache's BTreeMap
   runs several comparisons per lookup. Compile-time only; fine
   unless profiling says otherwise — noting so it's a known cost.
7. **Style sweep**: much of `fusion/` uses fully-qualified paths
   (`std::sync::Arc`, `compact_str::format_compact!`,
   `ahash::AHashMap`, …) for names used many times per file, against
   the project's use-statement convention. Cosmetic; a file-at-a-time
   sweep when touching each file anyway.
8. **The Ref-params skip in `has_unbound`/`bind_as`/`unbind_tvars`/
   `unbind_open_tvars`/`would_cycle_seen`** (surfaced by A4; behavior
   preserved verbatim behind explicit overrides). Analysis: for the
   unbind/bind gate walks the skip is de-facto harmless — a
   signature-STRUCTURAL `Alias<'a>` param is either concrete or a
   DECLARED tvar, and declared cells are rigid-gated during the def
   check, so there is nothing to unbind. The suspicious consumers are
   the two CLOSEDNESS tests: `constrain_known(closed_only)` and
   `unbind_open_tvars` both decide "is this inferred binding a closed
   fact?" via `has_unbound`, and a binding of the shape
   `Alias<'b-unbound>` (a Ref, e.g. `List<'b>`, unified into an
   anonymous cell from a declared rtype) reads as CLOSED — a partial
   mid-solve fact gets recorded / kept where the unbind-open rule
   says it must re-open (the first-writer-wins family those rules
   exist to prevent). No live witness; the structural twins
   (`Array<'b>` etc.) are walked correctly, so only NAMED aliases are
   exposed. Options: (a) make `has_unbound` walk Ref params (one-line
   now), accepting that closedness then reflects params; (b) keep the
   skip and document it as "Refs are opaque until expansion". I lean
   (a) for the closedness tests; `would_cycle_seen`'s skip looks
   genuinely deliberate (the guarded cell-graph walks never expand a
   name, so a Ref-mediated cycle can't hang them).

## D. What was NOT deeply reviewed

`typ/contains.rs`, `setops.rs`, `normalize.rs`, `cast.rs` (older,
documented in CLAUDE.md; only outline-checked), `expr/` (parser has
its own round-trip suite), `node/{data,pattern,bind,module,array,
error,op,compiler,genn}.rs` (outline + targeted reads only),
`env.rs`, `ide.rs`, `node_shape.rs`, graphix-rt. A second session
could cover `contains.rs`/`setops.rs` — they are the remaining
soundness-bearing files without a recent read.
