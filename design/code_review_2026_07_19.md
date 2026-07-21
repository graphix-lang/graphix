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
**DONE:** `fusion::TailPosition` + `tail_position()` (the
classification: Block/Parens/Select formers vs Leaf) and
`fusion::for_each_tail_leaf` (the query/marking walk with a
Select-spine hook) are the single definition, next to
`for_each_node`. `analysis::mark_tail_sites`, both
`body_has_self_tail_call` twins, and `emit_body_tail`'s dispatch all
go through them — a new tail-position former is a compile error at
every site that must agree. The stale `emit_body_into`/`emit_do`/
`emit_tail` doc references (deleted machinery) were cleaned with it.

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
**DONE:** `StateChannel` (ptr/enabled/next/replay/anchors — one
struct, two instances `state`/`site` on `LowerCtx`) + `TailCtx`
(loop_head/param_mark/call_slots/scrut_stale/sel_path); the
`BodyEmitter` trait is down to its one real method (`emit`, still
dyn — the R/E type erasure is load-bearing) with the nine data
getters now a plain `BodySpec` struct riding alongside in
`BodySource`; `define_kernel_body`'s 7-tuple is the named
`DefinedBody`. Verified instruction-identical CLIF against the
pre-change binary.

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

All landed with A6 except where noted.

- **DONE** `fusion/kernel.rs` staged arg-packer: now stages
  `(disc: u64, payload: u64, keepalive: Value)` in one pass — the
  two-stage `Value::U64` sentinel-carrier dance and the push loop's
  re-match + `unreachable!` are gone (payload words are stable across
  the Value's move into the smallvec).
- **DONE** `Kernel::update`'s `param_opts` is a smallvec like its
  siblings. (`dispatch_typed`'s pooled-vec fix landed with the review
  commit.)
- **DONE** emit repetition: `emit_drop_local(b, ctx, kind, vv)` is
  the single per-kind drop dispatch (`drop_owned_composites`,
  `emit_scope_drops`, tail-rebind residual drops; the select merge's
  scrut drop deliberately keeps its own match — a String scrutinee
  there is a classify bug it must error on); `emit_bottom_abort` /
  `emit_interrupt_check` now call `pending_exit_block()` (made
  raw-style); the STALE re-fold is `fold_stale` (2 sites survived the
  A2 split, not 3); `emit_qop_node`'s deliver-or-drop block is
  `emit_qop_error_disposal` (shared by the composite/string and
  value arms; the scalar arm keeps its simpler two-block form —
  factoring it would change the CLIF).
- **DONE** `resolve_static` calls `refresh_static_ftype` (now
  returning the ftype) instead of inlining it twice.
- **N/A** `for_each_node`'s 18 binary-op arms do NOT share a view
  type (each op is its own macro-generated struct), so or-patterns
  can't collapse them; a shared-payload NodeView variant would be a
  bigger restructuring than the 18 lines justify.
- **DONE** `StringInterpolate::update` uses `LPooled<String>`.
- **DONE** `Lambda::compile`'s captures renamed `def_scope`/`def_env`/
  `def_typ`/`def_argspec`/`def_spec`.
- **DONE** `FnType::suppress_throws` is the shared predicate;
  `Display` now honors `explicit_throws` like `PrettyDisplay` (its
  own `explicit_throws_always_shown` test documented exactly that
  intent — the unconditional-suppression cases are unreachable from
  parsed source, so round-trip is unaffected).
- `fusion/scaffold.rs`: fine as-is — the seven loop emitters share a
  visible skeleton, but the file's instruction-stability contract
  justifies the explicitness. Optional `LoopFrame` prologue/epilogue
  helper only if it provably preserves emission order. (Left as-is.)

## C. Questions / inconsistencies needing a ruling

1. **RULED + FIXED (Eric, 2026-07-20):** `scalar_to_payload_i64` now
   `sextend`s I8/I16/I32, matching `pack_value_to_u64` and the ABI
   doc's "the payload word IS the Value encoding" invariant (all
   payload widening flows through that one function; the other
   uextends in emit/ are boolean flag widenings).
   `scalar_to_payload_i64` vs `pack_value_to_u64` (`emit.rs`):
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
2. **FIXED (2026-07-20):** all four deleted — `Bind.scope` (and the
   dead AOT-relic `Bind::new` that was its only other holder),
   `Callee::Static.def` (NOT a keep-alive: `ctx.lambda_defs` owns
   every def `Value` for the ctx's lifetime, never evicted —
   `resolve_static` no longer takes the value), `Lambda.top_id`/
   `flags`, `Op::base_op`. graphix-compiler builds warning-free.
3. **RESOLVED (2026-07-20), and the question was sharper than the
   review knew** (Eric asked: what about DynCall sites in a kernel?).
   The reachability sweep (GXDBG_KERNEL_SLEEP probes, suite-wide +
   witnesses + the dyn-module corridor) established: fusion's descent
   never enters a sleep-initiating position and sleep initiates only
   at select arms, so `FusedKernel::sleep` AND `Kernel::sleep` were
   both unreachable — and neither touched the dyn slots' bound
   applies, whose interp twins (`CachedArgs::sleep` clears
   combineLatest slots; `GXLambda::sleep` sleeps the body) DO act on
   sleep. Worse, `Kernel` never overrode the no-op `Apply::delete`
   default, so slot applies never got `delete(ctx)` — a LIVE
   wake-interest leak via dynamic-module reloads (every swap deletes
   module kernels). Fixed: `DynCallSlot::{sleep, delete}` (apply +
   arg_refs, `fired` reset so a wake is an init view — the
   `CallSite` twins), `Kernel` implements `Apply::delete` and its
   `sleep` now KEEPS the arg slots (the arm-wake replay memory the
   old body wrongly cleared), clears replay words, and sleeps the
   slots; `FusedKernel::sleep` delegates. The sleep half is coherent-
   but-dormant until arm-region fusion lands; the probes stay as its
   verification instrument, and the generator's slept-arm template
   (30949359) is its witness generator.
4. **Outstanding review comments**: RESOLVED (print.rs half,
   2026-07-20). Eric rejected the codex CR as framed — each nested
   `fmt_pretty` re-measures from its own `start`, and the pretty
   impls break before recursing, so the span check effectively resets
   at every structural line break. The kernel of truth (the check
   ignored the line's starting column — the few mid-line recursions
   like `select `/`if ` could overrun by their prefix) is fixed: the
   fit check now adds the current column and drops the trailing
   newline from the span. Printer policy per Eric: perfection isn't
   possible (irreducible tokens beat any limit) — fix layouts
   case-by-case when they obviously look bad. `tval.rs:132` still has
   a resolved `XCR estokes` awaiting his delete/re-open pass.
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
   preserved verbatim behind explicit overrides).
   **RULED (Eric, 2026-07-20): `has_unbound` now walks Ref params**
   (option (a) below) — an inferred `Alias<'open>` binding is an open
   fact for the closedness tests. The remaining skips
   (`bind_as`/`unbind_tvars`/`unbind_open_tvars` — de-facto harmless;
   `would_cycle_seen` — deliberate) stay. Analysis: for the
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
   name, so a Ref-mediated cycle can't hang them). EVIDENCE
   (2026-07-20): flipping `has_unbound` to walk Ref params was
   invisible to compiler+graphix-tests (2015/0) and regress (210/0) —
   no live witness for either policy; the flip is behavior-neutral on
   every current test surface, so (a) is cheap if ruled correct.

## D. What was NOT deeply reviewed

`typ/contains.rs`, `setops.rs`, `normalize.rs`, `cast.rs` (older,
documented in CLAUDE.md; only outline-checked), `expr/` (parser has
its own round-trip suite), `node/{data,pattern,bind,module,array,
error,op,compiler,genn}.rs` (outline + targeted reads only),
`env.rs`, `ide.rs`, `node_shape.rs`, graphix-rt. A second session
could cover `contains.rs`/`setops.rs` — they are the remaining
soundness-bearing files without a recent read.
