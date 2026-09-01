# design/ — index

One line per document, grouped by state. Each doc's own header carries
its status; this index is the map. "Built" means the tree implements it
and the doc holds the rationale plus as-built records; "superseded" docs
are kept as history and say what replaced them. CLAUDE.md summarizes the
current rules; the design docs hold the *why* and the *how it got here*.

## Built and current

| doc | what |
|---|---|
| `final_jit_architecture.md` | `Expr → node graph → CLIF`; one IR, two evaluators |
| `distributed_jit.md` | GIR removed; `emit_clif`/`fuse` per node; the emit contracts |
| `representable_bottom.md` | bottom as a value; the taint channel |
| `dense_delivery.md` | `TagValue` every cycle; fired×bottom algebra; consumer caches gone |
| `organic_firing.md` | a node fires iff a consumed input fires |
| `wake_catchup.md` | select wake: tracked per-input fire bits (once-per-select consumption) + forced recompute from present values; edges deliver exactly once |
| `activation_state.md` | bottom-out rule; state multiplicity = activation multiplicity |
| `atomic_recursion.md` | evaluation is atomic within a cycle; containment outside the language |
| `recursive_activations.md` | activations are collection slots; no depth limit; `trait Collection` with the `'_` hole; P2b measurements |
| `collection_intrinsics.md` | MapQ/FoldQ as compiler nodes; inline CLIF loops |
| `kernel_instance_state.md` | per-instance/per-call-site kernel state, DynCall site identity |
| `unified_value_abi.md` | the (disc, payload) Value ABI across the JIT boundary |
| `env_independent_typerefs.md` | `TypeRef` resolution cells; name-compressed instance signatures |
| `type_operation_scaling.md` | COW/DAG walks + memos for every core type operation |
| `type_copy_discipline.md` | principle: justify every deep type copy |
| `tvar_constraints.md` | cell constraints are the only constraint store (phase C) |
| `nominal_abstract_types.md` | `type T = Abstract<rep>`; nominal identity; constructor/payload/pattern |
| `traits.md` | traits v1 (§11), core `Eq`/`Ord`/`Display` (§12), io traits (§13) |
| `module_system.md` | Rust-2018-style `use`; `self`/`super`/`package` roots |
| `list_native.md` | native List: slim 2-slot cons, `Type::List`, `[<1, 2>]` literals + list patterns, B3 fused ladder |
| `netidx_extraction.md` | the core is network-free; `sys::net` owns netidx |
| `graphix_fuzz.md` | the differential fuzzer: trace oracle, schedules, callable routes, twins, HDD |
| `typecheck_fuzzing.md` | the acceptance-plane (typemorph) lane |
| `queue_fn.md` | `queuefn` |
| `catch.md` | `catch(e) expr` installs a handler; not control flow |
| `lpool_audit_2026_07.md` | pooled-allocation audit; applied |

## Proposed, not built

| doc | what |
|---|---|
| `or_patterns.md` | or-patterns `p1 \| p2` — orthodox semantics; select-arm-only top level (building 2026-08-31) |
| `fusion_lowering_split.md` | split `try_fuse` into analysis + lowering (legibility) |
| `interp_lazy_bind_cost.md` | partial: what remains of the interp's per-activation cost (see header) |

## Superseded (history)

| doc | replaced by |
|---|---|
| `replay_frames.md` | `dense_delivery.md` (its `reset_replay` classification + frames survive) |
| `transient_recursion.md` | the 2026-08-13 retention ruling; `recursive_activations.md` |
| `sync_subset.md`, `sync_control.md`, `value_returning_loops.md` | `collection_intrinsics.md` |
| `impure_hof_fusion.md`, `composite_hof_fusion.md`, `clone_rebind_testing.md` | `collection_intrinsics.md` |
| `interfaces.md` | `traits.md` / `trait Collection` |
| `code_review_2026_07_19.md` | a point-in-time review record |

## Status snapshots

| doc | when |
|---|---|
| `status_2026-08-30.md` | whole-project assessment at the end of the recursion/traits/select arc |
