# Design documents

Design as built. Every document here describes a rule the tree
implements today and why; when a design changes, its document is
rewritten in place. Proposals that were withdrawn and records of past
arcs are in `git log`, not here. Each document opens with `Status:`
(built date or current principle), `Pins:` (the tests and findings that
hold it) and, where it absorbed an older document, `Supersedes:`.

## Firing and delivery

| doc | rule |
|---|---|
| `representable_bottom.md` | bottom is a value with a taint channel; the bottom-scrutinee and consulted-guard rules |
| `dense_delivery.md` | `TagValue` every cycle; the fired×bottom algebra; store and overlays; frames |
| `organic_firing.md` | a node fires iff a consumed input fires; the numbered deltas |
| `wake_catchup.md` | sleep is pause; a reselected arm recomputes from the present and re-raises only unseen fires, once |
| `activation_state.md` | held state never decides bottomness; state multiplicity = activation multiplicity |
| `async_sleep_outputs.md` | async builtins clear their output on sleep |

## Recursion and collections

| doc | rule |
|---|---|
| `atomic_recursion.md` | evaluation is atomic within a cycle; containment is the cooperative interrupt |
| `recursive_activations.md` | activations are collection slots; shrink = delete; no depth limit; `trait Collection` |
| `kernel_instance_state.md` | per-instance / per-call-site / per-activation kernel words for firing exactness |
| `collection_intrinsics.md` | MapQ/FoldQ as compiler nodes; inline CLIF loops |
| `queue_fn.md` | `queuefn` |

## Fusion and the JIT

| doc | rule |
|---|---|
| `final_jit_architecture.md` | `Expr → node graph → CLIF`; one IR, two evaluators |
| `distributed_jit.md` | `emit_clif`/`fuse` per node; the emit contracts |
| `jit_startup.md` | startup profiles, early rejection, and block liveness costs |
| `strict_fusion.md` | fusion is pure computation + fast fns only; what was deleted and why |
| `unified_value_abi.md` | the (disc, payload) Value ABI across the JIT boundary |

## Types

| doc | rule |
|---|---|
| `tvar_constraints.md` | cell constraints are the only constraint store |
| `env_independent_typerefs.md` | `TypeRef` resolution cells; name-compressed instance signatures |
| `type_operation_scaling.md` | memoized DAG walks for every core type operation |
| `type_copy_discipline.md` | principle: justify every deep type copy |
| `nominal_abstract_types.md` | `type T = Abstract<rep>`; nominal identity |
| `traits.md` | traits v1, the core `Eq`/`Ord`/`Display`, the io traits |
| `list_native.md` | native List: `Type::List`, literals, patterns, the fused ladder |
| `or_patterns.md` | `p1 \| p2` in select arms and element positions |

## Language

| doc | rule |
|---|---|
| `module_system.md` | Rust-2018-style `use`; `self`/`super`/`package` roots |
| `catch.md` | `catch(e) expr` installs a handler; not control flow |
| `place_references.md` | `&a[i]`, `&s.f`, `&t.0`, `&m{k}` as root + path |
| `seq_blocks.md` | `seq`/`seqq`: the pc machine, `until`, `do`, `try … with`, the completion rule, the error guards |

## Infrastructure

| doc | rule |
|---|---|
| `netidx_extraction.md` | the core is network-free; `sys::net` owns netidx |
| `graphix_fuzz.md` | the differential fuzzer: trace oracle, schedules, routes, twins, HDD, typemorph |
