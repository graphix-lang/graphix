# LPooled audit — 2026-07-31

Survey of temporary-collection allocations that should be pooled (or were
otherwise wasteful), triggered by the analysis.rs review finding that the
SCC pass was allocating fresh IntMaps/IntSets per run. Three parallel
sweeps (compiler node layer + analysis; fusion/typ/expr; rt + hot stdlib
packages), top tier hand-verified.

**Status: applied.** Fix-first + tier 1 landed in 54878efe, tier 2 in
bb26d7fb, tiers 3–4 in the following commit. What remains below is the
deliberately-skipped list (each with its reason) — delete this file when
those are either done or rejected.

## Applied (summary)

- **Fix-first:** parser `RESERVED` const→static (rebuilt the reserved-word
  set per identifier parsed); `emit/jit.rs` callee_layouts hoisted out of
  the define loop (was a full by_kernel rescan + SiteLayout clones per
  callee) and maintained incrementally; sys `dir.rs` walkdir results
  LPooled→GPooled (was a one-way drain from the blocking pool to the
  workers); `serialize.rs` stopped draining the already-pooled constraint
  pairs into a fresh Vec twice per FnType; core `buffer.rs` bytes→string
  copies removed.
- **Tier 1 (hot):** analysis.rs second half fully pooled (runs per runtime
  lambda bind), `strongly_connected` returns its sizes histogram instead
  of `mark_recursion` recomputing it, per-instance self-edge scan replaced
  with one pass over edges; `discover_lambda_calls` per-body scratch +
  `CalleeBody.sites`/returns pooled; `genn::apply`/`apply_prototype` take
  `SmallVec<[Node; 2]>` (the per-new-slot-per-cycle `vec![element]` is
  gone; ten callers updated); `env.rs` unbind ids; netstate subscription-
  pump coalescing map; sys `io.rs` read buffers.
- **Tier 2 (per-parse):** `apply_args`→LPooled (+`Post::Call`), the
  string-interpolation fold reduced to a three-case match (was rebuilding
  and discarding an intermediate StringInterpolate with a full argvec
  clone per part), modexp sig/sandbox lists, typexp empty-params Arc.
- **Tier 3 (fusion analysis + emit):** the `lifted` set is
  `LPooled<nohash::IntSet<BindId>>` end-to-end (was AHash + fresh);
  `collect_region_inputs`/`non_scalar_basename_collision` pooled;
  `walk_node_for_builtin_calls` scratch (positional SmallVec, labeled
  LPooled map, layout LPooled→Arc); `build_lambda_kernel`
  inputs/formal_kts/arg_ids/external pooled; `typ/tvar.rs`'s three
  `to_add` conjunct-dedup buffers pooled; `kernel.rs` positional_refs;
  emit-side scratch converted to SmallVec (disc buffers in nodes.rs,
  call.rs arg/taint/drop lists, select.rs guard dedup + leaves +
  classify + binds, flow.rs tail-rebind triple + `emit_tail_rebind_jump`
  signature, body.rs frame snapshots) or LPooled (jit.rs
  to_define/defined/funcids/needed/typed_args, lower.rs initial_vals,
  call.rs LambdaCallSlot list, nodes.rs struct-with fields); body.rs
  slot_names std-SipHash HashSet replaced with a linear scan.
- **Tier 4 (warm):** serialize unpack_module; `Module::compile_inner`
  nodes (pooled, drained into the retained `Box<[Node]>`); callsite
  typecheck1/emit scratch; tcp address strings via `format_compact!`;
  `set_many` batches are `GPooled` (were SmallVec spilling to a
  caller-thread Vec freed on the runtime thread).

## Remaining — skipped with reasons

- `fusion/emit/scaffold.rs:188` (`bind_leaves`) and `:1288`
  (`elem_leaves`): return values thread into emission structs
  (`FoldAcc::Composite.leaves`, tuple returns) — the type ripple exceeds
  the win (per-HOF-emit, small N).
- `fusion/emit/body.rs` `tables` (state-table entries): NOT a temp — it
  escapes into `SlotTableFrame.tables` on `ctx.slot_tables`. Confirmed
  retained; the sweep's doubt was right.
- `fusion/lowering.rs` `node_frame` deps: lands in `FrameResult`/
  `MemoEntry` (the memo twin is retained) — field-type split needed.
- `fusion/lowering.rs` `arg_types` full clone into `FnParam` +
  `BuiltinCallSiteInfo` (both long-lived): sharing needs an `Arc<[Type]>`
  field change — worth doing, separately.
- `fusion/emit/lower.rs:789` `declare_helpers` rebuilds a
  `BTreeMap<&str, FuncRef>` over the helper registry per compiled
  function: the fix is a dense array indexed by a helper enum — a
  refactor, not a pooling change.
- `fusion/emit/jit.rs` `callee_refs` + discovery `bodies`: `BTreeMap`,
  not Poolable; ordered iteration is load-bearing (#19 determinism).
- `str::to_lowercase`/`to_uppercase` (str/lib.rs:731/:749): a pooled
  `chars().flat_map(char::to_lowercase)` is NOT semantics-preserving —
  `str::to_lowercase` implements the context-sensitive Greek final-sigma
  mapping (Σ→ς word-finally) that char-by-char mapping gets wrong.
  Keeping the owned String; to_upper kept symmetric.
- `str::replace` (str/lib.rs:203): needs a hand-rolled match loop into a
  pooled buffer; std only yields owned.
- `sys/watch.rs:145` `arcstr::format!` per watched path: measure first —
  it may already build in place.
- `expr/serialize.rs:132/:142` TVar constraints `to_vec`: the `Pack`
  impl is on `Vec`; encoding via slice needs a netidx-side change.
- `expr/serialize.rs:317` unpack_index, `env.rs lookup_matching*`
  (IDE-only), `node/pattern.rs:354` (escapes into `CallbackParam.binds`):
  cold or retained.

## Non-findings — do NOT "fix"

- `ValArray::from_iter` call sites (str/lib.rs etc.): netidx's
  FromIterator already collects through an internal `LPooled` +
  `from_iter_exact` (netidx-value/src/array.rs:414).
- `collect::<Result<Box<[_]>>>` straight into node fields: the allocation
  is transferred into the long-lived Box, pooling buys nothing.
- Cranelift-owned `Signature { params, returns }`, symbol-name strings,
  `intern_layout(Vec)` — foreign-API ownership.
- Already clean before the audit: the whole runtime cycle path (rt.rs,
  gx.rs do_cycle/batch fan-out — zero per-cycle misses), array/map
  packages, `typ/` except tvar.rs, node/lambda.rs tail-dispatch frames,
  `Refs` (LPooled inside), ide.rs (GPooled), fusion/kernel_abi.rs freeze
  walk, fusion/builder.rs, intern.rs, emit/scalar.rs.
