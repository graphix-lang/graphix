# LPooled audit — 2026-07-31

Survey of temporary-collection allocations that should be pooled (or are
otherwise wasteful), triggered by the analysis.rs review finding that the
SCC pass was allocating fresh IntMaps/IntSets per run. Three parallel
sweeps (compiler node layer + analysis; fusion/typ/expr; rt + hot stdlib
packages), top tier hand-verified. Items marked ✓ were verified in the
source; unmarked items are sweep-reported and should be glanced at before
editing. Delete this file when the worklist is drained.

Exclusion rules applied: long-lived struct fields (amortized), foreign-API
ownership boundaries (cranelift `Signature`/symbol names, tokio/serde/std
`Command`), bounded `SmallVec`, cold startup/error/debug-gated paths.

## Fix-first: not pooling misses, outright bugs/waste

1. ✓ `expr/parser/mod.rs:76` — `RESERVED` is `pub const LazyLock<AHashSet<&str>>`.
   A `const` is inlined per use, so every `RESERVED.contains` (`:283` fname,
   `:307` typname — every identifier/type name parsed, including backtracked
   attempts) builds and drops the whole ~40-entry set. Neighbor `GRAPHIX_ESC`
   (`:67`) is correctly `static`. One-word fix: `const` → `static`.
2. ✓ `fusion/emit/jit.rs:737` — `callee_layouts: BTreeMap<usize, SiteLayout>`
   rebuilt by scanning ALL of `jit.by_kernel` and cloning every `SiteLayout`,
   once per callee body defined. Quadratic on the region compile path. Wants
   hoisting out of the define loop (or borrowing), not pooling.
3. ✓ `stdlib/graphix-package-sys/src/dir.rs:22` — `blocking_walkdir` returns
   `LPooled<Vec<DirEntry>>` taken on the `spawn_blocking` thread but drained/
   dropped on the async worker (`:85`). One-way drain from the blocking-pool
   threads' pools into the workers' — cross-thread handoff is the `GPooled`
   case.
4. ✓ `expr/serialize.rs:184,:198` — drains the already-`LPooled` result of
   `cell_constraint_pairs()` into a fresh plain `Vec` twice per `FnType`
   encoded, purely for the `<Vec<_> as Pack>` bound — which `Deref` already
   satisfies on the pooled value. Actively undoes existing pooling.
5. `stdlib/graphix-package-core/src/buffer.rs:19` — bytes→string via
   `String::from_utf8(b.into())`: materializes a Vec copy of the `Bytes`,
   then the String is copied again into `ArcStr`. `std::str::from_utf8(&b)`
   + `ArcStr::from` removes both copies, no pool needed. `:38` lossy twin:
   `into_owned()` allocates even on the all-valid `Cow::Borrowed` branch.

## Tier 1 — hot-path pooling misses (✓ verified)

- **`analysis.rs` second half** — per RUNTIME DISPATCH, not just compile:
  `analyze_bound_callee` runs from `CallSite::bind` (callsite.rs:1038) on
  every lazy lambda bind. The first half (`collect_static_graph`,
  `strongly_connected`) is now fully pooled; everything after is not:
  - `:225-231` `collect_resolved_sites`: `seen` IntSet, `sites` Vec (returned,
    both callers only borrow then drop), `stack` Vec, and `to_descend`
    allocated INSIDE the worklist loop — one Vec per stack pop.
  - `:263,:264,:270` `infer_effects`: `bodies`/`self_ids` IntMaps + the `eff`
    fixpoint IntMap, rebuilt per bind.
  - `:478` `mark_recursion`: `component_sizes` IntMap — the identical
    histogram in `strongly_connected` (`:157`) IS pooled; also recomputes
    what `strongly_connected` already computed (could be returned instead).
  - `:495` `self_edges` SmallVec rebuilt per instance by filtering ALL edges
    (O(instances×edges)); only `is_empty` + a `find_map` are consumed — a
    single pass with two scalars needs no collection at all.
  - `:593` `positional_arg_order`: Vec → `into_boxed_slice`; pooled-drain
    idiom applies (per tail site marked).
- ✓ `fusion/mod.rs:769-770` — `discover_lambda_calls`: `local_sites` IntMap +
  `enqueue` Vec allocated INSIDE the worklist loop, one pair per body
  scanned. Surrounding walk state `:764` `worklist`, `:766` `root_sites`,
  `:757` `callees` are per-attempt scratch too (`:759` `bodies` is a
  BTreeMap — likely not Poolable, flag only).
- ✓ `node/collection.rs:416,:421` (+ same shape `:911,:919` FoldSlot) —
  `vec![element]` / `vec![acc, element]` per NEW SLOT PER CYCLE in the
  MapQ/FoldQ grow loops. `genn::apply`/`apply_prototype` take
  `args: Vec<Node>` and consume via `into_iter` (genn.rs:69,82,110) —
  crate-internal signature, so `LPooled<Vec<_>>` or `SmallVec<[_; 2]>`.
- ✓ `env.rs:516` — `unbind_scope_subtree`: `ids: Vec<BindId>` per scope
  inside the loop; its five sibling collections in the same function are
  already `LPooled`. Smallest diff in the audit.
- ✓ `stdlib/graphix-package-sys/src/netstate.rs:300` — `last: IntMap<SubId,
  NEvent>` per batch in the subscription pump; the only unpooled collection
  in that pump (`out` beside it is `VBATCH.take()`). No await crossing.
- `stdlib/graphix-package-sys/src/io.rs:33,:68` — `vec![0u8; n]` per read
  builtin eval, sized to the REQUEST, moved into `Bytes` after truncate.
  `LPooled<Vec<u8>>` + `Bytes::copy_from_slice(&buf[..got])` recycles the
  big buffer and right-sizes the payload. Crosses `.await` (fine, flag).

## Tier 2 — per-parse

- `expr/parser/lambdaexp.rs:54,:65` — `apply_args` yields plain
  `Vec<(Option<ArcStr>, Expr)>` under `attempt()` in the postfix chain
  (allocates even on backtrack), feeding `Arc::from` at `arithexp.rs:120`.
  Needs `Post::Call`'s payload type changed (`arithexp.rs:62`). Same file
  already uses `LPooled<Vec<Arg>>` at `:81/:101`.
- `expr/parser/interpolateexp.rs:55` — `argvec` scratch plus up to THREE
  full deep clones at `:64,:73,:88`. The clones look independently
  removable; per string interpolation parsed.
- `expr/parser/typexp.rs:392` — empty `Vec` allocated just to make an empty
  `Arc<[_]>`; `Arc::from_iter([])`. Micro.
- `expr/parser/modexp.rs:72,:90,:97` — `Vec<SigItem>`/`Vec<ModPath>` →
  `Arc::from`; cold-ish (mod/sig headers only).

## Tier 3 — per-kernel-emit / per-compile-walk (sweep-reported)

~30 sites in `fusion/emit/`, mostly small `Vec<ClifValue>` disc/arg buffers
filled then borrowed as slices (pooling is safe — cranelift only borrows):

- `emit/body.rs:206` — std SipHash `HashSet<&str>` per tail-rebind emit for
  a handful of `contains` checks; worst container choice found. Also
  `:596,:768` frame-stack snapshots, `:599` `tables` (verify not retained).
- `emit/call.rs:792,:793,:715,:91,:242,:574` — per call-site/DynCall scratch.
- `emit/select.rs:298,:310,:683-:766,:774-:775,:999` — guard-feeder dedup +
  pattern-classify scratch (`:199` already LPooled; rest of file didn't
  follow).
- `emit/nodes.rs:575,:819,:865,:942,:1021,:1308` — six disc buffers for
  `propagate_flags`; `:856` struct-literal sort scratch, `:904` fields
  cloned out of type deref.
- `emit/flow.rs:458-460` — three `with_capacity` per self-tail-call emit.
- `emit/scaffold.rs:188,:1288`; `emit/jit.rs:912,:924,:541,:679,:1103`;
  `emit/lower.rs:61`; `emit/abi.rs:459` (`JitEnv.locals` — per-emission
  scratch struct, pooling recycles grown capacity).
- `fusion/lowering.rs:1222,:1230,:1173,:1176` (build_lambda_kernel scratch),
  `:310-:320,:391` (builtin-call walk; `:320` layout → `Arc::from`),
  `:862` (`node_frame` deps — resolve_abstract frames are hot). Also
  `:448` `arg_types.clone()` — full Vec clone where both destinations are
  long-lived; the clone itself is the waste.
- `emit/lower.rs:789` — `declare_helpers` rebuilds a `BTreeMap<&str,
  FuncRef>` over the whole helper registry per compiled function; a dense
  array indexed by helper enum kills the map and the string compares.
- `fusion/mod.rs:383,:384` — `collect_lifted_connect_targets`:
  AHashMap/AHashSet keyed by `BindId` — double miss, `BindId` is
  nohash-keyed elsewhere (`lib.rs:490`); also `:306,:305,:1136`.
- `typ/tvar.rs:429,:516,:601` — `to_add` conjunct-dedup Vec, three identical
  blocks (alias/freeze/copy), fires per unification cell-merge.
- `fusion/kernel.rs:259` — `positional_refs` drain-collect scratch.

## Tier 4 — warm / low value

- `expr/serialize.rs:269` (Vec→Arc in unpack_module), `:317`, `:132,:142`.
- `node/module.rs:369` — `nodes` collect → `Box::from` at `:401`; warm, not
  cold: `compile_inner` re-runs on dynamic-module source fires.
- `node/callsite.rs:1253,:2273,:2298` — typecheck1/emit scratch.
- `stdlib/graphix-package-str/src/lib.rs:731,:749` — `to_lowercase`/
  `to_uppercase` scratch String → ArcStr (file has the pooled-buffer idiom
  at `:299,:361`); `:203` replace (needs hand-rolled loop, more code).
- `stdlib/graphix-package-sys/src/watch.rs:145` — `arcstr::format!` per
  watched path; MEASURE first (may already build in place).
- `stdlib/graphix-package-sys/src/tcp.rs:207,:241,:269` — addr `to_string`.
- `graphix-rt/src/lib.rs:987` — `set_many`'s `SmallVec<[_;4]>` spills to a
  caller-thread Vec freed on the runtime thread for >4 sets (fuzzer epochs
  do) → `GPooled<Vec<_>>` if it matters.
- `node/pattern.rs:354` — escapes into `CallbackParam.binds`; completeness
  only.

## Non-findings — do NOT "fix"

- `ValArray::from_iter` call sites (str/lib.rs etc.): netidx's FromIterator
  already collects through an internal `LPooled` + `from_iter_exact`
  (netidx-value/src/array.rs:414).
- `collect::<Result<Box<[_]>>>` straight into node fields: the allocation is
  transferred into the long-lived Box, pooling buys nothing.
- Cranelift-owned `Signature { params, returns }`, symbol-name strings,
  `intern_layout(Vec)` — foreign-API ownership.
- Already clean: the whole runtime cycle path (rt.rs, gx.rs do_cycle/batch
  fan-out — zero per-cycle misses), array/map packages, `typ/` except
  tvar.rs, node/lambda.rs tail-dispatch frames, `Refs` (LPooled inside),
  ide.rs (GPooled), fusion/kernel_abi.rs freeze walk (exemplary),
  fusion/builder.rs, intern.rs, emit/scalar.rs.
