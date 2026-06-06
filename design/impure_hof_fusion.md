# Impure HOF fusion via partial-body splitting (plan)

Fuse the **sync sub-computation of an impure HOF callback** — an
`array::map`/`fold`/`filter`/`init` (or any `MapQ`/`FoldQ`-based builtin,
incl. `map::*`, `list::*`, GUI/TUI widgets) whose callback does
significant sync work and then *produces into* or *consumes from* an
async/side-effecting op (`net::publish`, `net::subscribe`, a `<-`
Connect, an RPC, I/O). Today these callbacks fuse **nothing** — the whole
body, including the expensive sync part, runs interpreted per array slot.

This is a **multi-phase milestone**, not a single-tick change — its core
(partial-body fusion) is a genuinely new compiler capability, verified
absent today (see §"Reusable vs net-new"). Land each phase green; the
discipline is the usual interp-first, then JIT.

The plan is the first one we've had for this problem that reuses the
existing kernel machinery instead of inventing a per-slot JIT loop or a
deep-clone-the-graph scheme — both of which were dead ends (see
§"Why not the obvious approaches").

## Why this matters (the use case)

This is not a corner case. The pattern — *big sync calc inside a HOF
callback, wrapped by an async op* — is how real reactive graphix code is
written. Concrete instances:

- **`stdlib/graphix-package-tui/src/graphix/browser.gx:93-111`** — the
  central `array::map(visible_rows, |row| { … })`. Each callback computes
  a display name (`str::basename` + select), branches on `mode`, and in
  the `` `Table `` branch runs a **nested** `array::map(visible_columns,
  |col| …)` whose body ends in `throttle(#rate, net::subscribe(path))`.
  Multi-statement, branching, ~19-line callback — and a live netidx
  subscription per cell.
- **`browser.gx:112-119`** — `content_widths <- throttle(…,
  array::fold(rows, [], |acc, row| { let row = array::map(row,
  str::len); … }))`. A pure sync width-accumulation fold whose result
  feeds a `<-` Connect through a throttle.
- **`book/src/examples/net/sum.gx:6-11`** (the textbook case) —
  ```graphix
  array::map(table.rows, |row| {
    let sum = array::fold(table.columns, v64:0, |acc, c|
      acc + net::subscribe("[row]/[c]")$);   // sync fold over async subs
    net::publish("[row]/sum", sum)            // async tail consuming the result
  })
  ```
  Sync prefix (`array::fold`) bracketed by async **upstream**
  (`net::subscribe` feeding the fold) and async **downstream**
  (`net::publish` consuming the sum).
- **`book/src/examples/gui/data_table_dashboard.gx:46-55`** —
  `array::init(1024, |i| svc(…))` where each element does nontrivial
  float math (`cpu = rn(0,30) + rn(1,5)*sin((t+rn(0,5))/rn(1,5))`) ×1024
  and `svc` fires **four** `net::publish` side effects. The sync math is
  exactly what we want fused, ×1024.
- **`data_table_scrolling.gx:33-48`**, **`data_table_calculated.gx`** —
  same shape (HOF over IDs/rows, sync compute, burst of publishes).

In every one the expensive part is sync and *fusable in principle*; only
the async wrapper forces the whole thing onto the interpreter.

## Empirically confirmed (the bail)

Running the code (not just reading it):

```graphix
{ let counter = 0;
  let a = [1, 2, 3];
  array::map(a, |x| { let y = x * 2 + 1;  // sync prefix
                      counter <- y;        // impure tail (Connect)
                      y }) }
```
`GRAPHIX_FUSION_DISCOVERY=1` reports `FUSEMAPF None`, `FUSEMAPJ NoJit`,
FUSEBAIL `dostmt:Connect,callqual:/array/map`. The byte-identical control
(Connect removed) fuses + JITs. So the sync `let y = x*2+1` prefix is NOT
carved out — the **first non-sync statement aborts lowering of the entire
body**.

Bail site: `fusion/lowering.rs` `emit_do_as_expr`, the catch-all
statement arm (~2008-2018). `let y = …` lowers (the `NodeView::Bind`
arm), but `counter <- y` is a `NodeView::Connect`, which isn't
`Bind`/`Nop`/`TypeDef`/`Use`, so it hits the catch-all, records
`dostmt:Connect`, and returns `None`. That `None` propagates through
`emit_hof_body_destructured` → `MapImpl::emit_gir` → the HOF call
lowering, which declines to fuse `array::map`. (The `net::publish`
variant additionally *times out* in the harness — the per-slot publish
array never settles — which is itself a reason to prefer the `<-` probe
shape for testing.)

## Why batch-loop can't do it (the semantic boundary)

For a **pure** callback we fuse the whole `array::map` into one batch-loop
kernel op (`GirOp::ArrayMap`) that iterates internally. That is invalid
for an impure callback on two independent counts:

1. **Side-effect cardinality.** `GirOp::ArrayMap` recomputes its entire
   output whenever *any* input changes. If `a[3]` changes, the loop
   re-runs for all N slots → `net::publish` fires N times instead of
   once. Wrong behavior, not just slow.
2. **It's mechanically impossible.** A sync kernel has no event loop,
   can't await, can't own per-slot subscription state. Which is exactly
   why `emit_gir` bails on async.

So impure callbacks **must** stay on `MapQ`'s per-slot reactive machinery
(one graph instance per element, diff-driven, O(1) per changed slot,
correct side-effect count). The question is never "make the HOF fuse"
— it's "fuse the sync sub-computation *inside* each per-slot callback
while keeping the per-slot machinery."

## The maximal-sync-subgraph-split reality

The mechanism that would do this — split a body into a maximal sync
sub-region (→ kernel) and the async edges around it — is **maximal sync
subgraph splitting**, the thing `RegionInputSource::Lifted` /
"M8.4 maximal fusion" was supposed to be. A code trace found it was
**never actually wired**: `RegionInputSource::Lifted` has zero
constructors in the live tree (only the enum variant, an unreachable
consumer at `lowering.rs:3609`, and doc comments survive);
`collect_region` / `collect_lifted_async` / `discover_region_inputs`
(named in the old notes) don't exist; every `RegionInput` producer uses
`RegionInputSource::Binding`. The live pipeline still uses the *initial*
model — a region is a fully-sync subtree, and an async sub-expression
**splits** the region rather than being lifted across it.

Worse, the documented design only ever described async **upstream** (an
async value lifted *into* a sync kernel as an input). The real use case
is **bidirectional**: in `{ let sum = fold(…subscribe…); publish(sum) }`
the sync kernel sits *between* an async input (`subscribe`, upstream) and
an async consumer (`publish`, downstream). A correct split must handle
both directions.

**So this milestone is "implement maximal sync subgraph splitting, for
real, bidirectionally, and apply it to lambda bodies."** The top-level
program case (what M8.4 originally targeted) and the per-slot callback
case share the same core split.

## The design

### 1. Fuse the concretely-typed CallSite, not the Lambda

Fusion needs **concrete monomorphized types** — you can't JIT `'a`, you
need `i64`. Those live on the **CallSite**, not the generic `LambdaDef`:
`Lambda::compile` stores the def's `typ` with unbound TVars
(`empty_tvar()` for unannotated params/return, `lambda.rs:471-482`); the
monomorphized form is produced at the call site by `resolve_tvars()` and
cached as `CallSite.resolved_ftype` (`callsite.rs:262-265`). This is the
same `NeedsCallSite` two-phase principle the typechecker already uses.

MapQ's **`analysis_pred`** is exactly the concretely-typed prototype: a
synthetic, never-executed CallSite built from the *resolved* FnType
(`mftyp` from `resolved.unwrap_or(typ)`) and statically resolved to the
callback's `GXLambda`. So the fusion entry point is a CallSite-phase
operation over `analysis_pred`:

```
fusion::fuse_callsite(concretely-typed CallSite) -> InitFn
```

It lives in **`fusion`, not `genn`** — `genn` depends only on
bind/callsite/Constant/Nop and cannot reach the runtime-node builders
(`FusedKernel::new` / `GirNode` in `fusion::builder` / `gir_interp`).
Putting it in `genn` would invert the dependency.

### 2. The split boundary IS the share/rebuild boundary

The key insight that makes this tractable: the **sync/async split
boundary coincides with the shareable/per-slot boundary.**

- The **pure sync sub-region** compiles to a kernel whose internals are
  JIT registers / interp env slots — it holds **no BindId-keyed reactive
  state**. So it is trivially **shareable** across all slots (Arc bump of
  `Arc<GirKernel>` / `Arc<WrappedKernel>`).
- Everything with **per-slot identity** — `net::subscribe` (upstream),
  `net::publish` (downstream), `<-` Connect, the element feed — is
  exactly the async/reactive residue. Its cross-cycle/runtime state is
  keyed by a **compile-time BindId** in the per-`ExecCtx` global tables
  (`Connect::update` → `ctx.set_var(self.id, …)`, `Ref::update` →
  `event.variables.get(&self.id)`, `publish`/RPC → `ctx.rt.ref_var(id)`).
  So it **must be rebuilt per slot** to get fresh BindIds.

Because these two boundaries coincide, splitting at the async barrier
separates "share this" from "rebuild this" with **nothing needing both**.
That sidesteps the two scariest implementation traps:

- **No `Node: Clone`.** (`Node = Box<dyn Update>`; there is no
  `clone_box`/`dyn_clone`; adding it would touch every node in the
  compiler + ~10 stdlib packages.)
- **No BindId-remap pass.** A verbatim clone of the per-slot graph would
  share the reactive BindIds and **silently stomp across slots**
  (last-writer-wins on `ctx.cached[id]` — a wrong answer, not a crash).
  We avoid it entirely: the only BindId-bearing part (the residue) is
  *constructed* fresh per slot (fresh ids for free), and the shared part
  (the kernel) has no BindId state.

### 3. The mechanism

**Compile time** — `fuse_callsite(analysis_pred)`:
- Run **partial** fusion on the callback body: carve the maximal sync
  sub-region → one shared kernel artifact (`Arc<GirKernel>` +
  `Arc<WrappedKernel>` JIT). Its inputs are the element, captures, and
  the **async-upstream** outputs; its output(s) feed the
  **async-downstream** consumers.
- Capture the async residue as a re-buildable recipe (the async Applies +
  their synth BindId wiring, both directions).
- Package as an `InitFn`, delivered via a `LambdaDef` (the per-slot
  `CallSite::setup_bind` resolves a `LambdaDef.init`, `callsite.rs:395`).

**Runtime, per slot** (`MapQ::update`'s `while slots.len() < a.len()`
loop, *unchanged in shape*):
- The slot's CallSite resolves to the `fuse_callsite` InitFn instead of
  building an interpreted `GXLambda`.
- The InitFn Arc-shares the kernel and builds a **fresh** `GirNode`
  wrapper (its per-instance `args`/`dyn_slots` are per-slot — share the
  kernel Arc, *not* the GirNode) plus a **fresh** async residue (fresh
  BindIds), wiring the slot's element feed.

For N elements: 1 shared compiled kernel, N async Apply nodes (each with
its own state — same as today), N lightweight GirNode wrappers. **No
runtime GIR generation. No loss of per-slot async state.** This is the
mechanism the old `unified_fusion.md` HOF section sketched (correctly) —
generalized to the bidirectional split and grounded in `fuse_callsite`.

### 4. The architectural payoff (mechanism, not policy)

Compare the two fusion paths by *compiler coupling*:

- **Batch-loop (`GirOp::ArrayMap`)** — fastest for pure callbacks, but
  each HOF needs a custom `GirOp` + interp arm + JIT arm + walker
  updates. Compiler-coupled; worth it only for the hottest few HOFs.
- **`fuse_callsite` per-slot shared dispatch** — works for **any**
  builtin's callback, pure *or* impure, with **zero** new compiler code
  per builtin. The builtin keeps calling `init`; we made `init` smarter.

So this is the *general* fused-builtin story, and the batch-loop becomes
the hand-optimization reserved for the hottest HOFs. A new reactive
container written next year fuses per-instance for free. The class this
unlocks is *every reactive container*: `map`/`filter`/`fold`/`group`/
`window`, dynamic dispatch/routing, and — the big one — the **GUI/TUI
widgets** (data-table rows, list items, chart series), each "N
structurally-identical reactive sub-computations created dynamically."
Today every one runs interpreted per row; with `fuse_callsite` each row
is a shared kernel + a thin per-row residue, and widget authors never
know fusion exists.

## Why not the obvious approaches

- **Re-run fusion per slot.** Runs the *optimizer* in the runtime hot
  path, N× identical analysis, and makes array growth trigger a compiler
  invocation — a latency spike at exactly the boundary where reactive
  code grows. It violates the compile-once/instantiate-many separation;
  it is the hack the whole effort exists to avoid.
- **Deep-clone the per-slot graph.** Needs `Node: Clone` (pervasive
  `clone_box` plumbing) *and* a BindId-remap pass — and a verbatim clone
  silently corrupts (shared reactive BindIds stomp across slots). The
  split makes both unnecessary.
- **One kernel for all slots (batch-loop).** Wrong side-effect
  cardinality + can't await (see §"Why batch-loop can't do it").

## Reusable vs net-new (verified against the code)

| Piece | Status | Notes |
|---|---|---|
| Concrete-type extraction (`resolved_ftype`/`resolve_tvars`) | **reuse** | `callsite.rs:262`, `lowering.rs:2302` |
| `(LambdaId, concrete FnType)` shared kernel cache | **reuse** | `ensure_lambda_kernel` + `ec.fusion_kernels`, `lowering.rs:2272-2447` |
| `build_kir_kernel_from_region` (build the pure kernel) | **reuse** | for the sync sub-region |
| `InitFn` shape / capture-over-Arc | **reuse** | but consumed via a `LambdaDef` field, not a bare closure |
| Per-slot `GirNode` wrapper sharing kernel Arc | **partial** | each slot needs its *own* GirNode (per-instance `args`/`dyn_slots`, `gir_interp.rs:2790`); share only the kernel Arc |
| Home in `genn` | **wrong** | belongs in `fusion` (dep inversion) |
| **Partial-body fusion (the split itself)** | **NET-NEW** | does not exist; see Phase 2 |

The split is the whole game. Everything that makes it a *split* —
effect-frontier body partitioning, an addressable async-residue
representation, per-slot residue rebuild wiring (bidirectional), and
removing the lambda-body async exclusion — is net-new.

## Phases

### Phase 0 — empirical baseline ✅ DONE (2026-06-06)
Confirmed the bail by running code (see §"Empirically confirmed"):
impure `array::map` → zero fusion, bail at `emit_do_as_expr` catch-all on
the first non-sync statement.

### Phase 1 — per-slot shared-kernel dispatch ✅ DONE (2026-06-06)
Make MapQ's per-slot loop dispatch a callback to a **shared compiled
kernel** instead of an interpreted `GXLambda`, when the callback body
lowers (no split yet). Landed green; **132 compiler + 1863 graphix-tests**.

What landed:
1. **`build_lambda_kernel`** (`lowering.rs`) — the FusionCtx-free core
   extracted from `ensure_lambda_kernel`: builds (or cache-hits) the
   `CachedKernel` and *returns* it. `ensure_lambda_kernel` is now a thin
   wrapper that registers the result into `FusionCtx`.
2. **`fuse_callsite(&CallSite, &mut ExecCtx) -> Option<FusedCallback>`**
   (`lowering.rs`) — from a concretely-typed, statically-resolved
   analysis CallSite, build `build_lambda_kernel` + JIT-compile
   (`compile_kernel_with_callees`, gated on `ec.jit_enabled`) and package
   a `FusedCallback`. Keys the kernel by a unique per-`LambdaId` name (no
   Ref fnode → the source-name SAFETY INVARIANT requires uniqueness; the
   per-slot path keys no `GirOp::Call` on it).
3. **`FusedCallback::build_slot(ctx, element_feeders, scope, top_id)`** —
   builds a fresh `builder::FusedKernel` Node per slot, sharing the kernel
   `Arc` (own per-instance `GirNode` state), fed by the element feeder(s)
   ++ shared capture feeders (`Ref`s to the captured bindings). The
   `FusedKernel` *owns its feeders* — which cleanly handles captures, the
   reason this won over the `InitFn`-via-`LambdaDef` shape (an `InitFn`
   returns a `Box<dyn Apply>` driven only with the CallSite's formal args,
   with no place for captures).
4. **`ExecCtx.jit_enabled`** — set by `compile()` from
   `!flags.contains(JitDisabled)`, so `fuse_callsite` (called from a
   builtin's `static_resolve_fn_args`, which doesn't get the flags)
   respects the 3-mode harness's fused (interp) mode.
5. **MapQ wiring** — `static_resolve_fn_args` calls `fuse_callsite` on
   `analysis_pred.pred`; `update()`'s per-slot loop builds each `Slot.pred`
   via `build_slot` when `fused_callback` is `Some`, else today's
   interpreted `genn::apply`.

**The "~zero value standalone" prediction was WRONG.** It assumed array
batch-loop competition: a sync array callback either batch-loops
(`GirOp::ArrayMap`, faster, so the per-slot loop never runs) or doesn't
lower (so per-slot can't either). But **non-array collection HOFs never
batch-loop** — `list::map`/`filter`/`find` over a recursive-variant
`List` go straight through MapQ's per-slot path, so per-slot `fuse_callsite`
dispatch is the *only* way they fuse. Result: `list_map`, `list_filter`,
`list_find_miss` flipped **None → Jit** (correct in all three modes — the
differential harness validated interp == fused == jit). Phase 1 is a real
win for the whole non-array-HOF cluster, not just plumbing.

**Follow-up (efficiency, not correctness):** `fuse_callsite` runs *eagerly*
in `static_resolve_fn_args` for every statically-resolved map callback —
including array maps that will batch-loop and never use the per-slot path,
wasting one kernel build + JIT compile each. Optimize by building the
`FusedCallback` *lazily* on first slot creation in `update()` (which only
runs when the map did NOT batch-loop). Bounded compile-time cost; deferred.

### Phase 2 — partial-body fusion (the milestone)

**Approach refined against the code (2026-06-06):** the split CANNOT live
inside `emit_do`/`emit_body` as the doc first guessed. `emit_do_as_expr`
(`lowering.rs:1967`) emits a `GirOp::Block` — a *kernel* `GirExpr` — and a
kernel expression has no way to *carry* an async residue (the residue is
reactive nodes, not kernel code). So the split lives one level up, at the
**`fuse_callsite` / body level** (a body-analysis + rewrite), and reuses
the Phase-1 machinery:

1. **Classify body statements sync vs async by `emit_node` success.**
   There is no live per-node effect map (`infer_effects`/
   `program_effect_map` are dead M8.4 references). But `emit_node`
   succeeding *is* "this is sync (lowers to a kernel)"; bailing is "async
   residue." Reuse it as the classifier.
2. **Build the kernel from the sync part** (the body with async statements
   removed) via the Phase-1 `build_lambda_kernel` — the kernel returns the
   *frontier* (the sync values the residue + the slot value need).
3. **Rebuild the residue per slot by RECOMPILING** (not cloning): per slot,
   build `FusedKernel(element, captures) → frontier bindings`, then build
   the async residue nodes (the `<-`/`publish`/…) wired to read those
   frontier bindings. Recompiling the thin residue gives fresh BindIds for
   free (no `Node: Clone`, no remap) and is strictly *less* per-slot work
   than today (MapQ already recompiles the whole body per slot; this moves
   the sync calc into the shared kernel).

**Minimal first cut:** the narrow shape `{ let v = <sync>; <async stmt
referencing v>; v }` (the probe / publish-tail / `<-` shapes) — where the
async statements are side-effects and the frontier is a single value.
Generalize to multi-value frontiers + async-upstream (`subscribe` feeding
the calc) after.

**Minimal first cut:** handle only the simplest split shape —
`{ <sync prefix>; <one async consumer of the result> }` (the `net/sum.gx`
/ publish-tail shape) — before general bidirectional effect-frontier
partitioning. That covers a large fraction of the real examples (the
publish-tail pattern) with far less machinery.

## Risks

- **R1 (sequencing):** without Phase 2 only pure callbacks fuse, but the
  use case is impure — Phase 1 alone is near-zero user value. Don't ship
  Phase 1 as "impure HOF fusion."
- **R2:** the fresh async residue rebuilt per slot has no analog (InitFn
  today returns a single `GXLambda`/`BuiltInLambda`) — fully new
  node-construction.
- **R3 (correctness):** per-instance `GirNode` state — share the **kernel
  Arc**, never the GirNode (`args`/`dyn_slots` are per-cycle per-instance,
  `gir_interp.rs:2790`). Wrong granularity = cross-slot corruption.
- **R4:** `ensure_lambda_kernel` silently bails (`None`) on String/Unit/
  Null returns (`lowering.rs:2415`) and dynamic-fn captures (`:2372`); a
  real impure callback may hit these — `fuse_callsite` inherits the
  cliffs.
- **R5:** home is `fusion`, not `genn` (dep inversion).

## Relation to the dead M8.4 / `Lifted` code

`RegionInputSource::Lifted` (`lowering.rs:3497`), its unreachable consumer
(`:3609`), and `FusionCtx::lifted_inputs` (`:106`, written only in the dead
branch) are **dead code** — the designed-but-never-wired maximal-fusion
lift. This milestone is where that lift finally gets built. Two options
to decide during Phase 2: (a) revive `RegionInputSource::Lifted` as the
async-upstream half of the bidirectional split, claiming the variant as
the starting point; or (b) delete it and build the split fresh under the
`fuse_callsite` framing. Either way, the doc claims (in
`whole_graph_fusion.md` Status, CLAUDE.md M8.4 entries) that the lift is
*landed* are wrong and are corrected alongside this doc.

## See also

- `design/unified_fusion.md` `### HOF fusion and the async-callback
  constraint` (L537-640) — the original sketch of this mechanism
  (async-upstream only, pre-`fuse_callsite`). Superseded by this doc.
- `design/whole_graph_fusion.md` `## Fusion model` / `## Effect
  classification` — the async-edge vocabulary; its `## Status` claim that
  maximal/lifted landed is corrected (it did not).
- `design/composite_hof_fusion.md` — the sibling cluster (composite
  *shape*; this one is *effect*/partial-body). Both feed the HOF emit
  path.
