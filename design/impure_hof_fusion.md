# Impure HOF fusion via partial-body splitting (plan)

> **STATUS (2026-07-01): IMPLEMENTED.** The impure-HOF story is built and
> shipping: an impure callback splits at the async boundary — the sync part
> fuses + JITs per array slot, the async residue node-walks. **The mechanism
> that landed is the SUPERSEDING DESIGN immediately below** (fuse the
> callback template once, then `clone_rebind` it per slot), **not** the
> original splice / `SplitKernel` / `build_body_split` plan in the lower half
> of this file — that machinery was deleted and is kept only for archaeology.
>
> Lowering is now distributed as per-node `Update::emit_clif` + the
> `fusion/scaffold.rs` HOF loop scaffolds; the GIR vocabulary throughout the
> older plan text (`emit_gir`/`GirOp`/`GirNode`/`Arc<GirKernel>`/`GirExpr`)
> is **historical** — the GIR IR was deleted (F3, 2026-06-12). Read those
> names as archaeology, not as the current mechanism. Current architecture:
> `design/distributed_jit.md`.

---

## ★ SUPERSEDING DESIGN (Jun 2026): clone the fused template per slot

**LANDED 2026-06-15 — this superseding design is now the ONLY mechanism;
the splice machinery below is DELETED.** Gone: `FusedCallback.split` /
`SplitKernel` / `build_body_split` / `splice_into_body`, plus the ExprId
searcher they rode — `Update::splice_child` / `fusion::splice_into` /
`fusion::find_node_by_id`. The impure-HOF split now runs the ordinary
`fusion::jit_node` on `MapQ`'s cloned `analysis_pred` template body IN
PLACE (gated on `ctx.jit_enabled`), then `clone_rebind`s the fused template
per slot — "fuse a partly-async callback" became just "fuse the node tree,
then clone it", no MapQ-special split types and no by-ExprId splice. The
text below describes the deleted machinery and is kept for archaeology. See
CLAUDE.md (Major recent changes) and `design/distributed_jit.md` (F stage).

The original superseding note: the splice machinery below was MapQ-special
and leaked split-awareness into the runtime; the replacement is **fuse the
`analysis_pred` once via the ordinary fusion path, then `clone_rebind` that
fused template per array slot.**

### Core insight

The per-slot path is *already* a rebinding clone today — it re-compiles
the callback body from its `Expr` per slot, minting fresh `BindId`s (that's
how each slot gets independent async state). The ONLY reason we needed the
splice machinery is that recompile-from-`Expr` produces an **unfused** body
(fusion is a node-level transform). So: fuse the template once, and
`clone_rebind` it (preserving the fused structure) instead of recompiling.

A `BindId` is **not** a number on a node — it's a key into three registries
(`env.by_id`, the scope `name→id` map, the runtime `ref_var` table). So
re-minting = re-registering via `env.bind_variable`, which updates the
scope name map — and then refs resolve by name. **The env's scoped name map
IS the remap; no side `RemapTable`.** (Verified: `env.bind_variable`
mints/re-mints a fresh `BindId` and writes the name map; `lookup_bind`
reads it.)

### `clone_rebind` contract (the semantics)

`clone_rebind(&self, ctx, scope) -> Node` produces an independent copy
positioned as if compiled fresh in the same lexical spot:
- every binding the subtree *introduces* → fresh env-registered `BindId`;
- every `Ref` to an *internal* binding → resolves to the copy's fresh one;
- every *capture* (`Ref` to an external binding) → keeps the outer id;
- immutable shared artifacts (fused-kernel `Arc`s) → shared, not copied;
- stateful builtins → fresh independent state via their own `clone_rebind`.

The re-binding rides the env name map exactly as `compile` does (binds
register fresh names; refs resolve by name). It's the symmetric twin of
`delete` (which tears down those same three registrations).

### Per-node mechanics (all verified against the live tree)

- **Trait method** on `Update` (→ `Node`) and `Apply` (→ `Box<dyn Apply>`),
  default = panic so a missed node is loud. **[DONE, green — Step 1.]**
- **`Ref`** (`node/bind.rs:305`): get the name — from `spec.kind`
  (`ExprKind::Ref{name}`) for source refs, or `env.by_id[self.id].name`
  (→ `ModPath::from_iter([name])`) for synthesized (NOP-spec) feeder refs;
  `lookup_bind(scope.lexical, name)` → fresh id, else keep `self.id`
  (capture falls out for free); `ctx.rt.ref_var(new_id, top_id)`.
- **`StructPatternNode`** (`node/pattern.rs`): walk the enum; for each
  `Bind(old_id)` read `env.by_id[old_id]` → `bind_variable(scope.lexical,
  name, typ, pos, ori).id` → `Bind(new_id)`; recurse Slice/Struct/Variant
  sub-patterns + their `all`/`head`/`tail` ids. (`by_id` is `pub`.)
- **`Bind`** (`node/bind.rs:188`): `rec`-aware order, read from `spec.kind`
  — non-rec = **value `clone_rebind` then pattern re-mint** (RHS can't see
  the binding); rec = pattern first then value (mirrors `Bind::compile`
  lib.rs:84/101 vs 66/75).
- **`Block`/`Do`**: recurse children in lexical order (the name map is
  transient scratch, correct as long as binds precede uses — same as
  `compile`).
- **`GXLambda`** (reached via `ApplyView::Lambda`): re-mint arg patterns,
  then `clone_rebind` the body **structurally** (preserve the spliced
  `FusedKernel`s) — NOT re-init-from-spec (that would un-fuse). Keep the
  `LambdaId` (clones share the cached JIT kernel).
- **`CallSite`** (`node/callsite.rs`): Bind-shaped — re-mint the `args`-map
  ids, re-point `arg_refs` at them, `clone_rebind` `fnode` + the bound
  callee `Apply`. Precondition: the template CallSite must be **bound +
  fused** (so there's a fused body to recurse into).
- **`FusedKernel`/`GirNode`** (`fusion/builder.rs:231`, `gir_interp.rs:2782`):
  it's an ordinary Node — `clone_rebind` recurses its feeder deps + the
  `dyn_slot` callback `Apply`s, **shares** the `Arc<GirKernel>`/
  `Arc<WrappedKernel>`/`Arc<KernelRegistry>` (the kernel is incidental —
  the precompiled update logic), re-inits per-cycle scratch + re-runs
  `pre_init` for dyn_slots against the cloned feeders.
- **Builtins** (delegate 100%): `BuiltInLambda` → inner `apply.clone_rebind`;
  `defpackage!` macro default for pure builtins (clone arg nodes + fresh
  state); hand-write the stateful residue ones (`Connect`, then publish/
  subscribe/timer/count) — cloned subscribe un-subscribes, count resets,
  timer re-arms.

### Compile-time + runtime rewire

- **Compile-time** (`MapQ::static_resolve_fn_args`): after binding the
  `analysis_pred`, run `crate::fusion::fuse()` on the callback body
  (`resolved_apply_mut() → Lambda(g) → g.body_mut()`) — the walker splices
  the sync regions in place, leaving the async residue. Store the fused +
  bound `analysis_pred` as the template. Replaces `build_body_split`.
  (Verified: `fuse(&mut Node, ctx, flags)` operates on any subtree.)
- **Runtime** (`MapQ::update`, per slot): `genn::bind(scope, "x", …)` →
  fresh element id; `let pred = template.clone_rebind(ctx, scope)` (the
  cloned arg[0] ref + the FusedKernel's element feeder both resolve `"x"`
  → the fresh element via the name map); `Slot{ id: element_id, pred }`;
  feed the slot value to `element_id` as today. No `slot_spliced`, no
  `resolve_static` force-bind, no deferred drive-loop splice.

### Deletions once the clone path is sole + green

`FusedCallback`, `SplitKernel`, `build_body_split`, `splice_into_body`,
`jit_compile_split_kernel`, `build_slot`'s split handling, MapQ's
`fused_callback`/`slot_spliced` fields.

### Status — DONE + green (132 compiler + 1865 graphix-tests)

All landed: the `clone_rebind` trait method (with a RE-INIT-from-spec
default, not panic — residue nodes recompile; only the spine overrides
structurally) + spine impls (`Ref`, `StructPatternNode`, `Bind`, `Block`,
`GXLambda`, `BuiltInLambda`, `CallSite`, `Connect`, `FusedKernel`/`GirNode`
via `GirNode::clone_shared`). Compile-time: `static_resolve_fn_args` builds
+ resolves the `analysis_pred` but does NOT fuse it (that poisons the
surrounding region's JIT for region-fused HOFs). Runtime: `MapQ::update`
LAZILY fuses the template on the first slot-growing cycle (`template_fused`
flag), then `clone_rebind`s it per slot. The clone path runs for EVERY HOF
callback (pure region-fused, pure MapQ, impure) with no regression.

Removed: the old per-slot `build_slot` dispatch, force-bind/deferred-splice,
`slot_spliced`, `fused_callback`. RETAINED (now the lazy-fuse *mechanism*):
`FusedCallback`/`SplitKernel`/`build_body_split`/`splice_into_body`/
`build_slot`/`fuse_callsite`.

**Remaining follow-up (#157):** builtin call in a callback's async residue
(`|x| { calc; net::publish(p, x) }`) hits the `Apply` panic default —
current tests use `Connect` residue only. Fix: builtins own `clone_rebind`,
or `CallSite::clone_rebind` leaves a builtin function unbound to re-init
fresh per slot. See CLAUDE.md "Impure-HOF fusion rebuilt on clone_rebind".

---


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
mechanism the earlier HOF-fusion sketches described (correctly) —
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

#### Phase 2 — partial-body fusion ✅ DONE end-to-end (2026-06-06)

**The milestone is complete.** An impure HOF callback now splits at the
async boundary, fuses + JITs the sync sub-region(s), and runs them
per-slot — proven observable by `lang::fusion::impure_hof_callback_splits`:
`array::map([1,2,3], |x| { let v = x*2+1; counter <- v; v })` → `[3,5,7]`
**and** the sync calc `x*2+1` JIT-ran per slot (`jit_invocations > 0`).
132 compiler + 1864 graphix-tests green, no regression.

**Per-slot splice — two paths (force + deferred):**
- **Force-bind at slot construction (the observable path).** In
  `MapQ::update`'s slot-grow loop, after building the slot's `pred`
  (a fresh `genn::apply` `CallSite`), the split case force-resolves it
  *event-free* via `cs.resolve_static(ctx, def, fv)` — `def` is the
  callback `LambdaDef` extracted from `ctx.cached[predid]` (downcast,
  the same value `bind()` uses), `fv` its value. Then
  `fc.splice_into_body(ctx, g.body_mut(), scope, top_id)` splices each
  `SplitKernel`'s shared `Arc` (feeders from `collect_region_inputs` on
  the per-slot body, matched by name) into the body **before its first
  run**. So even a one-shot (static) array fuses on its only cycle.
  `slot_spliced[i] = true` once resolved, so the deferred path skips it.
  Any failure (not a `CallSite`, no `fv`, resolve `Err`) leaves
  `slot_spliced[i] = false` → the deferred path retries.
- **Deferred splice in the drive loop (the safety net).** After each
  `s.pred.update`, an un-spliced split slot whose `CallSite` has lazily
  bound (`resolved_apply_mut() == Lambda(g)`) gets the same
  `splice_into_body`. This covers any slot the force path missed; the
  pre-splice cycle runs interpreted but yields the same value (the
  kernel computes the same sync sub-region), so it's correctness-safe.

`MapQ` gained `slot_spliced: Vec<bool>` parallel to `slots`
(push `false`/`true` on grow, pop on shrink). The async residue
(`counter <-`, publish, …) stays interpreted in the per-slot body; only
the sync `let`-value subgraph is replaced by the spliced `FusedKernel`.

**Perf note:** force-resolve compiles the per-slot `GXLambda` + feeders
at construction (runtime). That cost already existed for per-slot graphs
(`genn::apply` builds one graph per element); the splice adds feeder
compilation. Net win for arrays that update repeatedly (JIT'd sync calc
each cycle); a possible setup-cost loss for very large one-shot arrays.
Correctness is unaffected either way.

**Compile-time half** (the split engine, landed first):
- **`build_body_split(body, ec)`** (`lowering.rs`): when the whole body
  can't fuse (it has async ops, so `build_lambda_kernel` returns `None`),
  walk the body `Block`'s children and, for each `let`-value, try
  `build_region` on it. **`build_region` succeeding *is* the sync/async
  classifier** — it bails on async ops / unrepresentable types, so a
  success is exactly "this `let`-value is a fusable sync sub-region."
  Each success → a shared `SplitKernel` (kernel `Arc` + JIT `WrappedKernel`
  + the region's free-var inputs + the `value_id` to splice at). Reuses
  `collect_region_inputs` + `walk_node_for_builtin_calls` + `build_region`
  + `compile_kernel_with_callees` — the same machinery `fuse()` uses for
  the program tree, pointed at the callback body.
- **`fuse_callsite`** now tries the whole-body kernel first (Phase 1);
  on failure, falls to `build_body_split`. `FusedCallback.kernel` became
  `Option` (None for the split path) + a `split: Vec<SplitKernel>` field.
- **`build_slot`** falls back to interp for the split case (the per-slot
  splice — above — replaces the body instead).

**Generality / follow-ups.** `build_body_split` carves *every* `let`-value
that builds a region, so a body with several sync `let`s before/around an
async op fuses each independently — not just the single-prefix shape. Not
yet covered: a sync sub-region that is the *tail* expression of the block
(only `let`-values are scanned, not the block tail); a sync region nested
inside an async op's argument (e.g. `publish(p, bigcalc(x))` with no
intervening `let`); and multi-level `select`/`try` bodies. Each is an
extension of the same `build_body_split` scan.

## Risks

- **R1 (sequencing): RESOLVED.** Phase 2 landed, so impure callbacks now
  fuse their sync sub-regions end-to-end (not just pure callbacks).
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
`fuse_callsite` framing. Either way, older claims that the lift is
*landed* are wrong and are corrected alongside this doc.

## See also

- `design/distributed_jit.md` — the current fusion architecture
  (per-node `Update::emit_clif` + `fusion/scaffold.rs` loop scaffolds) that
  this design landed on.
- `design/composite_hof_fusion.md` — the sibling cluster (composite
  *shape*; this one is *effect*/partial-body). Both feed the HOF emit
  path.
