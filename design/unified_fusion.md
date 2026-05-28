# Fusion architecture

Last updated 2026-05-22.

This document describes graphix's fusion pipeline as it is. Earlier
versions of this file recorded a prototype-vs-legacy split; that
exploration has run its course and the result is what's described
here. The historical artifact lives on the `graphix-cranelift`
branch.

## Pipeline

graphix's top-level compiler driver is `graphix_compiler::compile`.
A `compile` call has three phases:

1. **Build** (`node::compiler::compile`): turn the input `Expr` into
   a compiled `Node` tree. Recursive descent: every `ExprKind`
   variant has a corresponding compile path that produces a `Node`
   (a `Box<dyn Update>`). The Node tree is the executable form.

2. **Typecheck**: `Update::typecheck` on the root walks the Node
   tree, populating `Expr.typ` cells (phase 1). Then a deferred-
   check fixpoint handles type-directed builtins (phase 2).

3. **Fuse** (`fusion::fuse`): walk the typed Node graph, build GIR
   kernels for fusable subgraphs, JIT-compile them, splice them
   into the Node tree in place. After this phase the returned Node
   tree contains kernel-backed sub-Nodes wherever fusion succeeded.

The function returns a single `Node<R, E>`. The runtime
(`graphix-rt`) stores it and calls `update()` on it. The runtime has
no fusion knowledge beyond passing compile flags through.

## Files-as-modules

`rt.load(file)` reads + parses a file producing `Arc<[Expr]>` (one
Expr per top-level statement), then wraps them in a synthetic
`ExprKind::Module { name: <basename>, value: Resolved { exprs, sig:
None, from_interface: false } }`. The compiler sees ONE Expr — the
file's module — and `compile()` returns ONE Node containing the
file's entire graph. Fusion runs over that whole graph in a single
pass; cross-binding dependencies are visible because they're all
inside the one Module Node.

This matches the language semantics: a graphix file IS a module.
The wrapping just makes that explicit at the Expr level.

### Inline `rt.compile`

`rt.compile(text)` is the REPL / embedded API. Each call to it
compiles one piece of text. The compiler's `compile()` takes a
single Expr, so the runtime calls it per top-level Expr produced by
the parse. Multiple statements within one `rt.compile` invocation
don't fuse across each other — that requires a Module boundary,
and `Do` blocks can't be used as the wrapper because they
introduce a lexical scope. Performance-sensitive code is expected
to be aggregated into a module file (or an explicit `mod {}`
declaration in the input text). For interactive use this tradeoff
is fine.

## NodeView

`NodeView` is an exhaustive typed view of the compiled Node graph,
one variant per concrete `Update` impl in the compiler. It is
defined in `lib.rs` and lives alongside the `Update` trait.

```rust
pub enum NodeView<'a, R: Rt, E: UserEvent> {
    Bind(&'a Bind<R, E>),
    Lambda(&'a Lambda),
    Block(&'a Block<R, E>),
    Module(&'a Module<R, E>),
    CallSite(&'a CallSite<R, E>),
    // ...one variant per concrete Update impl...
    Constant(&'a Constant),
    FusedKernel(&'a FusedKernel<R, E>),
}
```

Each `Update` impl is required to implement `fn view(&self) ->
NodeView<'_, R, E>` — no default impl. Adding a new node type
can't compile without choosing a variant; adding a new variant
forces every existing exhaustive match to be reviewed. This is the
project's "compiler tracks what humans would otherwise have to
remember" pattern, applied to the node graph itself.

NodeView is the analysis-layer abstraction over the
execution-optimized `Box<dyn Update>` graph. Fusion is the first
consumer; future analysis tools (linters, dead-code, escape
analysis, refactoring) use the same surface.

The node graph was originally structured as an owned enum (close
to what `NodeView` looks like), but LLVM optimizes vtable dispatch
better than enum match-on-discriminant for the runtime hot path —
hence `Box<dyn Update>` for execution. `NodeView<'a>` recovers the
enum-shaped representation as a borrowed view for analysis, with
no impact on the runtime.

## Walker

`fusion::walker::walk(&mut Node, ModPath)` recursively descends a
Node via `NodeView` dispatch, producing a flat list of
`KernelCandidate`s. The walker maintains:

- `scope: ModPath` — the current module scope. Updated on
  `NodeView::Module` and `NodeView::Block { module: true }` entry.
  Read from the Module/Block node's stored scope field.
- `module_top_level: bool` — true at file root + inside Module
  bodies; false inside Do blocks + Lambda bodies. Drives whether
  a Bind is a publishable ValueBind or an internal block-scoped
  let.

The walker steps inside Lambda bodies (today's legacy `for_each_child`
on the AST treated Lambda as a leaf). All scope and BindId
information is read straight from compiled Nodes — no
reconstruction.

## Candidate kinds

```rust
pub enum CandidateKind<'a, R: Rt, E: UserEvent> {
    LambdaBind {
        name: ArcStr,
        bind_id: BindId,
        bind: &'a Bind<R, E>,
        lambda: &'a Lambda,
    },
    AnonymousLambda {
        synth_name: ArcStr,
        lambda: &'a Lambda,
    },
    ValueBind {
        name: ArcStr,
        bind_id: BindId,
        bind: &'a Bind<R, E>,
    },
    Region { source_id: ExprId, body: &'a Expr },
}
```

| Candidate | Where it comes from | Build target |
|---|---|---|
| `LambdaBind` | `let f = \|args\| body` at any scope | Lambda kernel, on-demand monomorphization cache |
| `AnonymousLambda` | `\|args\| body` at HOF-arg / non-Bind position | Same as LambdaBind, synth name |
| `ValueBind` | `let x = <sync>` at module-top-level only | Value kernel publishing via `bind_id` |
| `Region` | Top-level sync expression that isn't a Bind | Region kernel replacing the node by ExprId |

Block-scoped lets (Binds inside `Do` blocks or Lambda bodies) are
NOT registered as `ValueBind`. They become internal `GirOp::Block`
lets of the surrounding kernel, not standalone publishable
candidates.

## Build phase

`fusion::builder::build(candidate, &mut ctx) -> Result<BuiltKernel>`
takes a candidate and produces a `BuiltKernel`:

```rust
pub struct BuiltKernel {
    pub fn_name: ArcStr,
    pub kernel: Arc<GirKernel>,
    pub source_id: ExprId,
    pub splice: SpliceTarget,
}

pub enum SpliceTarget {
    NodeReplaceById,    // Region candidates
    BindPublish { bind_id, top_id },  // ValueBind candidates
    LambdaCacheEntry { name },  // LambdaBind / AnonymousLambda candidates
}
```

The body Expr (`node.spec()`) is fed to `fusion::lowering::emit_expr`
to produce a `GirExpr`, then wrapped in a `GirKernel`. Free-var
inputs are discovered from the body — Refs whose BindIds are not
defined inside the body's scope become kernel inputs.

The GIR → GirKernel + JIT path (`fusion::lowering::*` +
`gir` + `gir_jit` + `gir_interp`) is reused from the
historical prototype's GIR machinery unchanged. The work in the
build phase is purely orchestration: pick the right inputs,
emit, wrap.

## On-demand monomorphization

LambdaBind / AnonymousLambda kernels are NOT built eagerly. They're
built lazily, per call site, keyed by the call-site-resolved type.
A library with 100 lambdas where the caller uses 3 only builds 3
kernels.

```rust
pub struct LambdaCacheEntry {
    pub bind_id: BindId,
    pub lambda: Arc<LambdaExpr>,
    pub monos: Mutex<BTreeMap<TypeKey, FusionLazyCache>>,
}
```

When a parent kernel's emit encounters `GirOp::Call("foo", args)`,
it derives a `TypeKey` from the resolved arg types, looks up
`monos[type_key]`, and on miss recursively builds the
monomorphization. This naturally drives the dependency closure —
only kernels reachable from a top-level Region or ValueBind ever
get built.

For lambdas whose value escapes (passed as fn-typed argument,
stored in a non-immediately-applied Bind), one default
monomorphization is pre-built from `spec_typ` so the runtime
DynCall path has something to dispatch into. The walker tracks
`dynamic_use: bool` per LambdaBind candidate.

## Splice phase

`fusion::splice(&mut Node, BuiltKernel)` replaces a sub-Node in the
tree with a `FusedKernel<R, E>` wrapper that holds the
`(GirKernel, WrappedKernel)` pair and implements `Update`. The
wrapper's `update()`:

1. Reads input values from feeder slots (cached BindId
   subscriptions).
2. Packs them into the JIT wrapper's u64 args layout.
3. Invokes the native code.
4. Unpacks the result.
5. Returns `Some(Value)` if any input changed; `None` otherwise.

The mutation is in-place in the Node tree being returned from
`compile()` — the runtime never sees the pre-fused state.

## Compile-time wiring

```rust
pub fn compile<R: Rt, E: UserEvent>(
    ctx: &mut ExecCtx<R, E>,
    flags: BitFlags<CFlag>,
    scope: &Scope,
    spec: Expr,
) -> Result<Node<R, E>> {
    let mut node = compiler::compile(ctx, flags, spec, scope, top_id)?;
    node.typecheck(ctx)?;
    while let Some(check) = ctx.deferred_checks.pop() {
        check(ctx)?;
    }
    fusion::fuse(&mut node, ctx)?;
    Ok(node)
}
```

The runtime side (`graphix-rt::gx`) becomes very simple:

```rust
// load():
let exprs = parse_file(...)?;
let wrapped = wrap_in_module(file_basename, exprs);  // synthetic Module
let node = graphix_compiler::compile(&mut self.ctx, self.flags, &scope, wrapped)?;
self.nodes.insert(node.spec().id, node);

// compile() (REPL / embedded):
// For each top-level Expr from the parse:
for expr in exprs {
    let node = graphix_compiler::compile(&mut self.ctx, self.flags, &scope, expr)?;
    self.nodes.insert(expr.id, node);
}
```

No fusion knowledge in the runtime. No iteration loops for
fusion-side bookkeeping. No splice plumbing.

## Module layout

```
graphix-compiler/src/
└── fusion/
    ├── mod.rs       — public API: `fuse(&mut Node, &mut ExecCtx)`,
    │                  re-exports from sub-modules
    ├── walker.rs    — Node traversal via NodeView, KernelCandidate
    ├── builder.rs   — candidate → BuiltKernel
    ├── splicer.rs   — BuiltKernel → in-place Node tree mutation,
    │                  FusedKernel<R, E> Update impl
    └── lowering.rs  — per-ExprKind GIR emission (largest piece;
                       reused from the historical prototype's
                       proven GIR translation machinery)
```

`gir` / `gir_jit` / `gir_interp` / `gir_jit_helpers` are
sibling modules — the GIR → native and GIR → interp layers. They're
unchanged in shape from the prototype.

## What this replaces

Nothing. The historical prototype on the `graphix-cranelift` branch
exists for reference. The code in tree IS the production fusion
implementation.

## Static call resolution

A compile-phase pass between typecheck and fusion that pre-binds
every `CallSite` whose function expression provably resolves to a
single known `LambdaDef`. Lives in
`graphix-compiler::static_resolve`.

### Resolvability

A CallSite is statically resolvable when one of:

- Its `fnode` is a `Ref(bind_id)`, AND `bind_id ∉
  ctx.unstable_bindings` (no `<-` Connect targeting it), AND
  `bind_id` was bound by a `Bind` whose value Node is a `Lambda`.
- Its `fnode` is a `Lambda` Node directly (rare: a lambda
  literal applied inline, `(|x| x+1)(42)`).

Anything else (function-typed parameter, struct field access
yielding a function, conditional function expression) stays
dynamic — `bind()` fires on the first runtime cycle as before.

### Pass mechanics

`resolve_static_calls(ctx, root)` runs two walks over the Node
tree:

1. **Collect** `BindId → LambdaDef Value` from every `Bind` whose
   `node.view()` is `NodeView::Lambda`. Walks Module / Block /
   Bind containers via NodeView.
2. **Resolve**. Walks the same tree mutably via `Any`-downcast on
   `&mut dyn Update` (Rust 1.86 trait upcasting). For each
   `CallSite` that matches the resolvability rule above, calls
   `cs.resolve_static(ctx, def, fv)`.

`CallSite::resolve_static` shares its body with the runtime
`bind()` path via a `setup_bind` helper — the only difference is
the event-cycle priming `bind()` runs after the shared work, which
the static path skips (there's no event at compile time). On the
first runtime cycle through a statically-resolved CallSite,
`update()` drives `bound = true` once to trigger the same
priming arm the dynamic path uses.

### Wins

- **Interpreter**: every dynamic call previously paid
  `fnode.update` + Value equality + downcast on every invocation,
  and a one-shot `bind()` on the first. Pre-resolving folds the
  first two into the standard update arm and runs `bind()` at
  compile time.
- **Fusion** (next iteration): the pre-bound Apply for a user
  lambda carries the body as a concrete `Node<R, E>`. Fusion's
  walker reaches the body through `ApplyView::GXLambda(g) ->
  g.body()` — no special LambdaBind candidate kind.

## Apply trait: ApplyView and GirEmitter

Symmetric to `NodeView`. `ApplyView` is a typed view of any
`&dyn Apply<R, E>`, letting the compiler reach into known Apply
shapes without `Any`-downcasting. `GirEmitter` is the opt-in
trait that lets a builtin's own crate own its GIR emission — the
compiler stops knowing builtin names.

### The types

```rust
pub enum ApplyView<'a, R: Rt, E: UserEvent> {
    /// A graphix-language lambda. Body is a walkable `Node<R, E>`.
    Lambda(&'a GXLambda<R, E>),
    /// A builtin that knows how to lower itself to GIR. The
    /// `GirEmitter` trait object carries the lowering logic; the
    /// builtin's runtime `Apply` impl and its `GirEmitter` impl
    /// typically live on the same struct.
    FusedBuiltin(&'a dyn GirEmitter<R, E>),
    /// Opaque builtin. Fusion treats it as a black-box and emits
    /// a runtime `DynCall` slot.
    BuiltIn,
}

pub enum ApplyViewMut<'a, R: Rt, E: UserEvent> {
    Lambda(&'a mut GXLambda<R, E>),
    FusedBuiltin(&'a mut dyn GirEmitter<R, E>),
    BuiltIn,
}

pub trait Apply<R, E>: Debug + Send + Sync + 'static {
    // ...existing methods unchanged...

    /// Typed view of this Apply for analysis-layer code (notably
    /// fusion). Default: `BuiltIn` — opaque. `GXLambda` overrides
    /// to `Lambda(self)`. Fusible builtins override to
    /// `FusedBuiltin(self)`.
    fn view(&self) -> ApplyView<'_, R, E> {
        ApplyView::BuiltIn
    }

    /// Mutable counterpart. Defaults symmetric.
    fn view_mut(&mut self) -> ApplyViewMut<'_, R, E> {
        ApplyViewMut::BuiltIn
    }
}

pub trait GirEmitter<R, E>: Send + Sync {
    /// Lower this call site to GIR. Returns `None` if the
    /// arguments don't match this emitter's expected shape (e.g.
    /// a HOF callback isn't an inline lambda) — fusion falls back
    /// to `DynCall`, never an error.
    fn emit_gir(
        &self,
        callsite: &CallSite<R, E>,
        /// Source-order call-site args, paired with their optional
        /// labels. The emitter inspects these to make shape
        /// decisions — e.g. "is the callback arg a `Lambda` Node?"
        /// requires the original Node, not just a Ref to it.
        args: &[(Option<ArcStr>, &Node<R, E>)],
        /// Signature-order arg Refs, parallel to the function's
        /// `FnType.args`. Always includes a slot per formal arg
        /// (defaults resolved). The emitter uses this when it
        /// just needs to forward an arg value into GIR.
        arg_refs: &[Node<R, E>],
        ctx: &mut FusionCtx,
    ) -> Option<GirExpr>;
}
```

### The default impl works for trait objects

The `BuiltIn` variant carries no reference, so the default
`fn view(&self) -> ApplyView<'_, R, E> { ApplyView::BuiltIn }`
compiles without a `where Self: Sized` bound and is therefore
object-safe. Every existing Apply impl across stdlib packages
silently inherits `BuiltIn` semantics — no per-package edits to
land the trait method. Packages override only when they have
something useful to expose (fusion intercepts).

This was the deciding reason for the no-ref `BuiltIn` variant
vs. the earlier `Other(&'a dyn Apply)` design — the latter
required touching every package.

### Dispatch

Fusion's walker reaches a resolved CallSite (`statically_resolved
= true`, `function = Some((_, apply))`) and asks:

```rust
match apply.view() {
    ApplyView::Lambda(g) => {
        // Inline the user-lambda body into the kernel being
        // built. Same GIR emission as any other sub-expression.
        ctx.emit(g.body())
    }
    ApplyView::FusedBuiltin(em) => {
        // Builtin handles its own emission. None falls back.
        em.emit_gir(callsite, ctx).or_else(|| emit_dyncall(...))
    }
    ApplyView::BuiltIn => emit_dyncall(...),
}
```

`BuiltInLambda` (the wrapper used for runtime typecheck/refs
plumbing on builtins) is invisible to fusion: its
`Apply::view()` delegates to `self.apply.view()`. The wrapped
inner's view is what fusion sees — `BuiltIn` for an opaque
builtin, `FusedBuiltin` for one that opted in.

### Why mutable view

A fused builtin may produce a kernel that subsumes deeply-nested
parts of the CallSite's argument subtrees — e.g. a HOF whose
callback contained its own fusable region. Splicing a
`FusedKernel` Node into the lambda body at runtime requires
`&mut Node<R, E>` access reaching down through `ApplyView`. The
mutable variant exists for this; without it, the walker would
need an `Any`-downcast escape hatch every time it wanted to
mutate inside a resolved call site.

### Why not a per-builtin ApplyView variant?

The original sketch was `ApplyView::ArrayMap(&_) | ApplyView::
ArrayFold(&_) | ...` — one variant per fusable stdlib function.
This is **rejected** for layering: `ApplyView` lives in
`graphix-compiler`, builtins live in dependent crates
(`graphix-package-array`, etc.). Rust doesn't extend enum
variants cross-crate, so the moment we added a variant for a
stdlib builtin we'd have to pull the package back into
`graphix-compiler` — undoing the package split.

The `GirEmitter` trait sidesteps this entirely. A user package
*outside* the stdlib can publish its own fusible builtin without
modifying `graphix-compiler`.

### Public FusionCtx surface

For `GirEmitter::emit_gir` to be useful from outside crates, the
fusion API must expose enough to:

1. Recursively emit child Nodes (so a HOF emitter can lower its
   callback body).
2. Inspect the current kernel's input slots (so the emitter can
   bind args to existing kernel params).
3. Push new inputs scoped to an inner emit (HOF callback's arg
   becomes a kernel input for the duration of the body emit).

```rust
impl FusionCtx {
    /// Recursively emit a child Node.
    pub fn emit(&self, node: &Node<R, E>) -> Option<GirExpr>;

    /// Look up kernel inputs by name.
    pub fn find_input(&self, name: &str) -> Option<&Input>;
    pub fn find_array(&self, name: &str) -> Option<&ArrayInput>;
    pub fn find_tuple(&self, name: &str) -> Option<&TupleInput>;
    pub fn find_struct(&self, name: &str) -> Option<&StructInput>;
    pub fn find_variant(&self, name: &str) -> Option<&VariantInput>;

    /// Scoped input push for HOF inlining.
    pub fn with_input<F, T>(&self, input: Input, f: F) -> T
    where
        F: FnOnce(&FusionCtx) -> T;
}
```

`GirExpr`, `GirOp`, `GirType` (already `pub` in
`graphix-compiler`) become part of the stable public API.

### Migration

Today's hardcoded HOF intercepts in `fusion::lowering`
(`emit_array_map`, `emit_array_filter`, `emit_array_fold`,
`emit_array_init`) move into the `graphix-package-array` crate
as `GirEmitter` impls on the package's Apply types. Per
migration:

1. Add a `GirEmitter` impl on the package's Apply type.
2. Override `Apply::view()` to return `ApplyView::FusedBuiltin(self)`
   (and `view_mut()` symmetrically).
3. Delete `emit_<name>` from `fusion::lowering`.
4. Delete the name-string dispatch entry from fusion's intercept
   table.

When all stdlib intercepts have moved, `fusion::lowering` knows
only graphix-native shapes (Constants, arithmetic, lets, refs,
select, etc.) — no per-builtin code.

### HOF fusion and the async-callback constraint

Higher-order builtins (`array::map`, `array::fold`, `array::filter`,
`list::*`, `map::*`, anything built on `MapQ` / `FoldQ`) have a
runtime structural requirement that drives how their fusion works:

**Each per-element invocation of the callback must have its own
independent state.** Concretely: in `array::map(a, |p| 1 +
subscribe(p)$)`, the `subscribe(p)` call carries netidx
subscription state — a connection, a subscriber id, a current
value. If the array has N elements with N different paths, we
need N independent subscribes. `MapQ::update` enforces this
today by synthesizing a fresh `Apply` Node per array slot via
`genn::apply` (see `MapQ::update`, the `while self.slots.len()
< a.len()` loop). Each slot's pred Apply has its own internal
state — including subscribe's subscription handle.

This per-slot state is non-negotiable. A naive "fuse the whole
callback into one kernel for all slots" approach is **wrong** —
it would collapse N subscriptions into 1, breaking the program.

#### Why we don't generate GIR at runtime

The obvious workaround — JIT-compile fresh GIR per slot when
`MapQ::update` creates a new one — adds significant runtime
overhead: compilation latency on every array growth, cache
pressure from N copies of nearly-identical native code,
non-obvious memory ownership for the compiled artifacts. It's
also unnecessary, see below.

#### How HOF fusion actually works

A compiled kernel is just code. Its "inputs" are BindIds it
reads from `event.variables` / `ctx.cached`. **Different
runtime instances can share the same compiled code while
pointing at different BindIds.** Today's `FusedKernel` Node
already supports this — `Arc<WrappedKernel>` is the compiled
code (shared), the per-instance feeder list is what binds it
to specific BindIds. The slot-level customization is in the
wrapper, not the kernel.

So for `array::map(a, |p| 1 + complex_pure(subscribe(p)$))`
where `complex_pure` is expensive sync work we want to fuse:

**Compile-time output** (the *body fusion plan* — produced by
fusion when MapQ's emit_gir runs over the callback body, IF
the body has at least one fusable sync sub-region):

- For each async sub-expression in the body (here: `subscribe(p)`),
  a "runtime feeder recipe" — how to instantiate the Async
  Apply, plus a synthetic BindId name the kernel reads from.
- For the sync sub-region (here: `1 + complex_pure(<sub_output>)`):
  a single compiled `Arc<WrappedKernel>` whose input slots
  correspond to the async sub-expressions' synth BindIds.

**Runtime, when `MapQ::update` adds a new slot:**

- Allocate a fresh BindId for each async sub-expression's
  output (e.g. `slot_i_sub_out`).
- Construct the async Apply Node (subscribe) writing to that
  BindId. **Each slot has its own — exactly preserving today's
  per-slot state semantics.**
- Construct a `FusedKernel` wrapper Node holding a clone of
  the shared `Arc<WrappedKernel>` + a feeder list pointing at
  this slot's async BindIds.
- The wrapper updates when any of its async feeders fires;
  invokes the shared compiled code; emits the result.

For N elements: 1 compiled kernel (shared via Arc), N async
Apply Nodes (each with its own state — same as today), N
FusedKernel wrappers (lightweight — an Arc clone + a small
feeder list per wrapper). **No runtime GIR generation. No loss
of per-slot async state.**

This is conceptually the same trick maximal-fusion uses for
outer-binding Refs: an Async sub-expression becomes a "lifted
input" of a sync kernel, fed by a separately-compiled Node.
The HOF case extends this: lifted inputs are filled **dynamically
per slot** instead of from a single statically-known Bind.

#### Scope for the initial migration

The body-fusion-plan + per-slot runtime instantiation is a
**substantial extension** to MapQ's runtime structure (new
slot shape: `{async_applies: Vec<...>, kernel_wrapper:
FusedKernel}` instead of today's `{id, pred: Apply, cur}`) and
to fusion's emit output (a "plan" type that survives until
runtime, not just a `GirExpr`).

For the initial migration (tasks #120–#119), HOF fusion is
restricted to the **fully sync callback** case. If any part of
the body is Async, `MapQ::emit_gir` returns `None`, fusion
falls back to today's DynCall HOF dispatch — same as today's
behavior. The all-sync case is the simple lambda body
`|x| x * 2 + offset` pattern that drives most stdlib HOF uses;
covering it gets the per-builtin emit_gir architecture in
place.

Partial fusion of mixed-effect callbacks is a follow-up,
explicitly out of scope for the initial migration. **When we do
implement it**, the per-slot kernel-wrapper + async-feeder
architecture above is the model — no runtime GIR generation,
no shared async state.

### Open: shared emitter helpers

Today's intercepts share helpers like `array_param_name` (resolves
the array arg Expr down to a kernel-input name + element type).
Once intercepts move per-package, each needs the helper. Options:

- Promote helpers to `FusionCtx` methods (broad public surface).
- Publish a `graphix-fusion-helpers` utility crate.
- Inline per-builtin (duplication, but each is small).

Resolves when the second or third package migrates and we see
the actual overlap.

### Open: GirOp stability

`GirOp` becomes part of the stable public API once builtins
construct variants directly. Adding a variant is non-breaking;
renaming or changing field shapes is breaking. The stability
story (when do we stop changing GirOp freely?) is open — for now
builtin authors track compiler version.

## Open design questions

### Closure capture as kernel inputs

When fusing a lambda body, free-var Refs to outside-the-body
BindIds become kernel inputs. The kernel ABI carries args + captures
uniformly (just more slots). `Lambda::compile`'s InitFn arranges
for each captured slot to be fed by a `Ref` feeder against the
bound BindId.

For lambdas with no captures (most stdlib lambdas, anything bound
at module top level reading only its args), this is a no-op.

### TypeKey formulation

`TypeKey` derives from the resolved call-site `FnType.args`
(concrete types post-TVar substitution). Exact form: printable
canonical fingerprint via the existing `Type`/`FnType` Display
impls, hashed to a stable key. Refinement on first cache miss
shouldn't affect correctness — different keys, different
monomorphizations.

### Anonymous lambdas inside HOF inlining

HOF inlining intercepts (`array::map` / `fold` / etc.) inline the
callback's body into the parent kernel. The parent doesn't emit a
`Call` to the callback. Under on-demand monomorphization the
callback's standalone kernel is never built — its `monos` map
stays empty. Zero wasted work.

If the same callback escapes (e.g., `let f = |x| ...; array::map(arr,
f)`), the walker marks `f` as `dynamic_use=true` and one default
monomorphization gets pre-built.
