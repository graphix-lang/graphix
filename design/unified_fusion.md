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

3. **Fuse** (`fusion::fuse`): walk the typed Node graph, build KIR
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
NOT registered as `ValueBind`. They become internal `KirOp::Block`
lets of the surrounding kernel, not standalone publishable
candidates.

## Build phase

`fusion::builder::build(candidate, &mut ctx) -> Result<BuiltKernel>`
takes a candidate and produces a `BuiltKernel`:

```rust
pub struct BuiltKernel {
    pub fn_name: ArcStr,
    pub kernel: Arc<KirKernel>,
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
to produce a `KirExpr`, then wrapped in a `KirKernel`. Free-var
inputs are discovered from the body — Refs whose BindIds are not
defined inside the body's scope become kernel inputs.

The KIR → KirKernel + JIT path (`fusion::lowering::*` +
`kernel_ir` + `kir_jit` + `kir_interp`) is reused from the
historical prototype's KIR machinery unchanged. The work in the
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

When a parent kernel's emit encounters `KirOp::Call("foo", args)`,
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
`(KirKernel, WrappedKernel)` pair and implements `Update`. The
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
    └── lowering.rs  — per-ExprKind KIR emission (largest piece;
                       reused from the historical prototype's
                       proven KIR translation machinery)
```

`kernel_ir` / `kir_jit` / `kir_interp` / `kir_jit_helpers` are
sibling modules — the KIR → native and KIR → interp layers. They're
unchanged in shape from the prototype.

## What this replaces

Nothing. The historical prototype on the `graphix-cranelift` branch
exists for reference. The code in tree IS the production fusion
implementation.

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
