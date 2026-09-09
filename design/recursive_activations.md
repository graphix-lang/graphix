# Recursive activations and the Collection trait

Status: built 2026-08-25 (shrink = delete ruled and built 2026-08-29;
carried-kind tail loops 2026-08-30)
Pins: `lang/functions.rs` (`tail_stateful_per_iteration`,
`fold_stateful_per_slot`, `tail_stateless_collapses`,
`tail_stateful_scalar`, `fn_invariant_tail_loop`,
`string_invariant_tail_loop`, `fn_formal_two_callbacks`,
`fn_formal_rebound`, `fn_formal_forwarded`, `cps_wrapper_recursion`,
`open_return_callee_in_callback`, `jit_deep_nontail_probe`,
`deep_nontail_recursion_completes`), `lang/attributes.rs`
`tail_recursive_stateful`, `lang/errors.rs` (`catch_per_activation`,
`catch_in_callee_stays_in_callee`, `catch_through_call`),
`lang/collection.rs`, `lang/select.rs`
(`shallow_ambiguous_same_tag_union`, `shallow_mixed_union_dispatch`),
`lib_tests/lift.rs` (`recursion_shrink_deletes_unreached_activations`,
`fused_recursion_sheds_unreached_blocks`),
`graphix-shell/tests/recursion_memory.rs`,
`graphix-shell/tests/check_runs_analyze.rs`,
`bench/collection/README.md`
Supersedes: transient_recursion, interp_lazy_bind_cost

Amends `activation_state.md` Ruling 2 (the tail-loop clause) and
dissolves the "not planned: higher-kinded self" deferral in
`traits.md` §5. Nothing here changes the bottom-out rule, organic
firing, or atomic recursion.

## 0. The goal

A `map` that does the same thing whatever it is mapping over — an
array, a list, a map, or a structure the user invented — and that a
user can IMPLEMENT for their own structure in the language. Intrinsics
for the fundamental structures are fair game in a language like this
(`collection_intrinsics.md`); the ugliness was that ONLY the compiler
could implement `map`, because the per-slot reactive semantics (one
live callback instance per collection position, identity across
resizes, per-slot sleep/wake) existed only inside it.

Why it is more than beauty: per-slot reactivity is the one thing
Graphix has that Haskell and Rust don't. `fmap` over a tree of IO
actions gives a tree of unrun actions; `map(tree, |p| net::subscribe(p))`
gives a tree of LIVE subscriptions whose identities track the
structure's positions across updates. That claim is only true if a
user's structure gets it too.

The observation that makes it cheap: recursion already has activations
(`activation_state.md` Ruling 2 — an activation per level for non-tail
recursion, retained across cycles, materialized lazily), and an
activation is exactly what a collection slot is. The gap was one
clause: a tail loop collapsed to ONE activation, so a tail-recursive
`map` over a linear structure had one publish site instead of n.

## 1. The ruling

> **A tail call creates an activation like any other call. A tail loop
> may reuse ONE activation only when its body is STATELESS — because
> then no program can tell the difference.**

Ruling 2 said a tail loop is one activation "FORCED, not chosen:
inlining semantics for general recursion plus constant-space tail loops
are jointly incompatible with per-depth history". The amendment keeps
both halves and puts the boundary where the history is: a stateless
body HAS no per-depth history, so constant space costs nothing; a
stateful body has O(n) history, and O(n) space is exactly what a slot
vector costs for the same n live things. Space is O(n) precisely when
there is O(n) state.

**Stateless**, of a lambda body (`analysis::infer_effects` computes
`LambdaFacts { effect, stateless }` in one fixpoint walk; the gate is
`lambda_is_stateless` = Sync ∧ stateless):

- the body is Sync (a cross-cycle node such as `~` or any async
  builtin makes it Async, so those are already out);
- every builtin it reaches is `Effect::Stateless` (`effects.rs` — a
  builtin's ONE classification; `Effect::Sync` names cross-invocation
  state or partial-delivery dependence);
- it contains no `<-` whose target is one of the body's own bindings
  (the connect identity law gives a target per-activation identity; a
  `ConnectDeref` is conservatively stateful);
- every callee it reaches, transitively, is stateless.

Stateless means cross-invocation STATE only — effects do not
distinguish one activation from many, so `dbg`, `log`, `error`, `now`,
`exit`, `hbs::render` and the like are stateless; `count`, `sum`,
`min`, `mean`, `uniq`, `once`, `take`, `skip`, `hold`,
`array::window`, the rand family and the http clients are not.
`#[tail_recursive]` asserts the gate (a stateful or async body fails
the assertion).

What follows for the three kinds of body:

| body | activations |
|---|---|
| stateless (arith, pattern, stateless builtins) | one, the framed loop |
| stateful Sync (`count`, `uniq`, `<-` ...) | one per iteration |
| Async | one per level, memory-bounded |

The two facts that forced the boundary, measured on both engines:
async recursion ALREADY allocated an activation per iteration (the
tail-loop gate required Sync, so an async body nested — `go(rest, acc +
(timer ~ x))` over 100 elements gave 4950, every level owning its
timer), and "a sync loop cannot observe the difference" was false:
`go([10, 20, 30], 0)` with `acc + count(x)` in the tail call returned
**6** under one activation and **3** under an activation per
iteration — which is what `array::fold([10, 20, 30], 0, |acc, x| acc +
count(x))` gives, because each FoldQ slot owns its `count`. The
per-iteration answer is the consistent one.

## 2. Mechanism — the interpreter needs no new driver

The non-tail dispatch path (`node/lambda.rs`, the body update under
`ensure_sufficient`) IS the per-iteration driver:

- **Activation = a retained instance per level.** Slot identity is
  depth, which is ordinal position in traversal order — the same rule
  MapQ applies to Map keys (a middle insert shifts later slots).
- **Re-feed** on the next cycle is the inner call site delivering the
  new tail to its already-bound instance.
- **Concurrency is right**: in `cons(f(v), map(tail, f))` the recursive
  argument does not depend on `f(v)`, so every level dispatches in the
  first cycle, as MapQ instantiates every slot at once. An accumulator
  threaded THROUGH an async value serializes, and FoldQ serializes
  identically (slot i's acc is slot i−1's output).
- **Stack** is heap-segmented under `ensure_sufficient`.
- The interpreter's tail loop reads the stateless gate instead of the
  sync gate; the frame machinery stays as the re-derivation discipline
  for a body with nothing to keep.

**Shrink = delete.** A depth not reached this cycle is deleted
immediately; re-reaching it is a fresh activation. This is MapQ's rule
for excess slots, adopted because a recursion activation IS a
collection slot: retaining an unreached depth asleep and RESUMING it on
re-descent would make a per-depth `count(e)` depend on the loop's depth
history — unpredictable, and inconsistent with MapQ, whose regrown
slots are fresh. Built as a scoped `ctx.deselecting_arm` flag, set only
while `Select::update` sleeps an arm it actively DESELECTS (a genuine
shrink, not a whole-recursion pause), under which a recursive-edge
`CallSite::sleep` deletes its callee (cascading) instead of retaining
it; cleared on crossing a callee body (`GXLambda::sleep`) so a
whole-recursion pause and external calls in the deselected arm retain.
Sleep is still pause for arms that PERSIST — an arm is a fixed position
(pause/resume), a recursion depth is a transient invocation
(delete/fresh). The JIT twin is `Kernel::update`'s reclaim of
per-activation `SelfBlock` subtrees not stamped with the current reach
generation (`kernel_instance_state.md`). If oscillation thrash from
immediate delete+realloc ever bites, the refinement is a reset-on-reuse
pool in BOTH systems — semantically identical.

Rejected: retaining shrunk activations (the initial call; revisited
for the semantics reason above). Rejected: an iterative slot-vector
driver (FoldQ generalized to successive tail-call arguments) — nesting
under stacker costs a few KB of segment per level against the tens of
KB of the retained instance, so a driver would save nothing that
matters; if per-activation footprint is the problem, the fix is
instance size.

## 3. Mechanism — the JIT

**A stateful tail loop is native recursion.** `emit_body_tail` jumps
only when `LowerCtx::tail.loop_head` exists; otherwise a tail-position
self-call is the ordinary native recursive call whose activation owns
its site blocks, so `f(n - 1, acc + max(n))` fuses and answers the same
on both engines (`tail_stateful_scalar`). A body that is stateless
loops by rebind-and-jump over one set of slots.

**Which lambdas loop is ONE shared predicate** (`structural_tail_loop`,
`fusion/lowering.rs`), so both engines agree: the body references its
own binding, every formal is positional with no varargs, every
LOOP-CARRIED formal is a kernel-encodable kind (scalars in registers,
composite pointers and two-word Values and Strings via the clone/drop
protocol — every `RegionInputKind`), and the body has a self tail call.
An INVARIANT formal — passed unchanged by every self-call
(`invariant_formals`) — is never rebound, so even an fn-typed one
loops: it drops out of the kernel signature (`KernelSig::skipped_args`,
the fn-capture precedent — its body uses are statically resolved calls)
and `emit_self_tail_call` rebinds by explicit slot index, skipping
invariant slots. Collapse is unobservable for a stateless body, so
what the lockstep protects is space parity, not semantics. Two
prerequisites fell out of the skip: the kernel cache key carries a
RESOLUTION FINGERPRINT (`FnResolutions` — per static lambda call site
the callee's LambdaId plus each forwarded fn-typed arg's resolution),
because a kernel bakes its instance's callback resolutions as CLIF
calls and two sites agreeing on every type can still resolve different
callbacks; and the premat wiring's synthetic Refs resolve by BindId
alone in the emitter (`JitEnv::lookup_id`). A collection-bodied lambda
refuses `build_lambda_kernel` explicitly — its sites inline the
scaffold.

## 4. Depth is bounded by memory, not a counter

There is no call-depth limit on either engine — no delivered bottom,
no trip poison, no whole-derivation rule. Eric's preference: limit
depth to available memory. The interpreter was already there once the
counter went; the kernel's non-tail self-calls run on the machine
stack, so it needed the JIT twin of `ensure_sufficient`:

- `graphix_stack_check` (0 interrupted / 1 call / 2 grow) at every
  SELF-call site — the same helper-returned-flag branch shape the
  counter had, so the fast path costs what the counter cost. On 2 the
  site spills its CLIF args to a stack slot at an 8-byte stride and
  calls `graphix_grow_stack(thunk, args, out)`, which runs the kernel's
  SPILL THUNK on a fresh 32MB segment (`stacker::grow`); the thunk
  (`jit::define_spill_thunk`, one per recursion-target kernel, declared
  before the body and defined after it) loads the params as the
  signature declares, calls the kernel, stores the two result words.
  Cross-kernel edges are acyclic (mutual recursion de-fuses), so only
  self-calls check. Measured: 2,000,000 deep in 1.4s and 507MB, ~250
  bytes of stack per level.
- The block-tree walks (`Kernel::drop`'s free, the reclaim) are
  explicit worklists — they recursed one Rust frame per activation,
  invisible under a 256 cap and a tokio-worker overflow at 20k.
- **The stack budget**: `GRAPHIX_STACK_BUDGET` (bytes) or
  `graphix_compiler::set_stack_budget`, unlimited by default; a
  thread-local counts live grown segments, and a grow that would
  exceed the budget aborts the runtime through `stack::budget_abort` —
  the ONE exit for both engines, setting `CtlFlag::Budget` beside
  `Abort` on the runtime's `Control` (the node-walk still gets that one
  segment so it unwinds at its next poll). `GXHandle::budget_aborted()`
  reads it and the fuzz runner maps a budget-aborted runtime to
  `Outcome::Timeout`: one outcome for a runaway whichever limit stops
  it, attributed to the subject's own runtime. Which containment fires
  first on an unbounded descent is a race between the engines' descent
  speeds (the JIT reaches 1GB in under a second, the node-walk in
  ~17s), not a property of the program. Fuzz children run under 1GB
  because a runaway kernel recursion otherwise grows stack at
  ~350MB/s until the subject deadline; `GRAPHIX_FUZZ_MEM_LIMIT` caps
  their address space.

Rejected: keeping the counter on the kernel while the interp lost it
(the engines would disagree above 256 on any deep sync non-tail
recursion). Rejected: the entry-only interim (`ensure_sufficient`
around `Kernel::update`, a fresh segment per invocation) — it bounds
depth at ~segment/frame, a large fixed number rather than memory, and
fails silently (a segfault) past it. Containment otherwise is
`atomic_recursion.md`'s: the interrupt, the budget, Ctrl-C.

## 5. The cost of an activation

The interp's constant is fine: ~8µs and ~15KB per activation in
release, and a flat 10k-slot async map runs in 0.4s. What was
superlinear was the DYNAMIC SCOPE: `Scope { lexical, dynamic }` were
both path strings, `Scope::append` extended both, and an activation's
body compiled under its call site's dynamic path — one retained string
of ~11 bytes × depth per level, Σ ≈ 2.2GB at 20k deep, 78% of cycles
in `is_canonical`. Its only consumer was the catch registry.

Eric's question settled the design: why extend the dynamic scope per
iteration at all? A handler install is the only event that must move
it.

- **`Scope::append` is lexical-only.** Blocks, select arms, handler
  bodies, lambda defs, impls and modules extend the lexical path and
  inherit the dynamic scope unchanged; a recursion whose body installs
  no handler shares its caller's dynamic scope across every activation.
- **`DynScope` is a parent-linked chain, one node per handler install**
  (`lib.rs`). `Catch::compile` covers the rest of its block with
  `scope.with_catch(..)`; `?` (compile) and a throwing call site
  (`typecheck0`) read `scope.dynamic.catch()` — the node IS the
  registry, so there is no `Env.catch` map and no longest-prefix walk.
  The lambda def gate compiles the body under a faux-catch CHILD of the
  def scope (collecting the body's `throws`) instead of overriding a
  registry key. A recursion whose body DOES install a handler adds one
  node per activation — the chain's legitimate length, O(1) per level
  ("catch in the body is something I could see being useful, so we have
  to handle it").
- Equivalence with the string registry: both lookups run at
  compile/typecheck time after every covering catch is installed, and a
  catch covers only scopes created after it, so reading the covering
  node sees exactly what the longest-prefix walk saw. `DynNode`'s drop
  is a loop down the parent chain (stack discipline).

| `--no-fusion`, release | 10k | 20k |
|---|---|---|
| deep `n + f(n-1)` | 1.19s / 568MB → 0.28s / 134MB | 4.10s / 2050MB → 0.52s / 236MB |
| async tail | 1.25s / 584MB → 0.35s / 151MB | 4.44s / 2083MB → 0.69s / 271MB |
| flat async map | unchanged (0.41s / 197MB) | unchanged (0.83s / 359MB) |

Nested cases are linear now (~25µs, ~10KB per activation above the
36MB base). `recursion_memory.rs` runs 20k interpreted activations in a
child and bounds peak RSS at 800MB.

## 6. The Collection trait

```graphix
trait Collection {
    val fold: fn(self<'a>, init: 'b, f: fn(acc: 'b, x: 'a) -> 'b throws 'e) -> 'b throws 'e;
    val filter_map: fn(self<'a>, f: fn(x: 'a) -> Option<'b> throws 'e) -> self<'b> throws 'e;
    val flat_map: fn(self<'a>, f: fn(x: 'a) -> self<'b> throws 'e) -> self<'b> throws 'e;
    val map: ..    = |c, f| filter_map(c, |x| f(x));
    val filter: .. = |c, f| filter_map(c, |x| select f(x) { true => x, false => null });
    val find: ..   = fold over an Option<'a> accumulator;
    val find_map: .. = fold over an Option<'b> accumulator;
    val len: fn(self<'a>) -> i64 = |c| fold(c, 0, |n, _| n + 1);
}
```

- **Required**: `fold` (traversal), `filter_map` (construction with
  selection) and `flat_map` (construction by concatenation — deriving
  it needs an identity element, which no self argument can witness).
  `map`/`filter` derive from `filter_map`; `find`/`find_map`/`len` from
  `fold`. Derived methods inherit the required method's slots, so a
  user type gets per-slot semantics from three hand-written recursions.
- **The blessed implementations are the intrinsics** (core's `mod.gx`:
  `impl Collection for Array<'_>`, `impl<'k> Collection for Map<'k,
  '_>`, `impl Collection for List<'_>`, marker bodies — `'array_fold`
  etc. — `collection_intrinsics.md`). They override every default,
  `len` included (O(1); `core_array_len`/`core_map_len` are core
  builtins the array and map packages bind to). The scaffold loops stay
  THE fast path: a list-accumulator body fused as a tail loop is a cons
  per element plus a reverse plus a copy, and the predictable-
  performance rule says that cannot be the default. What the general
  mechanism replaces is the intrinsics being the ONLY way.
- **A user's linear structure** writes `filter_map` as a tail chain
  over a list accumulator, front to back, finished by
  `list::to_array_rev` (one Rust walk) or its own constructor.
  Front-to-back matters: the suffix pattern `[init.., x]` would avoid
  the reverse but makes depth d element n−1−d, so every append shifts
  every slot. In-place `push` on a uniquely owned array does not work
  here: under per-iteration activations iteration i's accumulator is
  retained by i and passed to i+1, never unique.
- **Map is a functor over VALUES** under the last-parameter hole
  (`self<'a>` ≡ `Map<'k, 'a>`), so `Collection::map` on a Map maps
  values and `Collection::fold` folds values; `map::map`/`map::fold`
  are PAIR operations and stay as they are. Haskell draws the line in
  the same place (`Functor (Map k)`, `foldrWithKey` beside it).
- **Naming**: `Map` is the builtin type, so the trait is `Collection`
  (Eric's call).

Deletable at parity today (`bench/collection/README.md`): Array `map`
(two parity derivations: `filter_map` with a total callback, and via
`init`), Array `filter`, and Map's whole impl (already Graphix, at
parity through `fold_pairs`). Not yet: `fold` (the loop itself),
`find`/`find_map` (an Option-carrying fold can't early-exit, 2.4x),
`flat_map` (fold+concat is O(n²) by construction and doesn't fuse), and
List (per-element call overhead in a hand-written loop — `len`/`a[i]$`
calls plus the cross-kernel `f` call, ~50x — the cost any user impl
pays). Every intrinsic stays; the measurement decides per operation,
not principle.

## 7. Higher-kinded self: the hole

`self<'a>` is not a trait parameter; it is `self` as a type
CONSTRUCTOR. The design keeps traits v1's clean property — TYPING never
needs resolution; impl selection is a typecheck1 codegen decision:

- **A constructor is a type with a HOLE in its last parameter.** Two
  `Type` forms: `App(ctor, arg)` (`self<'a>`, `'c<i64>`) and `Hole`,
  spelled `'_` (Eric: it reads as the elided parameter, is explicit in
  a head, and round-trips when a bound constructor prints). `Type::app`
  fills when the constructor is concrete; a cell bound to an
  application whose constructor has since bound is READ as the filled
  type (`Type::app_filled`, applied by `with_deref`), so `is_a`,
  `cast`, select coverage, the typed printer and `kernel_abi` see
  `Array<'b>`, never an `App`. Only an OPEN constructor stays an
  application, treated like an open cell.
- **Decomposition is syntactic, on the receiver's outermost form only**
  (Haskell's rule; no kinds in unification): `decompose` splits
  `Array<e>` → `Array<'_>`, `e`; `Map<k, v>` → `Map<k, '_>`, `v`; a
  reference BY NAME with its last parameter; an abstract likewise;
  anything else (struct, tuple, union, primitive) is "not a type
  constructor". `fill_hole` is its inverse. In `contains`: `(App, Ref)`
  precedes the reference-expansion arm (a name is decomposed, never
  expanded); the general `(App, _)`/`(_, App)` arms sit at the END of
  the dispatch so ⊥, `Any` and an open cell keep theirs. The
  constructor variable binds BY NAME (`bind_ctor`) — through the
  general walk it met the expansion arm and bound to a list's union
  body. One recovery: a cell bound through `contains` holds a typedef's
  EXPANSION, which decomposes to nothing, so `app_split_for` unifies
  each registered head of the variable's trait bounds (filled with a
  fresh element) against the receiver, and the head that contains it is
  the constructor. `Hole` is a leaf equal only to itself and never
  bound; codegen match sites refuse it through their catch-alls, so an
  application that ever reached them would de-fuse, never miscompile.
- **Constructor traits**: `TraitDef.hole` — a trait applies `self` in
  every signature or in none (mixed is an error at `deftrait`). An impl
  head has exactly one hole, as the last parameter of its outermost
  form; a reference head (`List<'_>`) is owned by the package defining
  the name, a builtin constructor by the trait's package only.
  `find_impl` matches constructors (`Array<'_>` structurally,
  references by name); `resolve_trait_call` decomposes the resolved
  self type instead of expanding it; a receiver that does not decompose
  (a union of arrays) is refused. `'_` anywhere but an impl head is
  refused.
- **Generic code**: `'c: Collection` makes `'c` a constructor variable
  and the receiver is written `'c<i64>`; the sugar `|c: Collection|` is
  `'c: Collection, c: 'c<'e>` (`Type::trait_param`, one minting path).
  Per-callsite elaboration means the def-time body carries `App('c,
  'b)` as a form that normalizes when `'c` binds at each instance.
- **Rust-backed abstracts with parameters** decompose like any
  reference; a newtype delegates (`type AltMap<'k, 'v> = Abstract<Map<'k,
  'v>>`, `let map = |m, f| AltMap(map::map(m.0, |(k, v)| (k, f(v))))`).

### Rejected: v2 trait parameters

`trait WithErr<'e>` / `impl WithErr<`FsErr> for File` — parameters as
OUTPUTS of impl selection. Deferred because the client is thin: the one
named case is a custom error type, and union error types plus `?`
already cover most of what Rust needs `From` for (the residual is
encapsulation of a hidden error API). The hole cannot substitute — it
reads its bindings from the receiver's STRUCTURE, which is exactly why
it is resolution-free; an error type is a fact about the IMPL, which is
what "parameters as outputs of selection" means and why v2 pays the
resolution-order cost. Smuggling one into the receiver is a phantom
parameter, the pattern the io migration removed. Build it against the
first real module that wants it.

## 8. Typechecker rules the trait forced

All pre-existing, all pinned through the trait's defaults
(`lang/collection.rs`):

1. **A call site pre-unifies PARAMETER positions only**
   (`Type::pre_unify_arg` / `FnType::pre_unify_params`). Pushing
   declared parameter types into an unannotated callback is right; also
   pre-unifying the callback's still-open RETURN cell bound it to the
   whole declared return on first contact (`Option<'b2>` ⊇ open cell),
   and a generic `filter_map` wrapper was uncompilable. The return is
   judged after the body types.
2. **`FnType::constrain_known` follows alias chains**: a cell bound to
   a bare tvar is not a fact — recorded as one, every later occurs check
   read the fresh conjunct as a cycle.
3. **Wildcard narrowing skips union scrutinees** (`Select::typecheck0`):
   the walk that teaches `select n { 0 => .. }` its scrutinee is `i64`
   bound an open union member to an arm's type test (`select acc { null
   as _ => .., found => .. }` over `[e, null]` bound `e := null` and
   reported the second arm dead). A free union member stays free.
4. **The TVar×TVar fast path compares CELL identity** (`TVar::same_cell`),
   not tvar identity: two vars already aliased into one cell are
   unified, and falling through to the cycle guard poisoned both.

Open, deliberately: a named type variable in a lambda BODY annotation
(`let init: Option<'a> = null`) is a fresh cell, not the enclosing
signature's `'a`. Rust's rule (a body name means the definition's
variable) is the candidate and needs the instance-time name→type map.

## 9. Runtime type tests over recursive values

A select arm's inferred type predicate runs through `is_a`, and the
Variant arm recurses into payloads, so on a recursive ADT one consult
walked the entire remaining chain on native stack: a Graphix fold over
a 1000-element list aborted the process. Two rules:

- `Type::is_a_int` runs under `ensure_sufficient` (a runtime type test
  recurses through VALUE structure, so its depth is program-driven).
- **Shallow discriminators** (`Type::shallow_discriminant`, sealed
  lazily on the `PatternNode` at the select's first consult,
  `GXDBG_SHALLOW=1`): each arm's inferred predicate is sealed against
  the settled scrutinee type — members flattened, refs resolved
  seen-guarded, bound tvars deref'd — and a payload-carrying shape is
  replaced by its outermost form (payloads → `Any`) iff exactly ONE
  member overlaps its runtime footprint. Same tag+arity twins,
  tuple-vs-array, any-length arrays beside other array members, and
  explicit `x as T` predicates keep the deep walk (the user's claim
  stays strict). Consults drop from O(remaining) to O(arity); the list
  recursion curve went linear (release 49.4s → 0.41s at 8k). The kernel
  already tested tag+arity in O(1); this removes the engine asymmetry.

## 10. One compile channel

`compile_stmt` (the per-statement entry `--check` drives) carries the
`analysis::analyze` call itself, and `compile()` is a thin wrapper over
it — so `--check` rejects a false `#[tail_recursive]` with analysis's
own message and `GXDBG_EFFECT=1 --check` prints the effect facts. The
analyze call used to sit only at `compile()`'s tail, past where the
check path stopped (`check_runs_analyze.rs`).

## 11. The pressure tests

Three shapes, one per part of the mechanism; the first two are
fixtures in `lang/collection.rs`:

1. **Newtype delegation** (`collection_newtype_defaults`) — an abstract
   over an array, its three required methods over the intrinsics.
   Stresses only the trait: the hole, `use core::Collection::*`, the
   impl head form; the slots are the intrinsics'.
2. **A linear structure** (`collection_user_cons_list`) — a user cons
   list as a union typedef whose required methods are annotated
   module-level recursions. Stresses the amendment: this is the shape
   that used to hit the depth cap, and where the interpreter's
   per-activation footprint is measured.
3. **A tree** — the admin package's browser, a netidx path tree with a
   subscription per node: `type Tree<'a> = [`Node('a, Array<Tree<'a>>)]`,
   `map` is `` `Node(f(v), array::map(children, |c| map(c, f))) `` —
   recursion THROUGH a callback. Slot identity is tree position, the
   right identity for a browser (collapse a subtree: it sleeps; expand:
   it wakes). A rec callee inside a collection callback fuses
   (`fold_callback_name_collision`, `lang/functions.rs`).
