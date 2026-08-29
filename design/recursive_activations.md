# Recursive activations and the collection traits

**Status: DESIGNED 2026-08-24 (Eric + Claude), not built.** Amends
`activation_state.md` Ruling 2 (the tail-loop clause), dissolves the
"not planned: higher-kinded self" deferral in `traits.md` §5, and
DEFERS v2 trait parameters (§8). Nothing here changes the bottom-out
rule, organic firing, or atomic recursion.

## 0. The goal

A `map` that does the same thing whatever it is mapping over — an
array, a list, a map, or a structure the user invented — and that a
user can IMPLEMENT for their own structure in the language. Today the
collection HOFs are compiler intrinsics special-cased on three types
(`node/collection.rs`, `collection_intrinsics.md`), and there is no
way to write one: the per-slot reactive semantics (one live callback
instance per collection position, identity across resizes, per-slot
sleep/wake) exist only inside the compiler. Eric's framing: intrinsics
for the fundamental structures are fair game in a language like this;
the ugliness is that ONLY the compiler can implement `map`. This doc
argues that we COULD retire the intrinsics, and deliberately does not
argue that we should (§6).

Why it is more than beauty: per-slot reactivity is the one thing
Graphix has that Haskell and Rust don't. `fmap` over a tree of IO
actions gives a tree of unrun actions; `map(tree, |p| net::subscribe(p))`
gives a tree of LIVE subscriptions whose identities track the
structure's positions across updates. That claim is only true if a
user's structure gets it too.

The observation that makes it cheap: recursion already has activations
(`activation_state.md` Ruling 2 — an activation per level for non-tail
recursion, retained across cycles, materialized lazily), and an
activation is exactly what a collection slot is. The gap is one
clause: a tail loop collapses to ONE activation, so a tail-recursive
`map` over a linear structure has one publish site instead of n.

## 1. Two facts, measured (2026-08-24, both engines)

1. **Async recursion already allocates an activation per iteration.**
   The tail-loop gate requires a Sync body — `analysis.rs:629`,
   `structural && lambda_is_sync(..)` — so an async tail-recursive
   body never loops today; it NESTS, one retained instance per level.
   `go(rest, acc + (timer ~ x))` over 100 elements gives 4950: every
   level owns its timer. The `publish_loop` example in §2 already does
   what its author intends. What it does not do is pass 256: the same
   chain over 300 elements trips `DEFAULT_MAX_CALL_DEPTH`
   (`lib.rs:131`) and bottoms.

2. **"A sync loop cannot observe the difference" is false.** `count`,
   `once`, `uniq`, `hold`, `take`, `skip` are Sync since P7 and
   stateful. `go([10, 20, 30], 0)` with `acc + count(x)` in the tail
   call returns **6** today (one activation; `count` sees all three
   deliveries) and **3** under an activation per iteration — which is
   what `array::fold([10, 20, 30], 0, |acc, x| acc + count(x))` gives,
   because each FoldQ slot owns its `count`. Today's 6 is the
   inconsistency.

So the proposed ruling is already the de facto semantics for async
bodies, the collapse criterion has to be STATELESS rather than SYNC,
and the missing pieces are a predicate, a deletion, and a stack bound.

## 2. Ruling 2, amended

> **A tail call creates an activation like any other call. A tail
> loop may reuse ONE activation only when its body is STATELESS —
> because then no program can tell the difference.**

Ruling 2 said a tail loop is one activation "FORCED, not chosen:
inlining semantics for general recursion plus constant-space tail
loops are jointly incompatible with per-depth history". The amendment
keeps both halves and puts the boundary where the history is: a
stateless body HAS no per-depth history, so constant space costs
nothing; a stateful body has O(n) history, and O(n) space is exactly
what a slot vector costs for the same n live things. Space is O(n)
precisely when there is O(n) state.

**Stateless** (of a lambda body, a fixpoint of the same shape as the
M6 effect analysis):

- the body is Sync (existing `intrinsic_effect`; a cross-cycle node
  such as `~` or any async builtin makes it Async, so those are
  already out);
- every builtin the body reaches is `STATELESS` (the fact is recorded
  per builtin, `ctx.builtin_stateless`, `lib.rs:2038`);
- the body contains no `<-` target (the connect identity law gives a
  target per-activation identity in recursion — a body that has one
  has state by definition);
- every callee it reaches, transitively, is stateless.

The example the writeup started from:

```graphix
let rec publish_loop = |a: Array<string>, c: i64| select a {
    [] => null,
    [p, rest..] => {
        net::publish(p, c)$;
        publish_loop(rest, c + 1)
    }
}
```

`net::publish` is Async, so today this nests and each path has its own
publish site up to 256 paths, then bottoms. Under the amendment it is
the same program without the cliff.

What follows for the three kinds of body:

| body | today | amended |
|---|---|---|
| stateless (arith, pattern, stateless builtins) | one activation, framed loop | unchanged |
| stateful Sync (`count`, `uniq`, `<-` ...) | one activation — **6** above | activation per iteration — **3** |
| Async | activation per level, capped at 256 | activation per level, memory-bounded |

Only the middle row changes value semantics. The fuzz corpus will
count its pins; expect few — a P7 builtin inside a `let rec` tail body
is rare.

## 3. Mechanism — the interpreter needs no new driver

This was the surprise. The non-tail dispatch path (`node/lambda.rs:511`,
`crate::stack::ensure_sufficient(|| self.body.update(..))`) IS the
per-iteration driver:

- **Activation = a retained instance per level** (the retention ruling
  2026-08-13). Slot identity is depth, which is ordinal position in
  traversal order — the same rule MapQ applies to Map keys today (a
  middle insert shifts later slots). No drift.
- **Re-feed** on the next cycle is the inner call site delivering the
  new tail to its already-bound instance. Exists.
- **Shrink**: the base-case arm selects, the recursive arm sleeps,
  and every deeper activation sleeps with it — pause, not reset.
  Wake resumes. A `RESTART` builtin clears on sleep either way; a
  subscription pauses.
- **Concurrency is right**: in `cons(f(v), map(tail, f))` the
  recursive argument does not depend on `f(v)`, so every level
  dispatches in the first cycle — as MapQ instantiates every slot at
  once. An accumulator threaded THROUGH an async value serializes
  (the §1 timer chain took 100 cycles), and FoldQ serializes
  identically (slot i's acc is slot i−1's output).
- **Stack** is already heap-segmented under `ensure_sufficient`; the
  only bound is the counter.

So the interpreter's change is: the `tail_loop` gate reads STATELESS
instead of `lambda_is_sync`, and the depth counter goes (§5). The
frame machinery (`replay_frames.md`) stays exactly as it is for the
stateless loop — frames are the re-derivation discipline for a body
with nothing to keep.

An iterative slot-vector driver (FoldQ generalized to successive
tail-call arguments) was considered and is NOT proposed: nesting under
stacker costs a few KB of segment per level against the tens of KB of
the retained instance itself, so the driver would save nothing that
matters. If the per-activation footprint turns out to be the problem
(§9 P0), the fix is instance size, not the driver.

**Retention vs deletion** (RESOLVED 2026-08-29 — recursion adopts
MapQ's rule). The open question was whether recursion should RETAIN
excess activations asleep (as it did) or DELETE them like MapQ deletes
excess slots on shrink. It was reframed as a SEMANTICS question, not
just memory: retaining an unreached depth's state and RESUMING it on
re-descent is a per-depth `count(e)` whose value depends on the loop's
depth history — unpredictable, and inconsistent with MapQ, whose
regrown slots are fresh. Since a recursion activation IS a collection
slot, recursion now follows the identical rule: **a depth not reached
this cycle is deleted immediately; re-reaching it is a fresh
activation.** Interp built (`99387e17`): a scoped `ctx.shrink_unwind`
flag, set only while `Select::update` sleeps an arm it actively
DESELECTS (a genuine shrink, not a whole-recursion pause), makes a
recursive-edge `CallSite::sleep` delete its callee (cascade) instead of
retaining it; cleared crossing a callee body (`GXLambda::sleep`) so a
whole-recursion pause and external calls in the deselected arm retain
(sleep-is-pause). `sleep is pause` still holds for arms that PERSIST; a
slot that ceases to exist is deleted. The distinction: an arm is a
fixed position (pause/resume), a recursion depth is a transient
invocation (delete/fresh). Deferred (memory-only, transparent): the JIT
`SelfBlock` free on unwind — a fused recursion carries no observable
per-depth state (stateful de-fuses), so the differential agrees without
it; the JIT still retains its block tree until Drop. If oscillation
thrash from immediate delete+realloc ever bites, add a reset-on-reuse
pool to BOTH systems (semantically identical, avoids the churn) — the
refinement noted at the ruling.

### As built — P1a (2026-08-24)

- The fixpoint in `analysis::infer_effects` computes `LambdaFacts
  { effect, stateless }` in one walk (`body_facts`/`node_facts`/
  `callee_facts`), stored beside `intrinsic_effect` as
  `LambdaDef::stateless`. A `<-` is state only when its target is one
  of the body's own bindings (`Refs::with_bound` over the body); a
  `ConnectDeref` is conservatively stateful; builtins read
  `ctx.builtin_stateless`. The tail-loop gate reads
  `lambda_is_stateless` (Sync ∧ stateless); `#[tail_recursive]` asserts
  it (a stateful or async body fails the assertion — the async case was
  a pre-existing hole, the attribute accepted a body that nests).
- `STATELESS` was redefined to mean cross-invocation STATE only —
  effects do not distinguish one activation from many — and its reach
  widened accordingly: `dbg`, `log`, `error`, `now`, `exit`,
  `join_path`, `tempdir_path`, `all`, the variadic divide,
  `buffer::decode`, `hbs::render`. Its previous consumer (the
  transient-recursion gate) is gone; this gate is the only one. Still
  stateful, correctly: `count`, `sum`, `product`, `min`, `max`, `mean`,
  `uniq`, `once`, `take`, `skip`, `hold`, `array::window`, the rand
  family, `buffer::encode`, the http clients.
- The kernel needed one seam: `emit_body_tail` intercepted every
  tail-position self-call as a rebind-and-jump structurally, before
  asking whether the kernel loops, so a stateful body reached
  `emit_tail_rebind_jump` with no loop head ("TailCall in kernel
  without has_tail_loop") and de-fused. It now jumps only when
  `LowerCtx::tail.loop_head` exists; otherwise the self-call is the
  ordinary native recursive call whose activation owns its DynCall
  site blocks — and `f(n - 1, acc + max(n))` fuses and answers 55 on
  both engines (`tail_stateful_scalar`).
- Pins (`lang/functions.rs`): `tail_stateful_per_iteration` (`count`,
  3 not 6 — interprets: the `[x, rest..]` slice pattern is the
  pinned select residue, not the rule), `fold_stateful_per_slot` (the
  fold twin, 3), `tail_stateless_collapses` (60, unchanged),
  `tail_stateful_scalar` (55, fuses); `lang/attributes.rs`
  `tail_recursive_stateful` (rejected). Coverage note: `count` inside a
  select ARM still de-fuses through the P7 `SLEEP_RESTARTS` arm gate
  whatever the loop does.
- P0 measured first (interp, debug build): ~1.9 ms and ~28 KB per
  recursion activation against ~0.7 ms and ~13 KB per MapQ slot —
  2–3× the thing it generalizes, both dominated by the lazy-bind cost
  `interp_lazy_bind_cost.md` already names. Not a blocker for the
  semantics; a 10k-deque first result is ~20 s debug / a few seconds
  release, and instance size is where to look if that bites.

## 4. Mechanism — the JIT

Two independent pieces.

### 4a. Stateful tail loops

A fused tail loop is rebind-and-jump over ONE set of state words
(`emit/lower.rs`). Under the amendment a stateful Sync body needs a
state set per iteration.

- **Phase 1 — de-fuse.** A tail loop whose body is not stateless
  refuses to fuse, exactly as the `SLEEP_RESTARTS` arm gate de-fuses a
  select whose arm reaches a restart builtin. The interpreter nests it.
  This lands the semantics with zero new emission and lets the corpus
  count the change.
- **Phase 2 — per-iteration site blocks (coverage, later).** The
  machinery exists: in-loop callee SITE BLOCKS are chain leaves with a
  `words` stride from `graphix_slot_state_table` (`emit/lower.rs:516`),
  and the shrink-to-zero rule truncates a chain whose level shrinks.
  A stateful tail loop selects its block by iteration index and
  truncates on a shorter chain. Async bodies interpret regardless.

### 4b. Depth bounded by memory, not a counter

Eric's preference: limit depth to available memory. The interpreter
is there once the counter is deleted. The kernel's non-tail
self-calls run on the machine stack, so it needs a stack switch —
the JIT twin of `ensure_sufficient`:

- The wrapper writes a STACK LIMIT into a wire slot per invocation
  (derived from `stacker::remaining_stack` at entry).
- At every non-tail recursive call site (self and mutual edges) the
  kernel compares the stack pointer against the slot — one inline
  compare-and-branch — and on the slow path calls
  `graphix_grow_stack(thunk, args_block)`: the args are spilled to a
  block (the `graphix_dyncall` marshaling shape), the helper runs
  `stacker::grow(SEGMENT, || thunk(args_block))`, and the thunk — one
  generated entry per recursion-target kernel — unspills, installs the
  NEW limit in the callee's wire slot, calls the kernel with the
  register ABI, and writes the result back.
- Cost on the fast path: the compare. fib(30)'s 1.6M calls is the
  benchmark that measures it.

The trampoline lands WITH the counter deletion, not after: removing
the interpreter's bound while the kernel keeps one makes the engines
disagree above 256 on any deep sync non-tail recursion. The
entry-only interim (`ensure_sufficient` around `Kernel::update`, a
fresh segment per invocation) bounds depth at ~segment/frame, roughly
1e4–1e5 — a large fixed number, not memory, and silent (a segfault)
past it. Not acceptable as the resting state.

### As built — P1b (2026-08-24)

- **The counter is gone on both engines.** `Control` is the two
  interrupt flags again; `DEFAULT_MAX_CALL_DEPTH`, `depth_push/pop`,
  the trip and trip-poison bits, `RtDiagnostic::CallDepthLimit` (the
  enum is empty; the runtime channel stays), `GRAPHIX_DBG_DEPTH`, the
  five `graphix_depth_*` helpers, the scaffold's per-loop depth charge,
  the scrutinee ride's poison read and the interp's `trip_poisoned`
  ride gate are deleted. The interp's non-tail dispatch was already
  under `ensure_sufficient`; nothing else changed there.
- **The kernel trampoline.** `graphix_stack_check` (0 interrupted /
  1 call / 2 grow) replaces `graphix_depth_push` at every SELF-call
  site — the same helper-returned-flag branch shape, so the fast path
  costs what the counter cost. On 2 the site spills its CLIF args to a
  stack slot at an 8-byte stride and calls `graphix_grow_stack(thunk,
  args, out)`, which runs the kernel's SPILL THUNK on a fresh 32MB
  segment (`stacker::grow`); the thunk (`jit::define_spill_thunk`, one
  per recursion-target kernel, declared before the body and defined
  after it) loads the params typed as the signature declares, calls
  the kernel, stores the two result words. Cross-kernel edges are
  acyclic (mutual recursion de-fuses), so only self-calls check.
  2,000,000 deep: 1.4s, 507MB — ~250 bytes of stack per level
  (`jit_deep_nontail_probe`, `deep_nontail_recursion_completes`).
- **The block-tree walks were the second recursion.** `Kernel::drop`'s
  `free_self_block_tree` and the frame reset's `reset_self_block_tree`
  recursed one Rust frame per activation, unguarded — invisible under
  the 256 cap, a tokio-worker overflow at 20k on the first probe. Both
  are explicit worklists now.
- **The stack budget** (§5 in practice): `GRAPHIX_STACK_BUDGET` (bytes)
  or `graphix_compiler::set_stack_budget`, unlimited by default; a
  thread-local counts live grown segments, and a grow that would exceed
  the budget ABORTS the runtime (`Control::abort`, the sticky shutdown
  Ctrl-C arms — the node-walk still gets that one segment so it unwinds
  at its next poll). The fuzz pool gives every child 1GB, because a
  runaway kernel recursion otherwise grows stack at ~350MB/s until the
  subject deadline, and a box runs 64–288 workers.
- **Pins.** Nine campaign witnesses were INFINITE recursions that the
  cap turned into agreeing bottoms (`f(n - 0)`, `select f(0) {..}`,
  `f(f(x))`); they pin nothing without a cap and are retired —
  corpus 446 → 437. The finite ones stay as "completes above 256"
  witnesses. The harness already classifies an interp that exhausts
  its CPU budget while the JIT's value stands as SLOW (AGREE), so a
  deep legitimate recursion is not a divergence.
- **The measurement that matters.** The interp's cost per activation
  is SUPERLINEAR in depth: 1.1ms/37KB at 2k, 1.8ms/44KB at 5k,
  5ms/110KB at 20k (a 20k-deep `n + f(n - 1)` is 102s and 2.2GB in
  `--no-fusion`; the JIT does 2M in 1.4s). `GRAPHIX_DBG_PERF` puts
  ~1ms in `bind` and ~0.8ms in `setup` per activation at 2k — each
  activation COMPILES its body from the AST — and something else grows
  with depth on top. This was hidden behind the cap. It is now the
  critical path for everything async in this design (P2's slots
  interpret by construction) and is P1c below.

## 5. Containment, and what a bound means now

`atomic_recursion.md` already rules that a program may spin forever
inside one cycle and that containment lives outside the language: the
cooperative interrupt, armed by a human or an embedder, observable by
no program. Memory is the same shape: a recursion that exhausts memory
aborts the process on both engines (stacker's segment allocation and
the allocator fail the same way), the fuzz children run under
`GRAPHIX_FUZZ_MEM_LIMIT`, and the shell's Ctrl-C path is unchanged.
There is no depth TRIP any more — no delivered bottom, no
`depth_tripped` poison, no whole-derivation rule — because there is no
depth limit to trip. The seven `depth-*`/`nontail-recursion-depth-bound`
pin families re-bless as "deep recursion completes" witnesses (§10).

## 6. The collection trait

```graphix
trait Collection {
    val fold: fn(self<'a>, init: 'b, f: fn(acc: 'b, x: 'a) -> 'b) -> 'b;
    val filter_map: fn(self<'a>, f: fn(x: 'a) -> ['b, null]) -> self<'b>;
    val map: fn(self<'a>, f: fn(x: 'a) -> 'b) -> self<'b>
        = |c, f| filter_map(c, |x| f(x));
    val filter: fn(self<'a>, f: fn(x: 'a) -> bool) -> self<'a>
        = |c, f| filter_map(c, |x| select f(x) { true => x, false => null });
    val find: fn(self<'a>, f: fn(x: 'a) -> bool) -> ['a, null] = ..;
    val find_map: fn(self<'a>, f: fn(x: 'a) -> ['b, null]) -> ['b, null] = ..;
    val flat_map: fn(self<'a>, f: fn(x: 'a) -> self<'b>) -> self<'b>;
    val len: fn(self<'a>) -> i64 = |c| fold(c, 0, |n, _| n + 1);
}
```

- **Required**: `fold` (traversal), `filter_map` (construction with
  selection) and `flat_map` (construction by concatenation — deriving
  it needs an identity element, which no self argument can witness).
  `map`/`filter` derive from `filter_map`; `find`/`find_map`/`len`
  from `fold`. Derived methods inherit the required method's slots, so
  a user type gets per-slot semantics from three hand-written
  recursions.
- **The blessed implementations** are the intrinsics, unchanged in
  mechanism: `impl Collection for Array { let map = 'array_map; let
  fold = 'array_fold; .. }` — the reserved marker names as impl
  bodies, `LambdaDispatch::Collection` keyed on the impl binding
  instead of on the free function. They override every default,
  including `len` (O(1)). List and Map likewise. The scaffold loops
  (`emit/scaffold.rs`) stay as THE fast path: a list-accumulator body
  fused as a tail loop is a cons per element plus a reverse plus a
  copy, ~20–30× off the scaffold's ~1000× (the List benches' 15× cons
  gap), and the predictable-performance rule says that cannot be the
  default. What the general mechanism replaces is the intrinsics being
  the ONLY way; retiring their interp side (`node/collection.rs`'s
  slots, prefix retention, per-slot firing/sleep/replay, result
  assembly) in favour of Graphix bodies is a separate decision this
  doc does not make.
- **A user's linear structure** writes `filter_map` as a tail chain
  over a list accumulator, front to back, finished by
  `list::to_array_rev` (one Rust walk: reverse + build; to be written)
  or its own constructor. Front-to-back matters: the suffix pattern
  `[init.., x]` would avoid the reverse but makes depth d element
  n−1−d, so every append shifts every slot. With the list accumulator
  an async update at slot d re-conses the suffix (n−d) and rebuilds
  (n) — ~3× MapQ's n clones per update, and the retained chain shares
  tails, O(n) cells total. In-place `push` on a uniquely owned array
  (the JIT's owned-accumulator loops) was considered and does not
  work here: under per-iteration activations iteration i's
  accumulator is retained by i and passed to i+1, never unique.
- **Naming.** `Map` is the builtin type; `deftrait` rejects a trait
  named like a TYPEDEF in scope but a builtin is not a typedef, and
  `Map` in a bound position would parse against the type. The doc
  uses `Collection`; Eric's call.
- **Map is a functor over VALUES** under the last-parameter hole
  (`self<'a>` ≡ `Map<'k, 'a>`), so `Collection::map` on a Map maps
  values and `Collection::fold` folds values. `map::map`/`map::fold`
  are PAIR operations (`kv: ('k, 'v) -> ('k2, 'v2)`, `map.gxi:3`) and
  stay as they are. Haskell draws the line in the same place
  (`Functor (Map k)`, `foldrWithKey` beside it).

## 7. Higher-kinded self: the hole

`self<'a>` is not a trait parameter; it is `self` as a type
CONSTRUCTOR. `traits.md` §5 declined it for two reasons: "the
collection HOFs are compiler intrinsics with per-slot semantics; a
`Map` trait abstracting over them is a different project" — this doc
is that project, so the reason dissolves — and "`['a, null]` is
structural and cannot be a target", which stands and does not matter:
Option is not a collection.

The design that keeps v1's clean property (TYPING never needs
resolution — the call's type is known from the trait alone, impl
selection is a typecheck1 codegen decision):

- **A constructor is a type with a HOLE in its last parameter.** A
  constructor variable is a `TVar` that binds to such a type.
  `self<'x>` is the form `App(self, 'x)`: fill the hole with `'x`.
  Normalization: `App(c, a)` with `c` bound → the filled type.
- **Decomposition is syntactic, on the receiver's outermost form only**
  (Haskell's rule; no kinds in unification): unifying `App(c, a)` with
  `Ref{name, params}` (params non-empty) binds `c := Ref{name,
  params[..n−1] ++ [Hole]}` and unifies `a` with `params[n−1]`;
  `Array<e>` → `c := Array<Hole>`, `a ~ e`; `Map<k, v>` → `c := Map<k,
  Hole>`, `a ~ v`; anything else — a struct, a tuple, a union, a bare
  primitive — is an error ("not a constructor"). `App(c, a) ~ App(c',
  a')` unifies pairwise. An unbound plain tvar against `App(c, a)`
  binds to the `App` form. One new arm pair in `contains`; the
  typechecker-must-be-instant rule is not at risk.
- **Impl heads name the constructor**: `impl Collection for AltList`
  (a parameterized abstract, hole = its last parameter), `impl<'k>
  Collection for Map<'k, _>`, `impl Collection for Array`. `find_impl`
  decomposes the receiver argument's type and matches heads by
  constructor identity; `heads_overlap` treats `Hole` as a leaf that
  overlaps anything in its position.
- **Each impl's method has an ordinary polymorphic signature** once
  the hole is filled: Array's `map` is `fn(Array<'a>, f: fn(x: 'a) ->
  'b) -> Array<'b>` — today's `array::map` exactly. Dispatch is v1's
  flow (`resolve_trait_call` → `find_impl` → re-point).
- **Generic code**: `'c: Collection` makes `'c` a constructor variable
  and the receiver type is written `'c<i64>` (new syntax: a tvar
  applied to parameters, expression and type positions). The
  per-parameter sugar extends: `|c: Collection|` ≡ `'c: Collection,
  'e fresh, c: 'c<'e>`. Per-callsite elaboration means the def-time
  body only carries `App('c, 'b)` as a form; it normalizes when `'c`
  binds at each instance.
- **Union receivers are an error** for a hole trait (v1's
  union-dispatch select is for non-constructor self; extend later if a
  case appears).
- **Rust-backed abstracts with parameters** (`type AltList<'a>;`) are
  `Ref`s and decompose. Eric's newtype example, `type AltMap<'k, 'v> =
  Abstract<Map<'k, 'v>>`, delegates: `let map = |m, f|
  AltMap(map::map(m.0, |(k, v)| (k, f(v))))`.

## 8. Deferred: v2 trait parameters

`trait WithErr<'e>` / `impl WithErr<`FsErr> for File` — parameters as
OUTPUTS of impl selection, one impl per self type (`traits.md` §5).
Deferred because the client is thin. The one Eric named is a custom
error type, and it comes in two shapes:

- **Per-impl error type** (`trait Read<'e> { read: .. -> Result<bytes,
  'e> }`): io declares one fixed `` `IOError(string) `` for every
  stream (`io.gxi:16`) and has no pressure for more.
- **Conversion at `?`**: a module declares `throws MyErr` (an abstract
  type — the real motive is a stable, hidden error API), writes `impl
  From<[`IoErr(string), `ParseErr]> for MyErr`, and `?` inside a
  `throws MyErr` function converts. This WOULD work under v2 with no
  return-type inference — the self at a `?` site is DECLARED by the
  enclosing function's `throws`, the parameter is the union of
  accepted sources, and the site check is `source ⊆ 'src` (plain
  `contains`). But union error types plus `?` already cover most of
  what Rust needs `From` for; the residual is encapsulation.

The hole (§7) cannot substitute: it reads its bindings from the
receiver's STRUCTURE (unification, no impl lookup), which is exactly
why it is resolution-free; an error type is a fact about the IMPL,
i.e. the impl table, which is what "parameters as outputs of
selection" means and why v2 pays the resolution-order cost. Smuggling
an error type into the receiver's structure is a phantom parameter
(`File<'e>`) — the pattern the io migration just removed. When a real
module wants v2, build it against that module.

## 9. Pressure tests

Three examples, one per part of the mechanism; they are the doc's
fixtures.

1. **Newtype delegation** — `type Grid<'a> = Abstract<Array<Array<'a>>>`,
   a spreadsheet whose cells are async evaluations; `impl Collection
   for Grid` is two `array::map`s. Stresses only the trait: the hole,
   `use core::Collection::*`, the impl head form. The slots are the
   intrinsics'.
2. **A linear structure** — a deque of 5,000 live subscriptions.
   `filter_map` is a tail chain with a list accumulator. Stresses the
   amendment: this is the case that hits 256 today, and where the
   interpreter's per-activation footprint gets measured (an activation
   is a whole body instance — select, slice, cons, two call sites —
   several times a MapQ slot's; `interp_lazy_bind_cost.md` names
   instantiation as the known slow path).
3. **A tree** — the admin package's browser, a netidx path tree with a
   subscription per node: `type Tree<'a> = [`Node('a, Array<Tree<'a>>)]`,
   `map` is `` `Node(f(v), array::map(children, |c| map(c, f))) `` —
   recursion THROUGH a callback. Slot identity is tree position, which
   is the right identity for a browser (collapse a subtree: it sleeps;
   expand: it wakes). It works in the interpreter today because async
   bodies nest, and the bound bites by depth (20), not size (a million
   nodes). What it stresses is the pinned de-fuse
   (`fold_callback_name_collision`: a rec callee inside a collection
   callback keeps the collection on the node-walk) — moot for the
   async tree, the coverage gap for a sync tree fold.

## 10. Deletion inventory, and what stays

Deleted:

- `DEFAULT_MAX_CALL_DEPTH`, `Control::{max_call_depth,
  set_max_call_depth, depth_push, depth_pop}`, the interpreter's trip
  arm in `GXLambda::update` (`node/lambda.rs:436`), the `DEPTH TRIP`
  diagnostic and `GRAPHIX_DBG_DEPTH`.
- `ctx.depth_tripped` and its rides (the scrutinee-ride refusal, the
  tainted guard's held-ride false, pop-to-zero clearing), the
  whole-derivation trip rule.
- `graphix_depth_push`/`_pop`/`_tripped` (`emit_helpers.rs:616`), the
  scaffold's depth charge (`emit_helpers.rs:668`), the kernel's
  value-level trip propagation and `builder.rs:100`'s region-level
  report.
- Pins re-blessed from "trips at N" to "completes at N":
  `depth-guard-jul2026`, `depth-guard-marshal-jul2026`,
  `depth-trip-delivered-bottom-aug2026`,
  `depth-trip-quiet-remint-aug2026`, `depth-trip-settle-aug2026`,
  `depth-trip-whole-derivation-aug2026`,
  `nontail-recursion-depth-bound-aug2026`,
  `empty-scaffold-depth-charge-aug2026`.

Stays:

- MapQ/FoldQ and the eight scaffold loops, as the blessed impls (§6).
- Frames and the framed tail loop, for stateless bodies.
- The retention ruling, the interrupt, `GRAPHIX_FUZZ_MEM_LIMIT`.
- Every bottom-out / organic-firing / quiet-flag rule: activations
  fire and ride exactly as retained non-tail activations do today.

## 11. Phasing and open questions

- **P0 — measure.** Test 2 (§9) in the interpreter: time to first
  result and RSS for 1k/10k activations with an async body, against
  the same over `array::map`. This decides whether instance size needs
  work before the semantics land.
- **P1 — the amendment.** The stateless predicate (analysis.rs, the
  M6 shape); the `tail_loop` gate reads it; the JIT de-fuses stateful
  tail loops (4a phase 1); the counter and the trip machinery go
  (§10) TOGETHER WITH the kernel stack trampoline (4b); pins
  re-blessed; fleet soak.
- **P1c — the interp's activation cost** — PROFILED 2026-08-25 ("As
  measured — P1c" below). The constant is FINE: ~8µs / ~15KB per
  activation in release (the dev build's "2ms" was opt-level 0). The
  superlinear term is ONE thing: the DYNAMIC SCOPE PATH. `Scope.dynamic`
  is a flattened `Path` string, an activation's body compiles under its
  call site's dynamic path, every `sel`/`do`/`ca` level appends a
  component (`Path::append` copies the string and `is_canonical`
  rescans it), and the arm's CallSite retains the result — one
  ~11-byte-per-level string per activation, Σ = 2.0GB at 20k deep
  (measured 2050MB), 78% of the cycles in `is_canonical`. Its only
  consumer is the catch registry. Fix, if built: the dynamic scope
  becomes a parent-linked chain carrying its catch (~200 lines, no
  cloner, no second compile path). The flat per-slot cost is diffuse
  (~40µs; 10k async slots in 0.4s) — live with it. The clone_rebind
  resurrection is DEAD: the constant it would remove is 8µs. BUILT
  2026-08-25 ("As built — P1c" below): lexical-only `Scope::append` AND
  the chain, since a `catch` in a recursive body is a shape Eric wants
  to work — deep 20k `--no-fusion` 4.10s/2050MB → 0.52s/236MB.
- **P2 — the trait.** The hole in the type system (§7), `trait
  Collection` in core with the intrinsic impls under it, `list::
  to_array_rev`, the three pressure tests as fixtures, generator
  vocabulary for traits (there is none yet — the aug24 soak note).
- **P2b — the measurement.** Graphix bodies for every operation of
  all three intrinsic collections, benchmarked against the intrinsics
  on both engines (`bench/` self-timed corpus + the interp footprint
  of P0); an intrinsic that is not worth its machinery is deleted,
  per operation.
- **P3 — coverage.** Per-iteration site blocks for stateful tail
  loops (4a phase 2); recursion-through-callback fusion.

Decided (Eric, 2026-08-24):

- **Retain** shrunk activations to start. Expect to revisit; it is a
  runtime/GC question and can change without touching semantics.
- The trait is **`Collection`**.
- **`len` stays** in the trait.
- **`flat_map` is in the trait**, required (its callback returns
  `self<'b>`, and deriving it needs an identity element no
  self-argument can witness). Three required methods: `fold`,
  `filter_map`, `flat_map`.
- **Write Graphix bodies for ALL the intrinsics' operations** — Array,
  List and Map — and measure them against the intrinsics on both
  engines. "They're all short; if the intrinsics turn out not to be
  worth what we thought they were worth then there's code we can
  delete." The measurement decides what stays, per operation, not
  principle. §6's "blessed implementations" paragraph is the position
  until the numbers are in.

**GO** (Eric, 2026-08-24).

## As measured — P1c (2026-08-25)

Release build (`-C force-frame-pointers=yes`, debug=1, stdlib minus
gui/tui/http/db), 14 cores. Three subjects, N ∈ {2k, 5k, 10k, 20k}:
`deep` = `n + f(n - 1)` (non-tail, sync — interprets only under
`--no-fusion`), `atail` = `f(n ~ (n - 1), acc + n)` (async tail — an
activation per iteration on both engines), `amap` =
`array::map(array::init(N, |i| i), |x| x ~ (x * 2))` (flat async
slots). Wall / max RSS; shell startup ≈ 0.03s / 36MB.

| subject | mode | 2k | 5k | 10k | 20k |
|---|---|---|---|---|---|
| deep | `--no-fusion` | 0.11s / 67MB | 0.37s / 185MB | 1.19s / 568MB | 4.10s / 2050MB |
| deep | fusion | 0.03s / 36MB | 0.03s | 0.03s | 0.03s / 39MB |
| atail | either | 0.12s / 71MB | 0.40s / 193MB | 1.25s / 584MB | 4.44s / 2083MB |
| amap | `--no-fusion` | 0.11s / 65MB | 0.22s / 115MB | 0.41s / 197MB | 0.83s / 359MB |
| amap | fusion | 0.05s / 48MB | 0.10s / 65MB | 0.17s / 94MB | 0.32s / 153MB |

Nesting: 10× depth = 37× time, 30× memory — the per-activation
memory grows ~10 bytes × depth. Flat: ~40µs and ~15KB per slot,
linear. `GRAPHIX_DBG_PERF` at deep 20k: binds=19735, bind 3987ms,
setup 3863ms (the body compile), tc1 68ms, analyze 50ms; at amap 20k:
binds=40000, bind 325ms — 8µs each.

perf (`cycles:u`, the cpu_core event — this box is hybrid, the
cpu_atom event holds 48 samples and misleads a report read top-down),
deep 20k: `netidx_core::path::is_canonical` 78% SELF, every sample
under `select::compile`'s per-arm `append_block("sel", …)` →
`Scope::append` → `Path::append` → `Path::from`; 79% of all cycles
under `CallSite::bind`, 45% under `setup_dynamic_bind`. amap 20k: no
symbol above 11% — `avl::Node::make_mut` 10.8% (`env.by_id` COW
inserts), `drop_in_place<avl::Node>` 4.5%, `GXRt::ref_var` 3.7%,
`CallSite::update` 2.9%, `analysis::mark_recursion` 2.4%.

**Mechanism.** `Scope { lexical, dynamic }` are both `ModPath`
strings and `Scope::append` extends both. The dynamic half exists for
one thing: the catch registry. A `catch` statement installs under
`scope.dynamic + "#c<id>"` and the rest of its block compiles under
that covered scope (error.rs, `Catch::compile`); `?` resolves its
handler at compile time and a call site with a throwing callee at
`typecheck0`, both via `Env::lookup_catch`, which walks
`Path::dirnames(..).rev()` longest-prefix-first over
`Env.catch: Map<ModPath, (BindId, ExprId)>`; the lambda def gate
temporarily overrides the def scope's key with a faux catch that
collects the body's `throws`; and an instantiated body compiles under
its CALL SITE's dynamic scope (lambda.rs `InitFn`) so a `?` in a callee
finds the caller's handler. Nothing else reads `.dynamic` (13 sites).
So the dynamic scope is a parent chain spelled out as a string, and
every activation re-spells its entire ancestry: one retained
`ArcStr` of ~11 bytes × depth per level, Σ_{d≤20000} 11d ≈ 2.2GB —
the measured 2050MB — plus an O(depth) copy and scan per append.

**The fix, if built.** `DynScope(Arc<DynNode { parent: Option<DynScope>,
catch: Option<(BindId, ExprId)> }>)` with `nearest` (the closest
installed catch at or above) computed at creation; `Scope::append`
mints a child (the dynamic half carries no text anyone reads);
`Catch::compile` installs on the covered child as it creates it;
`lookup_catch` reads `nearest`; the def gate compiles the body under a
fresh faux-catch child instead of overriding and restoring an existing
key; `Env.catch` disappears. Equivalence: both lookups run at
compile/typecheck time after every covering catch is installed, and a
catch only ever covers scopes created AFTER it (the rest of its block),
so `nearest`-at-creation sees the same registry state — the one
override, the def gate's, is what the faux child reproduces. O(1) per
level in time and memory; deep 20k should land near the flat cost
(~0.9s / ~300MB), and the interp's per-activation time drops ~4× on
top of the memory. Est. ~200 lines. Not decided whether to build it now
or live with the quadratic until the trait needs it.

Measurement kit (scratchpad, this session): `deep_N.gx` / `atail_N.gx`
/ `amap_N.gx`, `sweep.sh <bin> [--no-fusion]`, `prof.sh <name> <cmd>`
(perf record + self/children reports). The profiling binary was built
with `RUSTFLAGS="-C link-arg=-fuse-ld=mold -C target-cpu=native -C
force-frame-pointers=yes" CARGO_PROFILE_RELEASE_DEBUG=1 cargo build
--release -p graphix-shell --no-default-features --features
array,str,map,sys,list,rand,re --target-dir ~/tmp/target/prof` (12m55s
clean).

## As built — P1c (2026-08-25)

Eric's question that settled the design: *why extend the dynamic scope
per iteration at all?* The dynamic scope has exactly one kind of event
that must move it — a handler install — and everything else on it was
the lexical half's business copied over by `Scope::append`. So both
halves of the fix landed together:

- **`Scope::append` is lexical-only.** Blocks, select arms, catch
  handler bodies, lambda defs, impls and modules extend the lexical
  path and inherit the dynamic scope unchanged. A recursion whose body
  installs no handler shares its caller's dynamic scope across every
  activation: zero per-level cost.
- **`DynScope` is a parent-linked chain, one node per handler install**
  (`lib.rs`: `DynScope(Option<Arc<DynNode { catch: (BindId, ExprId),
  parent }>>)`). `Catch::compile` covers the rest of its block with
  `scope.with_catch(..)`; `?` (compile) and a throwing call site
  (`typecheck0`) read `scope.dynamic.catch()` — the node IS the
  registry, so `Env.catch`, `lookup_catch`, its `restore_lexical_env`
  clones and its `unbind_scope_subtree` sweep are deleted. The lambda
  def gate compiles the body under a faux-catch CHILD of the def scope
  (`def.scope.with_catch(faux)`) instead of overriding the def scope's
  key and restoring it afterwards. A recursion whose body DOES install
  a handler adds one node per activation — the chain's legitimate
  length, O(1) per level, and the case the lexical-only change alone
  would have left quadratic (Eric: "catch in the body is something I
  could see being useful, so we have to handle it").
- Equivalence with the string registry: both lookups run at
  compile/typecheck time after every covering catch is installed, and a
  catch covers only scopes created after it (the rest of its block), so
  reading the covering node at lookup sees exactly what the longest-
  prefix walk saw. The only registry MUTATION was the def gate's
  override, which the faux child reproduces (the def-time body's `?`s
  resolve to the faux bind, collecting the body's throws; instances
  compiled at call sites never see it, as before). The core-trait
  prototype scope (`traits.rs build_prototypes`) and the rt's
  initial-scope compile (`gx.rs`) start from `DynScope::root()` — they
  used the module path as a dynamic path, which could only ever have
  found a catch installed at exactly that module level.
- The chain can be as deep as a handler-per-activation recursion, so
  `DynNode`'s drop is unwound into a loop (`Arc::try_unwrap` down the
  parent chain), per the stack-discipline rule for destructors, and its
  `Debug` prints depth + innermost catch rather than the chain.

Measured (release, quiet box; before → after):

| subject, `--no-fusion` | 10k | 20k |
|---|---|---|
| deep `n + f(n-1)` | 1.19s / 568MB → 0.28s / 134MB | 4.10s / 2050MB → 0.52s / 236MB |
| async tail | 1.25s / 584MB → 0.35s / 151MB | 4.44s / 2083MB → 0.69s / 271MB |
| flat async map | 0.41s / 197MB → 0.41s / 196MB | 0.83s / 359MB → 0.84s / 360MB |

The nested cases are linear now (~25µs, ~10KB per activation above the
36MB base); the flat case is untouched, as it should be. The after
profile is diffuse — typecheck (`RefHist::new`, `contains_dispatch`,
`settle_terminal`), `env.by_id` COW, pool take/drop — nothing above 3%.
`GRAPHIX_DBG_PERF` deep 20k: bind 725ms / setup 535ms over ~18k binds.

Pins: `lang/errors.rs` `catch_per_activation` (a handler in a recursive
body belongs to its activation — three distinct cells sum to 6),
`catch_in_callee_stays_in_callee`, `catch_through_call` (a body that
installs nothing reaches the caller's handler); `graphix-shell/tests/
recursion_memory.rs` runs 20k interpreted activations in a child and
bounds peak RSS at 800MB (debug peaks at ~420MB now; the string form
peaked past 2GB in either build) — 10s in debug, so it runs un-gated.

**What the release regress found (4 "regressions", 0 semantics).** The
four surviving depth-trip-era pins whose programs are UNBOUNDED
DESCENTS (`f(x - 1)` at `x = 0`: `03_soak_trip_adjacent`,
`01_depth_trip_then_refwrite`, `trip-poison-extent/01,02`) came back
`interp: Timeout` vs `jit: RuntimeErr(runtime did not respond)`. Both
engines hit the SAME 1GB stack budget — the JIT in 0.84s, the node-walk
in 16.7s (4.7GB of activations at ~1.7KB of native stack each) — and
the regress deadline is 3s, so which containment fired first was a
race the faster interp had just moved. Under `atomic_recursion.md`
containment is outside the language, so the harness now says so: a
budget abort sets `CtlFlag::Budget` beside `Abort` on the runtime's
`Control` (`stack::budget_abort`, the ONE exit for both engines — the
kernel's `graphix_stack_check` used to abort silently, without the log
line or any mark), `GXHandle::budget_aborted()` reads it, and the fuzz
runner maps a `RuntimeErr` from a budget-aborted runtime to
`Outcome::Timeout` — one outcome for a runaway whichever limit stops
it, attributed to the subject's own runtime (regress runs subjects
concurrently in one process, so a global counter could have credited
one subject's abort to another). The pins stay: they now pin "an
unbounded descent is a runaway on both engines". Fleet consequence: a
mutated program whose base case is lost is common, and without the
rule every one would have been a Timeout-vs-RuntimeErr finding at the
3s campaign timeout.

## As built — P2 (2026-08-25)

The trait, its hole, and the three blessed implementations, as they
landed (`c3be365f`'s successor commit). §6/§7 hold; the deviations are
listed last.

**The hole, in the type system** (`typ/mod.rs`, `typ/contains.rs`,
`typ/print.rs`, `expr/parser/typexp.rs`): two `Type` forms — `App(ctor,
arg)` (`self<'a>`, `'c<i64>`; a constructor variable applied to an
element) and `Hole` (written `'_`, Eric's spelling: it reads as the
elided parameter, it is explicit in a head, and it round-trips when a
bound constructor prints). `Type::app` fills when the constructor is
concrete; `decompose` is the syntactic split on the outermost form
(Array → element, Map → value, a reference by NAME with its last
parameter, an abstract likewise); `fill_hole` is its inverse. In
`contains`: `(App, Ref)` before the reference-expansion arm (a name is
decomposed, never expanded), the general `(App, _)`/`(_, App)` arms at
the END of the dispatch so ⊥, `Any` and an open cell keep theirs (⊥
fits; the cell binds to the application form), `app_contains` fills a
bound constructor or decomposes the other side, and a cell bound to an
application whose constructor has since bound is read as the filled
type (`app_behind`) before a reference on the other side expands. The
constructor variable binds to the constructor BY NAME (`bind_ctor`) —
through the general walk it met the reference-expansion arm and bound
to the list's union body, and every later lookup keys on the name.
One recovery for receivers that lost their name: a cell bound through
`contains` holds a typedef's EXPANSION (the documented rule), which
decomposes to nothing; `app_split_for` unifies each registered head of
the constructor variable's trait bounds, filled with a fresh element,
against the receiver, and the head that contains it and thereby binds
the element is the constructor. `Hole` is a leaf equal only to itself
and never bound to; the two match sites the compiler could not guide
(`kernel_abi`/emit) refuse through their catch-alls, so an application
that ever reached codegen would de-fuse, never miscompile.

**Constructor traits** (`env.rs`, `node/traits.rs`, `node/callsite.rs`):
`TraitDef.hole` — a trait applies `self` in every signature or in none
(mixed is an error at `deftrait`). An impl head must have exactly one
hole, as the last parameter of its outermost form; a reference head
(`List<'_>`) is owned by the package that defines the name (never
expanded — its body is a union, an abstract's a box), a builtin
constructor by the trait's package only. `find_impl` matches
constructors (`Array<'_>` structurally, references by name).
`resolve_trait_call` decomposes the resolved self type instead of
expanding it; a receiver that does not decompose (a union of arrays)
is refused as "not a type constructor". `trait_contains` on a
reference under a constructor trait consults `find_impl` by name. The
sugar `|c: Collection|` is `'c: Collection, c: 'c<'e>`
(`Type::trait_param`, used by both minting sites — `Lambda::compile`
had a duplicate of `rewrite_trait_args`' arm). `'_` anywhere but an
impl head is refused at every annotation site.

**The stdlib**: `trait Collection` in core's `mod.gxi` with the §6
signatures (each callback `throws 'e`) and defaults for `map`,
`filter`, `find`, `find_map`, `len`; `impl Collection for Array<'_>`
and `impl<'k> Collection for Map<'k, '_>` in core (marker bodies —
`'array_fold` etc. are compiler intrinsics, no package dependency; the
Map impl folds/filters/maps VALUES over the pair intrinsics and
`flat_map`s by union), `impl Collection for List<'_>` in the list
package. `core_array_len`, `core_map_len`, `core_map_union` are core
builtins (a builtin's name carries its registering package; the array
and map packages bind their `len` to them, and map exposes `union`).
`list::to_array_rev` (one walk) for accumulator finishes.

**Found and fixed in the typechecker** (all pre-existing, all pinned by
`lang/collection.rs` through the trait's defaults):

1. A call site PRE-UNIFIED a declared parameter type with an
   argument's type before typechecking the argument (`callsite.rs`,
   both sites) — right for pushing parameter types into an unannotated
   callback, but it also bound the callback's still-open RETURN cell to
   the declared return on first contact (`Option<'b2>` ⊇ open cell
   bound the cell to the whole option, and `'b2` could never equal the
   generic wrapper's `'b`). `Type::pre_unify_arg` /
   `FnType::pre_unify_params`: parameter positions only; the return is
   judged after the body types. A generic `filter_map` wrapper
   (`|xs, f| array::filter_map(xs, |x| f(x))`) was uncompilable before
   this.
2. `FnType::constrain_known` recorded a cell bound to a bare tvar (an
   alias CHAIN, `TVar::alias`'s fallback) as a fact — a fresh unbound
   conjunct that every later occurs check read as a cycle ("cannot
   infer a finite type"). It follows the chain now.
3. `Select::typecheck0`'s wildcard narrowing (the walk that teaches
   `select n { 0 => .. }` its scrutinee is `i64`) ran on UNION
   scrutinees and bound the open member to an arm's type test —
   `select acc { null as _ => .., found => .. }` over `[e, null]` with
   `e` open bound `e := null` and reported the second arm dead. A free
   union member stays free: the walk skips union scrutinees.
4. The TVar×TVar fast path in `contains_dispatch` compared tvar
   identity (`addr`/`id`), not CELL identity — two vars already
   aliased into one cell fell through to the cycle guard, whose walk
   reaches "itself" through the shared cell, and both were poisoned
   `cycle_refused` ("cannot infer a finite type" at settle if the cell
   ends unbound). Latent forever; fix 1 exposed it by deferring the
   return aliasing, so the rigid re-walk of a call site met the
   signature's `'a` and a callback's return as a same-cell pair
   (`array::init(4, |i| f("x"))` with `f`'s return still open — the
   data_table_dashboard example, caught by `examples_compile`). A
   same-cell pair is already unified: the fast path now answers true
   (`TVar::same_cell`). Pinned by
   `lang/functions.rs::open_return_callee_in_callback`.

**Deviations from §7.** The hole is spelled `'_` rather than omitted.
The `Ref → last param` decomposition is exact for a value whose type
is still the name, and for one whose cell holds the expansion goes
through the recovery above (the alternative — binding cells to names
instead of expansions — is the right long-term shape, name-compressed
inference everywhere, and a separate change). Union receivers fail at
the argument check with a type mismatch rather than the dedicated
message. Fusion coverage, not correctness: the Array intrinsic reached
THROUGH the trait dispatcher interprets (the direct `array::map`
fuses), as does a trait default over an abstract wrapper and a
constructor-variable parameter's instance — ASPIRE annotations in the
fixtures, P3's list.

**Still open, deliberately.** A named type variable in a lambda BODY
annotation (`let init: Option<'a> = null`) is a fresh cell, not the
enclosing signature's `'a` — the typed-seed defaults work because the
fresh cell unifies later, but `w3`-shaped code (annotating a nested
callback's parameter with the outer `'a`) does not; Rust's rule (a
body name means the definition's variable) is the candidate, and it
needs the instance-time name→type map. Eric's call.

Pins: `lang/collection.rs` (array/list/map through the trait, the
generic parameter, the newtype with defaults, the user cons list with
annotated module-level recursions, `find_map` default, and five
rejections: union receiver, non-constructor head, filled head, `'_`
outside a head, mixed `self`).

## Found during P2b (2026-08-25)

The P2b differential corpus — bench/collection/ (timed, intrinsic vs
trait dispatch vs trait-default body vs hand-written Graphix recursion)
plus the `collection_bodies_*` value-agreement fixtures in
lang/collection.rs — found four issues before any timing ran: the
fixture face crashed the test process on its first run.

1. **`--check` never runs `analysis::analyze`** (MUST FIX — Eric,
   2026-08-25). **FIXED — verified 2026-08-26:** `compile_stmt` (the
   per-statement entry `check_inner` drives) carries the analyze call
   itself now (graphix-compiler/src/lib.rs, "Runs ALWAYS"), and
   `compile()` is a thin wrapper over it — one compile channel, not
   three. Witnesses re-run green: `--check` rejects a false
   `#[tail_recursive]` with analysis.rs's own message, and
   `GXDBG_EFFECT=1 --check` prints 84 lines on soak-dash. Pinned by
   `graphix-shell/tests/check_runs_analyze.rs`. (Original record: the
   analyze call sat only at `compile()`'s tail, past where
   `check_inner` stopped — the def assertions were verified only at
   load, and `GXDBG_EFFECT=1 --check` printed 0 lines on a program
   that prints 1600 under load.)

2. **Runtime type tests recursed unguarded through VALUE depth**
   (FIXED same day: `Type::is_a_int` runs under `ensure_sufficient`,
   typ/cast.rs). `arm_match` (node/pattern.rs) runs each arm's
   inferred type predicate through `is_a`; the Variant arm recurses
   into payloads, so on a recursive ADT — `` `Cons('a, List<'a>) `` —
   ONE arm consult walks the entire remaining chain on native stack. A
   Graphix-recursion fold over a 1000-element list aborted the process
   in release (~200 in the debug test harness, which is how the
   fixture found it); the intrinsic never trips it (no per-cons deep
   predicate). The guard makes it correct (memory-bounded). The COST
   stands: each consult is O(remaining), so a recursive list traversal
   is O(n²) — bench/collection's lfold_rec row is the number. The
   kernel twin tests tag+arity in O(1) (the aug-07 representation+arity
   rule), an engine asymmetry. OPEN: give the interp arm test the same
   discrimination when tag+arity uniquely determine the member within
   the scrutinee's STATIC union — the deep payload walk adds nothing
   there, the static type already proves it. BUILT same day:
   Type::shallow_discriminant (typ/cast.rs) — at the select's first
   consult each arm's INFERRED predicate is sealed against the
   settled scrutinee type: the scrutinee's members are flattened
   (Refs resolved seen-guarded, bound tvars deref'd; Any/unbound/
   cycle => keep the deep walk), and a payload-carrying predicate
   shape (variant/tuple/struct/array/map/error) is replaced by its
   outermost form (payloads => Any, plus is_a short-circuits for
   Array<Any>/Map<Any,Any>) iff exactly ONE member overlaps its
   runtime footprint — same tag+arity twins, tuple-vs-array
   (e86d18c1's class) and any-length arrays beside other
   Value::Array members all refuse and keep the deep walk, as do
   explicit (x as T) predicates (the user's claim stays strict).
   Consults drop to O(arity); the list-recursion curve went linear
   (debug: 5.1s -> 0.98s at 1k, x2 per doubling after; release 49.4s -> 0.41s at 8k). Sealing is
   lazy (no new compile pass) and sits on PatternNode (seal_shallow,
   sealed at Select::update's first pass); GXDBG_SHALLOW=1 prints
   each seal decision. Pinned by shallow_ambiguous_same_tag_union /
   shallow_mixed_union_dispatch (lang/select.rs).

3. **`structural_tail_loop`'s formal-kind gate denies the tail driver
   by a kernel-ABI condition — on BOTH backends** (FIXED 2026-08-25
   for the wide face — loop-INVARIANT formals; BUILT record below).
   Formals must freeze to Prim/Array/Tuple/Struct
   (fusion/lowering.rs); String, Variant, Nullable, Map and
   Value-shaped (every recursive ADT) formals fail, and since the gate
   deliberately keeps the backends in lockstep, both run a tail-SHAPED
   recursion as native per-level recursion: no constant space, an
   activation per level. `|l: List<..>|` traversals, `|s: string|`
   accumulators, `[T, null]` state loops, Map builders — common
   shapes. TAILDBG shows 0 passes; the i64-formal twin shows one per
   iteration. Options: split the seam (the interp loops whatever
   analysis blesses — tail structure + Sync + stateless — since its
   frame rebind needs no ABI freeze; the kernel keeps its subset and
   native-recurses with the P1 trampoline otherwise), or widen the
   kernel's loopable kinds. Collapse is unobservable for stateless
   bodies (Ruling 2), so what the lockstep protects here is space
   parity, not semantics.
   BUILT same evening — the invariant-formals cut: a formal every
   self-call passes through UNCHANGED (the arg is the formal's own
   Ref — `invariant_formals`, fusion/lowering.rs) is never rebound,
   so the kind gate applies only to loop-CARRIED formals, in the ONE
   shared predicate (both engines still agree on which lambdas
   loop). An invariant fn-typed formal drops out of the kernel
   signature entirely (`KernelSig::skipped_args` — the fn-capture
   precedent: its body uses are statically-resolved calls; callers
   skip the arg, and `emit_self_tail_call` rebinds by explicit slot
   index, skipping invariant slots). Two prerequisites fell out:
   (a) the kernel cache key gained a RESOLUTION FINGERPRINT
   (`FnResolutions` — per static lambda call site the callee's
   LambdaId plus each forwarded fn-typed arg's resolution): a kernel
   BAKES its instance's callback resolutions as CLIF calls, and two
   sites agreeing on every TYPE can still resolve different
   callbacks — the latent instance-aliasing hazard the skip would
   have made live (pinned by fn_formal_two_callbacks; latent for fn
   CAPTURES already, unreachable only because fn formals killed
   every such build). (b) The premat wiring's SYNTHETIC Refs
   (genn::reference — NOP spec, no name) resolve by BindId alone in
   the emitter (`JitEnv::lookup_id`) — which also closed
   missed-fusion item 1 (the rec callee inside a fold callback;
   fold_callback_name_collision upgraded to Jit by the harness's own
   demand). One guard: a COLLECTION-bodied lambda refuses
   `build_lambda_kernel` explicitly — its sites inline the scaffold
   via the Apply hook, and the fn-formal freeze failure was the
   accidental guard keeping the marker's undefinable kernel out of
   region builds (the fold/filter jit probes caught it). Measured
   (release): `fold_rec` 8.40 s -> 16.3 ms JIT / 9.34 s -> 0.26 s
   node-walk (the interp tail loop applies too; the ~60x left to the
   intrinsic is per-iteration DynCall/call overhead, not
   activations); `map_push` collapses on both engines (copy-bound).
   Residue: loop-CARRIED String/Variant/Nullable/Value formals keep
   native per-level recursion (`lfold_rec` — widening the rebind
   kinds is the remaining cut), and fn-formal FORWARDING still
   interprets (fn_formal_forwarded, ASPIRE — the wrapper-premat
   residue). Pins: fn_invariant_tail_loop,
   string_invariant_tail_loop, fn_formal_two_callbacks,
   fn_formal_rebound, fn_formal_forwarded (lang/functions.rs).

4. **`#[tail_recursive]` passes where the loop does not collapse**
   (OPEN — follows 3). The assert checks
   `RecursionKind::TailRecursive` + `lambda_is_stateless`
   (analysis.rs `check_def_assertions`); the operational flag
   additionally requires the formal-kind gate. The book sells the
   attribute as the constant-space guarantee; a List-formal function
   passes the assert and native-recurses. Either the assert consults
   the same seam, or fixing 3 interp-side closes the gap and the
   kernel's native+trampoline residue gets documented honestly.
   NARROWED 2026-08-25 by the invariant-formals cut: fn/String-
   invariant shapes genuinely loop now; the remaining
   assert-vs-collapse gap is loop-CARRIED String/Variant/Value
   formals (List traversals).

## As measured — P2b first cut (2026-08-25)

bench/collection/ (19 self-timed benches, both engines, release,
best-of-3; full table + notes in its README). The headline numbers:

- Intrinsic vs trait dispatch: `array::fold` 0.28 ms vs
  `Collection::fold` 2.21 s at 100k (~7800x) — dispatch interpreted
  (P3's price tag). FIXED same day: the typecheck1 driver returned
  early after `resolve_trait_call`, skipping the HOF
  pre-materialization every ordinary static call falls into, so the
  impl instance's callback never registered and the collection body
  couldn't inline-emit. Now the block is `CallSite::premat_fn_args`,
  called from both paths: `fold_trait` is at parity (0.31 ms) and the
  `|c: Collection|` generic path fuses too (both ASPIRE fixtures
  upgraded to Jit by the harness's own demand). Residue: a Graphix
  WRAPPER impl body (Map's values-fold over `fold_pairs`) still
  interprets — the nested derived callback's call to `f` doesn't
  resolve through the inner collection site. That wrapper shape is
  every trait DEFAULT body, so it is the named next target on the
  road to deleting intrinsics.
- Intrinsic vs Graphix recursion: the callback FORMAL (fn-typed) fails
  `structural_tail_loop`'s kind gate (finding 3), so every
  stdlib-shaped body (`|a, f, i, acc|`) native-recurses per level —
  `fold_rec` 8.4 s vs 0.28 ms at 100k. On List the quadratic arm
  consult (finding 2) stacked on top: 9.6 s at 4k vs 0.15 ms (~62,000x;
  measured curve 0.55/1.97/9.6/49.4 s at 1k/2k/4k/8k — clean O(n^2)).
  FIXED same day (shallow discriminators): the curve is linear
  (0.050/0.096/0.201/0.414 s — 119x at 8k), lfold_rec is 0.29 s, and
  the residual ~1900x to the intrinsic is finding 3's per-level
  activation. FIXED same evening for the wide face (the
  invariant-formals cut, finding 3): `fold_rec` 8.4 s -> 16.3 ms JIT
  / 0.26 s interp — both engines loop; the ~60x left to the
  intrinsic is per-iteration DynCall/call overhead. `lfold_rec`'s
  loop-CARRIED Value formal keeps the per-level activation (the
  remaining cut).
- Trait DEFAULT bodies over the intrinsics: filter's fuses at 1.9x the
  intrinsic (fine); map's DE-FUSES (the bare-`'b`-as-`Option<'b>`
  callback widening — a fusion-coverage bug; fixed, the default would
  be a 2 ms row); find's is 4 orders off (unfused double select);
  flat_map's pays O(n^2) concat AND interprets. `map` via `init` is
  the one parity derivation (2.6 ms vs 2.5 ms), Array-only.
  The widening FIXED same evening: a TOTAL filter_map callback
  (frozen return provably null-free — `frozen_may_be_null`,
  conservative) can never produce the `Null` the intrinsic drops, so
  `emit_filter_map_kind` routes it to the MAP loop — filter_map with
  a total callback IS map. `map_fmshape` 4.21 s -> 2.3 ms, parity
  with `map_intr`; Array `map` now has TWO parity derivations and is
  the first deletable CANDIDATE (pinned:
  filter_map_total_callback, lang/collection.rs).

VERDICT (per the phase question "what stays"): every intrinsic stays.
The deletion question is not answerable until the WRAPPER-premat
residue (trait-default bodies) and the carried-Value rebind widening
land — re-run the corpus after each (P3, finding 3's invariant face,
and the map-default widening all landed same day; Array `map` is the
first deletable candidate, two parity derivations). The measurement's real product
this round was the four findings above plus the widening bug.
