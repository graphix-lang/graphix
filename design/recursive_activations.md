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

**Retention vs deletion** (open, §11): MapQ DELETES excess slots on
shrink; recursion RETAINS them asleep (the retention ruling: "let the
user run out of memory"). A deque that grows to 100k and shrinks to
10 keeps 100k sleeping instances. The recommendation is to keep the
ruling (retain), measure, and treat reclamation of long-asleep
activations as a runtime GC concern if it ever bites — not a
semantics one, since sleep is pause.

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
  resurrection is DEAD: the constant it would remove is 8µs. DECISION
  PENDING (Eric, 2026-08-25: profile first, maybe live with it).
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
