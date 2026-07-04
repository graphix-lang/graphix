# TVar cell constraints

Status: PROPOSED (2026-07-02, Eric + Claude). Not yet built. Slotted after
the fuzzer-v2 phase-2.0 JIT bottom/firing fixes unless the unannotated-gate
fusion story moves it up.

## The problem

A type variable cell today is `TVarInnerInner { id, frozen, typ:
Option<Type> }` — a name and an optional binding, nothing else. The only
constraint machinery in the language lives on `FnType.constraints:
Vec<(TVar, Type)>` (the explicit `'a: Number |x: 'a| -> 'a` form), and it
is checked post-hoc at call sites (`CallSite::typecheck0`, after argument
unification: "whatever `'a` ended up bound to must be contained by the
constraint"). The constraint is a property of the *function signature*,
not of the *variable*.

So a node that learns something partial about a type — arith knowing its
operands are numbers, `never()` knowing nothing at all — has exactly two
tools: bind the cell NOW, or leave it unconstrained. Arith picks the
former (`node/op.rs` `arith_op!` typecheck0: "init types that aren't known
by now to Number" — `contains(Number, operand)` binds an unbound operand
cell to the wide Number PRIMITIVE SET). That choice is load-bearing: the
arith result-type computation (the `ut` table) bails "type must be known"
on an unbound operand, so typecheck0 force-feeds it.

The wide binding is where three real problems come from, all found or
sharpened by the fuzzer-v2 work:

1. **Bare lambdas are "monomorphic wide"** (fuzz/triage-fuzzer-v2/
   typecheck_observations.md #3). `let f = |a| a + a` infers
   `fn(a: Number) -> Number` with the wide set BAKED into the cells; the
   call site's `reset_tvars` (callsite.rs — per-site freshening that
   already works) has no tvars left to freshen. Every call passes
   positionally (`Number ⊇ f64`), but the result is Number-wide, so
   `let x: f64 = f(f64:1.0)` rejects — infectiously, through any op that
   touches the result. The explicit `'a: Number` form works precisely
   because its return IS the param cell.
2. **The `never()` wide-Number pollution de-fuses unannotated gates**
   (the `gated_scalar_unannotated` ASPIRE fixture). `never()`'s tvar
   flows into arith, gets bound to the wide set, and the wide set
   genuinely denotes multiple register classes — no freeze can soundly
   pick one, so the whole region silently node-walks. Annotating one
   `let` fixes it, which is exactly the kind of unpredictable
   performance cliff graphix exists to avoid.
3. **Bad error locality.** The wide set is legal at the site that bakes
   it and only collides with reality later, so the reject fires
   somewhere downstream of the actual mistake, printing a wide-set
   mismatch instead of the constraint that was violated.

## The design

Give the CELL a constraint:

```rust
pub struct TVarInnerInner {
    pub(crate) id: TVarId,
    pub(crate) frozen: bool,
    pub(crate) typ: Arc<RwLock<Option<Type>>>,
    /// Everything this variable is ever bound to must be contained by
    /// this type. None = unconstrained.
    pub(crate) constraint: Option<Type>,
}
```

with ONE enforcement point: the bind sites in the contains/unify walk.

- **Binding** an unbound cell to `t` requires
  `constraint.check_contains(t)`; violation is a type error AT THAT SITE
  ("'a: Number cannot be bound to string").
- **Aliasing** two unbound cells intersects their constraints; an empty
  intersection is a type error.
- **Arith** (and any future partially-informed node) ADDS the Number
  constraint to an unbound operand cell instead of binding it wide. The
  information "the user did arithmetic on it, so it's a number" is
  preserved exactly, without foreclosing which number.
- **Same-cell result aliasing**: for `a + a` (both operands the same
  cell), same-type arithmetic is type-preserving, so the result type IS
  the operand cell. `|a| a + a` then infers literally
  `fn(a: 'a) -> 'a, 'a: Number` — identical to the explicit form — and
  `reset_tvars` does the per-site instantiation it already knows how to
  do (constraints copy to the fresh cells).
- **The explicit `'a: Number` syntax becomes sugar** that seeds the cell
  constraint at lambda compile time. `FnType.constraints` stays for
  printing/interface files (and the callsite post-check can remain as a
  belt-and-suspenders assert), but the semantics move to the cells.

### Deferred arith result typing (the typecheck1 piece)

With operands constrained-but-unbound, arith's `ut` table can't run at
typecheck0 — and must no longer bail. The node instead defers: its result
is the operand cell (same-cell case) or a fresh Number-constrained cell
(distinct-operand case), and **typecheck1 re-runs the `ut` table** once
bindings have settled, erroring "type must be known" only if the operands
are still undetermined then. typecheck1 already exists as the settle-up
pass; this is a natural residence. This is the sound, cheap kernel of
"re-do inference later" — NOT per-call-site re-inference of lambda bodies
(true polymorphic instantiation), which would duplicate typechecking work
per site and is explicitly out of scope.

### The two-distinct-operands question (`|a, b| a + b`)

A subset constraint cannot express the relation `result = arith_join(a,
b)`. Options considered:

- **(i) Unify the operand cells.** Simple, makes two-param bare lambdas
  fully polymorphic — but a semantic TIGHTENING: today `f(i64:1,
  f64:2.0)` passes (both params wide); unified cells would reject
  mixed-type calls.
- **(ii) Fresh constrained result cell + typecheck1 consistency check.**
  The result is a fresh `'r: Number` that use sites may narrow (e.g. an
  annotation binds `'r := f64`); typecheck1's deferred `ut` run then
  verifies the narrowed result contains the join of what the operands
  became. Keeps today's mixed-operand acceptance; the result narrows
  when the program pins it.
- **(iii) Keep the result wide for the multi-cell case only.** Smallest
  change; two-param bare lambdas keep the current annotation-reject
  behavior.

RECOMMENDATION: (ii). It is strictly more expressive than today, changes
no currently-accepted program, and reuses the deferred-`ut` machinery the
same-cell case already needs. (i) can be revisited later as a deliberate
language decision if mixed-type arith turns out to be rare in practice.

## What gets better

- `let f = |a| a + a; let x: f64 = f(f64:1.0)` compiles (observation #3).
- **Optional-arg builtins with constrained-tvar returns fuse and
  annotate** (found 2026-07-03): `rand::rand(#clock:1)` — the signature
  `fn<'a: [Int, Float]>(?#start:'a, ?#end:'a, ...) -> 'a` with both
  optional args omitted leaves `'a` unbound, so the region silently
  node-walks (unbound-tvar freeze skip; the
  `stdlib/graphix-package-rand` `rand_float_default` fixture is the
  ASPIRE pin), and `let r: f64 = rand::rand(#clock:1)` REJECTS
  outright — the same annotated-result class as observation #3. With
  cell constraints + deferred inference, the annotation binds the cell
  and the defaults' types settle it otherwise.
- The `never()` gate idiom fuses without annotation: the gate's cell
  stays narrowable, the freeze sees one register class, and
  `gated_scalar_unannotated` flips from ASPIRE/None to Jit — a direct
  predictable-fusion win. The `freeze_for_abi_normalized` third rung
  (resolve_tvars for the never-Set pollution) may also simplify.
- Constraint violations report at the violating site with the
  constraint named.

## Ripple points (implementation checklist)

- `typ/contains.rs`: every site that binds an unbound cell gains the
  constraint check; cell-aliasing sites gain constraint intersection.
  This is the heart and the main review surface.
- `typ/tvar.rs` + the copying walks: `reset_tvars`, `alias_tvars`,
  `resolve_tvars`, `copy`/`normalize` must carry constraints to fresh
  cells. `would_cycle` should consider constraint types.
- `node/op.rs` `arith_op!` (and `Neg`, checked ops): constrain instead
  of bind; defer `ut` to typecheck1. Audit other `contains(Number, …)`
  "init to" sites (comparisons init operands similarly? verify).
- Printer: `'a: Number` display already exists for FnType constraints;
  cells print their constraint when unbound.
- Pack/serialization: verify whether TVars cross the Pack boundary
  (packed-AST pattern type predicates) and whether constraints must
  serialize.
- `frozen` interaction: freezing a constrained-unbound cell — decide
  whether freeze binds to the constraint (current wide behavior as the
  terminal fallback) or refuses.
- Fusion: `freeze_for_abi` treats a constrained-unbound cell as
  unfreezable (as today for unbound); the win comes from cells being
  BOUND narrow more often, not from freezing constraints.

## Testing

- Fixtures: the three bare-lambda probes from observation #3 (single
  site annotated, two-site unannotated, infectious-op variant), a
  constraint-violation error-message fixture, mixed-operand `a + b`
  acceptance (pins decision (ii)), and flipping
  `gated_scalar_unannotated` to `FuseExpect::Jit`.
- The fuzzer is the regression net: gen-check's compile rate (bare
  lambdas are generated organically), the differential campaign for
  value semantics, and the `run!` corpus + FUSE audit for fusion drift.

---

# Holistic execution plan (2026-07-04, Eric-approved direction)

Eric's bar: "go through the whole typecheck process and rethink every
part of it in the new frame … when it's done, it should be as if we
always had tvar constraints." This section is the full plan; the design
above stands, and two soak-week discoveries sharpen it:

- **Observation 4 is the reset semantics itself.** `Type::reset_tvars`
  replaces EVERY `TVar` leaf with `TVar::empty_named(..)` — bindings
  included. A def-time body unification (`'a: Number |x:'a| -> 'a
  f64:0.` binds `'a := f64`) is ERASED at each call site, so `f(i64:3)`
  type-checks and the static type lies about the runtime value (the JIT
  marshal-panic class; the runtime now survives it via placeholders, but
  the acceptance is unsound). In the new frame the operation is
  **instantiate, not reset**: a BOUND cell is a settled fact and stays;
  an unbound-constrained cell freshens WITH its constraint. The name
  `reset_tvars` should die with the old semantics.
- **The soak corpus is the acceptance net**: 11 recorded findings are
  this class (generate 004/012/013/020/024/040/046/067, fuzz
  003/026/032). Post-fix they must either AGREE or reject in BOTH modes
  at the violating site.

## Phases

**A — the cell.** `constraint: Option<Type>` on `TVarInnerInner`; bind
sites in `typ/contains.rs` check it; alias sites intersect (empty
intersection = error at site); every copying walk carries it
(`alias_tvars`, `resolve_tvars`, `copy`, `normalize`, the new
`instantiate`); `would_cycle` considers constraint types; printer shows
`'a: C` for unbound-constrained cells. Unit tests at the typ layer.

**B — constraint producers, re-derived.** The explicit `'a: T |…|` form
seeds cells at lambda compile (FnType.constraints becomes a DERIVED
display/interface artifact; the callsite post-hoc check downgrades to a
debug assert). Arith/cmp/Neg/checked ops CONSTRAIN unbound operands
instead of binding wide; the `ut` result table defers to typecheck1
(same-cell aliasing for `a + a`; decision (ii) fresh constrained result
for `a + b`). Audit every other `contains(<wide set>, …)` init-to site
(comparisons, builtins' .gx signatures, math package).

**C — the lambda/callsite protocol.** Def-time inference unifies the
body against param/return cells (bindings that result are FACTS —
obs-4 rejects at the call site with the constraint named). Call-site
`reset_tvars` → `instantiate` (fresh unbound-constrained cells only).
Re-examine: `bind_as`, the two-phase typecheck0/1 division (typecheck1
is the settle-up pass and gains the deferred `ut`), lambda_ids /
static-resolution interplay, and fusion's `build_lambda_kernel` cache
key + refuse-on-disagreement (should become UNREACHABLE for sound
programs once instantiation is sound — keep as an assert).

**D — every other typecheck feature, rethought.**
- `select` arm unification: the `any_as_tvar` view exists to make `Any`
  leaves unify-through-able; an unconstrained fresh cell IS that
  semantics — evaluate folding the view into the constraint machinery
  (one mechanism, not two). Pattern `type_predicate`s serve FOUR
  consumers (unification/exhaustiveness/dead-arm/dispatch) — re-check
  each against constrained cells.
- `never()`/`empty_tvar` flows: stay unconstrained; gates narrow at
  use; `gated_scalar_unannotated` and `rand_float_default` flip to Jit.
  Re-evaluate `freeze_for_abi_normalized`'s third rung (added for
  never-Set pollution — may be removable).
- typedefs/abstract types: `deftype` params already carry
  `Option<Type>` constraints — unify the representation with cell
  constraints (one constraint concept everywhere).
- `frozen` × constraint: freezing a constrained-unbound cell BINDS to
  the constraint iff it denotes a single register class, else refuses
  (the terminal fallback mirrors today's wide behavior without lying).
- Pack/serialization: constraints must round-trip wherever TVars do
  (packed-AST pattern predicates).
- Error messages: violations name the constraint and the site; add
  fixtures asserting message shape.

**E — acceptance + the big test.** Obs-3 probes compile; the 11 corpus
findings agree-or-both-reject; `gated_scalar_unannotated` +
`rand_float_default` → `FuseExpect::Jit`; mixed-operand `a + b` pin
(decision ii); constraint-violation message fixtures; full gates
(1664+, workspace FUSE audit zero drift, regress 55+); then the
combined overnight soak (all three campaigns) that gates the whole
week's work.

## Phase B as built (2026-07-04) — refinements over the plan above

Everything below is LANDED and gate-verified (workspace 1664+ green,
FUSE audit zero drift, regress 56/56, the 3 fuzz-corpus typecheck
findings now AGREE). Where it deviates from the sections above, this
section wins.

- **The conjunction, not `Option<Type>`:** the cell carries
  `constraints: SmallVec<[Type; 1]>` — a CONJUNCTION every future
  binding must satisfy. Alias-merge is INFALLIBLE (no Env at alias
  sites); an unsatisfiable conjunction errors at SETTLE time instead,
  via the witness rule: settle binds an unbound-constrained cell to the
  narrowest conjunct every other conjunct contains
  (`TVar::settle`, typ/contains.rs), and "no witness" is the
  "unsatisfiable constraints on 'a: i64 & string" error.
- **Instantiation preserves cell topology.** `reset_tvars` freshens
  keyed by CELL IDENTITY (one fresh cell per source cell), not by name
  — `|a| a + a` shares one cell between `'a` and the rtype and every
  instance now does too. The FnType constraints list comes through the
  same map, so list entries start out sharing their arg/rtype cells
  (they previously froze a separate `empty_named` cell and ORPHANED —
  the root of the first unsound-accept found while building this).
- **Derived-vs-arg-reachable settle split (replaces the (ii)/(iii)
  deliberation).** Ground truth discovered: per-site lambda BODIES are
  recompiled and re-typechecked per call site (`setup_bind` →
  `rf.typecheck0`, `resolve_static` drives `typecheck1`) but their
  errors are SWALLOWED — fusion enablers, not acceptance gates. So
  decision (ii)'s per-site `ut` verify has no unswallowed home, and a
  freely-narrowable derived result cell would be UNSOUND
  (`let r: f64 = f2(i64:1, i64:2)` accepted, body computes i64).
  Resolution: at `CallSite::typecheck0`-end, constrained cells
  reachable from the rtype/throws but NOT from any arg (derived
  results) settle eagerly — before an annotation could narrow them;
  arg-reachable cells stay open through typecheck0 (annotations narrow
  them and the actual args enforce the narrowing — obs-3, rand). The
  typecheck1 terminal settles whatever remains, walking the LIVE ftype
  (never the stored list — orphan hazard). Because the settle witness
  is the conjunction's narrowest member, a derived cell whose def-time
  fact was `i64` settles to `i64`, not wide — `g(true) + g(false)`
  stays precise and fuses.
- **Arith/Neg constrain-don't-bind** as designed: unbound tvar operands
  get a `Primitive(Typ::number())` conjunct (exactly the old step-2
  bind set — no acceptance drift; the wide duration/datetime set stays
  a known-operand CHECK only), same-cell operands alias the result to
  the operand cell, distinct-cell deferred results get a fresh
  number-constrained cell, and the `ut` table re-runs in typecheck1
  after settling the operand cells (`typecheck_tail`).
- **`constrain_known` seeds the cell from the def-time binding even for
  explicitly-listed names** — the obs-4 fix: `'a: Number |x:'a| -> 'a
  f64:0.` gets conjuncts `[Number, f64]`, so `f(i64:3)` now rejects AT
  THE ARG with the constraint named. Unbound single-conjunct cells are
  also pushed to the list for display/interfaces (multi-conjunct cells
  stay unlisted — one type per list slot, and an approximation could
  leak into interface matching).
- **Deleted eager wide-binders:** the `CallSite::typecheck0` post-hoc
  list loop and `GXLambda::typecheck0`'s per-instance list loop are
  gone; bind sites (cell checks in contains) plus the two settle points
  replace them.
- **Known seams / follow-ups:**
  - `rand_float_default` and `gated_scalar_unannotated` did NOT flip to
    Jit yet. rand: default exprs (`= 0.0`) compile per-site inside
    `try_static_resolve`, which runs AFTER the typecheck1 terminal
    settle — the settle binds `'a := [Int, Float]` first. Fixing this
    is an ordering question (settle after static resolution, or default
    TYPES participating in typecheck0) — phase C territory. gated: the
    never()-gate narrowing is the phase D item.
  - Dynamically-dispatched calls (no static resolution) check only the
    signature; a per-site body inconsistency there de-fuses/lazy-binds
    rather than rejects — same class as the old per-site swallowing,
    now confined to dynamic dispatch. The fuzzer's dyncall coverage
    exercises it.
  - Bonus coverage win: `lazy_three_level` (three-level bare-lambda
    chain) flipped None → Jit.

## Expected simplification wins (verify, don't assume)

Candidates for the "unexpected wins" Eric predicts: retiring
`any_as_tvar` (one unification mechanism), removing the freeze third
rung, deleting the callsite post-hoc constraint check, simplifying
`static_resolve`'s never/wrapper special cases, and making the fusion
mono-cache refusal an assert. Each retirement gets its own commit with
the reasoning, or a note in this doc for why it must stay.
