# clone_rebind test plan — hunting the silent bugs

`clone_rebind` (the per-slot graph cloner behind MapQ HOF dispatch) is
green across the suite, but the suite mostly proves *value correctness on
the tested paths*. The three things that nag about the design are exactly
the classes a passing test can hide. This is the plan to flush them out.
Tests exist to **find bugs**, not pass — a failure here is the goal.

## The nags these target

1. **`alias_variable` env pollution (recompile-default).** To make the
   recompile-from-spec path capture-safe, `Update::clone_rebind`'s default
   *mutates* the clone scope's name map (`env.alias_variable(scope, name,
   id)`) for each unresolved external ref. A clone should be read-only wrt
   shared env state; this isn't, and the alias persists. It's only hit for
   the env/reference nodes (`TryCatch`, `Module`, `ByRef`/`Deref`,
   `Sample`, `ConnectDeref`, `Lambda`, `Use`/`TypeDef`) — which have **zero**
   capture coverage today. Smelliest mechanism, least exercised.

2. **clone↔delete binding symmetry / `env.by_id` growth (the leak).**
   Every `clone_rebind` re-mints bindings via `bind_variable`; the
   separate-clone does it twice (template + per slot). Is every minted
   binding unbound on teardown? In particular MapQ's own per-slot
   `bind_variable("x")` — the slot's `delete` unrefs the pred's *refs* but
   nothing obviously removes the `"x"` entry from `by_id`. Silent
   unbounded growth in a long-lived reactive program is the scariest
   failure mode because nothing turns red.

3. **`compile()` side effects at runtime.** The recompile-default calls the
   full compiler per slot for env nodes — with whatever registration side
   effects it carries (`builtin_bindings`, catch handlers, references). The
   structural clones sidestep this; the recompile path embraces it,
   uncharacterized.

A fourth, structural, gap: the ~30 structural clone impls are only
exercised *in aggregate* by the one `x*k` fixture. No per-node-shape
coverage through the clone path.

## The four efforts (priority order)

### 1. Clone-equivalence matrix — START HERE (no infra)

One fixture per residue node shape, each forced through MapQ's clone path
(async callback — a `counter <- x` makes the HOF async so MapQ actually
runs and the separate-clone clones the *pristine* body through the
structural impls) **and** each capturing an outer binding `k`. Assert the
per-element result array. If any structural clone has a wrong field, a
dropped child, or a lost capture, the value goes wrong and the test fails.

Coverage targets: `Add`/`Mul` (arith+capture), `Gt`/`Eq` (comparison),
`And`/`Or`/`Not`, `Select` (single-arm + two arms binding the same name —
the transient-name-map case), `Tuple`/`Struct`/`Variant`/`Array`/`Map`
producers carrying a capture, `TupleRef`/`StructRef`/`ArrayRef` reading a
captured composite, `StringInterpolate` with a capture, `TypeCast`, and a
nested combination. Harness: `clone_map(body) -> Value` wrapping
`let counter = 0; let k = 3; array::map([1,2,3,4], |x| { let r = <body>;
counter <- x; r })`.

### 2. The accounting invariant (highest value; needs a hook)

Load an impure HOF, grow the array to N, shrink to 0, and assert
`env.by_id` (and runtime ref counts) return to baseline — clone↔delete
symmetry as an executable invariant. The `ExecCtx` lives in the runtime
task, so a `GXHandle` introspection hook is needed (the way `JIT_INVOCATIONS`
/ `match_shape` were added): e.g. `GXHandle::env_stats() -> EnvStats {
by_id_len, ref_var_total, … }`. Once the hook exists, grow/shrink
invariants are cheap to add everywhere. This is the test most likely to
catch a real bug (nag #2).

### 3. Env-node-in-callback fixtures (exercise the alias path)

A `try/catch`, a `~` (Sample), a nested `mod`, and `&`/`*` (ByRef/Deref) —
each inside an impure callback capturing an outer binding. These are the
*only* things that drive the `alias_variable` recompile path (nag #1).
Assert cross-slot value correctness AND per-slot independence (e.g. a
stateful builtin per slot doesn't cross-contaminate — subscribe back and
verify).

### 4. (ceiling) proptest-generated callback bodies — the swarm

Generate random valid callback expressions, run direct (no fusion) vs.
clone-path, assert equal output. Highest power, most setup. Build after
the deterministic matrix proves the shapes individually, so a proptest
failure has fewer places to hide.

## Bug found (effort #1) — let-bound select arm binding

The matrix immediately earned its keep. A `select` with an arm *binding*
(`select x { 1 => k, n => n * k }` — `n` is a catch-all that binds the
whole scrutinee), when it is the value of a `let` rather than the block
tail, panics on fusion:

```
undefined scalar local `n` — GIR is malformed   (gir_interp.rs:999)
```

i.e. the lowered kernel emits a `GirOp::Local("n")` read but never binds
`n` to the scrutinee value. The SAME select in **tail** position fuses
fine (`clone_select_capture` passes), so it's specific to a let-bound
(non-tail) select with an arm binding.

**It is NOT a `clone_rebind` bug.** The body panics identically via the
region-fuse path (`region_select_let_bound`, no `counter <-`, never
touches MapQ's clone), so the defect is in the fusion emit layer
(`emit_select_as_expr` / the let-value path) — pre-existing, surfaced by
the matrix. The two tests (`clone_select_let_bound`,
`region_select_let_bound`) are `#[ignore]`d as live documentation of the
bug; remove the attribute when the emit is fixed. (Per the project rule,
off-topic bugs are discussed before fixing — this one is queued for that
conversation.)

The first lesson the matrix confirms: the structural clones are sound for
the shapes tested (arith, comparison, bool ops, select-in-tail,
tuple/struct/array/map/variant producers + accessors, string interp,
nested) — 13/13 green — and the *only* thing that broke was an
independent fusion limitation it flushed out.

## Result (effort #2) — the accounting invariant HOLDS

Added `GXHandle::env_stats() -> EnvStats { by_id_len, ref_var_keys,
ref_var_total }` (a `ToGX::EnvStats` introspection hook mirroring
`match_shape`/`describe_shape`; the handler reads
`ctx.env.by_id.len()` and walks the runtime `by_ref` registry).
`env_accounting_grow_shrink` drives an impure HOF
(`array::map(arr, |x| { let v = x*2+1; counter <- v; v })`) — `arr`
bound at root scope and driven by BindId via `GXHandle::set` (so the
test apparatus mints no bindings of its own) — up to N=4 and back to 0,
four times, snapshotting `env_stats` at the bottom of each cycle.

The invariant held **tightly**:

```
peak (N=4 slots): by_id_len 771, ref_var_keys 766, ref_var_total 782
every bottom (0): by_id_len 759, ref_var_keys 750, ref_var_total 758
```

- All four bottom snapshots are **byte-identical** → zero per-cycle
  drift in any of the three registries. `Slot::delete`
  (`pred.delete` + `unbind_variable`) fully reverses each grow's
  `clone_rebind` (the per-slot `bind_variable("x")` + the cloned
  template's internal bindings + their `by_ref` edges).
- Peak ≫ bottom (each grow mints +12 bindings / +16 ref-keys / +24
  ref-edges for 4 slots) → the test is **non-vacuous**, and a leak of
  even a single binding per cycle would make `bottoms[1] != bottoms[0]`
  and fail. The `peak.by_id_len > base.by_id_len` guard makes that
  meaningfulness an explicit assertion.

So nag #2 — "silent unbounded `env.by_id` / `by_ref` growth, the
scariest failure mode because nothing turns red" — is now an
executable, sharp, green invariant. No leak.

## Result (effort #3) — the recompile-default / alias path is SOUND

Four fixtures plant an env/reference node (no structural `clone_rebind`
→ recompile-default + `alias_variable`) inside an impure callback
capturing the outer `k`:

- `clone_byref_deref_capture` — `let r = &k; *r + x` → `[4,5,6,7]`.
- `clone_sample_capture` — `x ~ k` (not soundly fusable, stays a
  structural `Sample`) → `[3,3,3,3]`.
- `clone_trycatch_try_capture` — `try (x / k) catch(e) => -1`
  (non-firing catch, try value used) → `[0,0,1,1]`.
- `clone_trycatch_catch_capture` — firing catch via the Connect
  pattern, capturing `x` AND `k` → `[4,5,6,7]`.

All four pass: the recompile-default re-resolves each capture correctly
per slot, with no cross-slot alias contamination (nag #1). The alias
path is exercised and sound.

**Method note — two wrong fixtures, caught by the non-clone ground
truth** (reinforces [[feedback-run-the-code]]). The first
`catch_capture` attempt (`try { error(\`Boom)?; 0 } catch(e) => x+k`)
produced `[0,0,0,0]`, which *looked* like a clone bug. Running the
SAME body as a PLAIN non-HOF program (`diag_plain_trycatch`) produced
`0` too → not clone, not fusion. Root cause was a wrong mental model of
try/catch:

> **The catch handler is side-effect-only.** `try BODY catch(e) => H`
> evaluates to BODY's value, *always* — even on error. `H` runs for its
> effects (a `<-` Connect to a separate binding) and does NOT provide a
> replacement value. A direct-value catch (`catch(e) => 99`) surfaces
> nothing: when BODY is all-error (`(0 /? 0)?` → `never`), the
> try-catch produces no value at all (the slot times out). This is the
> literal reading of "try-catch always evaluates to the last expression
> in try even if there is an error", and it's why every real
> firing-catch test (CHECKED_DIV0, CATCH4 in lang/errors.rs) uses
> `catch(e) => binding <- e` and returns the binding separately.

Localizing a suspected clone bug by running the body in the *non-clone*
path (plain top-level, or pure-HOF `genn::apply`) before blaming the
clone is the discipline that turned a false alarm into a semantics
lesson — twice.

## Result (effort #4) — the proptest swarm AGREES (128 cases)

`clone_matches_reference` generates random i64-valued callback bodies
over the element `x` and the capture `k` (`body_strategy`,
`prop_recursive` depth 4) and asserts the CLONE path (`clone_map`,
impure → per-slot `clone_rebind` of a fused template) produces the SAME
array as the non-clone REFERENCE path (`pure_map`, region-fuse or fresh
per-slot interpreted CallSite). Grammar (all total, no error/overflow at
these magnitudes): `+ - *`, literal-pattern `select` (no arm binding →
sidesteps #162), and tuple/struct accessors emitted as `{ let p = (a,b);
p.0 }` block-exprs (binding-first, since `(a,b).0` is a parse error;
nesting shadows `p`, valid + bonus coverage). 128 cases × (reference +
clone) all agree, 15s.

**Off-topic finding (parser, not clone):** the *first* grammar emitted
`(a, b).0` directly and proptest shrank to the minimal `((x, x).0)` —
which is a **parse error**: tuple/struct field access does not apply to a
parenthesized literal, only to a bound name (`let t = …; t.0`). A parser
limitation surfaced by the swarm; the grammar was reshaped to the
binding-first block form. (Not filed — it's a known shape; the matrix's
accessor fixtures already use the binding form.)

So the swarm — the highest-power axis — found no clone_rebind
disagreement across 128 randomly-nested compositions. Combined with the
matrix (shapes individually), the accounting invariant (no leak), and
the env-node fixtures (alias path sound), `clone_rebind` is well
covered.

## Status

- [x] 1. Clone-equivalence matrix (13 fixtures green; found 1 pre-existing
      fusion bug — let-bound select arm binding)
- [x] 2. Accounting invariant + GXHandle env-stats hook (HOLDS — no leak;
      tight byte-identical bottoms, peak ≫ bottom)
- [x] 3. Env-node-in-callback fixtures (4 green — recompile-default/alias
      path SOUND; surfaced the catch-is-side-effect-only semantic)
- [x] 4. proptest swarm (128 random nested bodies, clone == reference;
      surfaced an off-topic parser limitation on `(a,b).0`)

## #162 review — TWO MORE confirmed bugs (the thread kept giving)

An adversarial review workflow of the #162 fix (3 finders → verify)
surfaced two CONFIRMED silent-wrong-output bugs, both empirically
reproduced:

**A. Non-idempotent scrutinee DUPLICATED.** The `known_consts` channel
(and the pre-existing arm-condition `gir::cmp(scrut.clone(), …)`) INLINE
the scrutinee GirExpr at *every* use site. For an idempotent scrutinee
(a `Local` read) that's invisible; for a non-deterministic / side-
effecting one it diverges. `select rand::rand(…) { n => n == n }`
returned `false` (each `n` re-dispatched `rand`, drawing two values);
`n - n` returned a nonzero. PARTLY PRE-EXISTING: a multi-literal-arm
select (`select rand(…) { 0 => …, 1 => …, _ => … }`) already duplicated
the scrutinee across the arm conditions, independent of the #162
binding work.

Fix: `stabilize_scrutinee` — bind a non-`Local` SCALAR scrutinee to a
fresh temp local once (`let __sel_scrut_<exprId> = scrut`), then feed
`Local(temp)` to every arm condition / type-predicate / binding. Expr
form wraps the `IfChain` in a `Block`; stmt form prepends a
`GirStmt::Let`. The temp needs no `register_kir_binding` — the runtime
Block/Let provides its value and `GirOp::Local` dispatches on the expr's
own `typ`. Verified: `rand n==n → true`, `n-n → 0`, nested non-
idempotent scrutinees, arith-scrutinee multi-ref bindings, all in the
JIT path.

**B. Variant-payload arm had the SAME shadow bug** the scalar `Bind`
arm's guard fixed — a payload bind whose name collides with a kernel
input read the input, not the payload (`select \`Pair(x, b) => x + b`
with element `x` in scope → `[4,5,6,7]` instead of `[14,15,16,17]`).
Fix: the same `if arm_ctx.lookup_local(bind_name).is_some() { return
None }` guard in the variant payload loop (→ bails to interp, correct
value). The review also CONFIRMED-clean the rest of the fix (scalar
Bind correctness, guard completeness for the non-shadow cases).

Regression: `fused_select_scrutinee_evaluated_once` (rand `n==n`),
`_once_subtract` (rand `n-n`), `fused_select_stabilize_multiref` (arith
scrutinee, JIT), `fused_variant_payload_shadow`. 132 compiler + 1895
graphix-tests green.

## Bottom line

All four efforts done. One real pre-existing bug found (#162, let-bound
select arm binding — now FIXED) — and pulling that thread surfaced a
CASCADE of deeper bugs, each one level below the last, all now fixed:

1. **#162** — fused `select` arm binding panic (found by the matrix).
2. **Scrutinee duplication** (`rand n==n → false`) + **variant-payload
   shadow** (found by an adversarial review of the #162 fix).
3. **#167** — `Select::clone_rebind` re-minted arm patterns in one shared
   scope instead of per-arm `sel<id>` sub-scopes → a sibling arm's binding
   shadowed a selected arm's outer ref → hang (found by the matrix; fixed
   in the node walk per [[feedback-node-walk-is-canonical]]).
4. **#168** — `Lambda::clone_rebind` (the recompile-default) dropped a
   nested HOF callback's GRANDPARENT capture, because `Lambda::refs` is
   empty so the alias loop never aliased it → the inner map hung (found by
   AUDITING for more clone_rebind-vs-compile divergences after #167).

The through-line: **`clone_rebind` must faithfully reproduce `compile`'s
scoping/capture behavior** — #167 (per-arm scopes) and #168 (lambda
captures) were both silent divergences from the compile path. Plus a
parser limitation surfaced (`(a,b).0`). The original four test efforts
validated `clone_rebind` on the axes they covered (per-shape matrix,
accounting, alias path, random composition); the cascade shows those axes
didn't exercise nested/shadowing scope edges — now covered by the #167
and #168 regression fixtures (incl. `CFlag::FusionDisabled` node-walk
variants). 132 compiler + 1901 graphix-tests green, 0 ignored.

## #162 FIXED — fused `select` arm binding (Jun 2026)

Root cause (empirically traced + corroborated by a 4-agent understanding
workflow): `emit_arm_condition`'s `StructurePattern::Bind(name)` arm
(`fusion/lowering.rs`) registered the bound `name` as a kernel **input
feeder** with no value, so the arm body's `Ref(name)` lowered to a
dangling `GirOp::Local(name)` that panicked at runtime
(`gir_interp.rs:999`, "undefined scalar local"). The bug is NOT
let-bound-specific and NOT clone_rebind — it hit **any** fused `select`
with an arm binding (catch-all `n`, typed capture `i64 as n`,
guard-using captures), in tail / let-value / arithmetic position alike.
(The matrix's `clone_select_*` fixtures "passed" only because the impure
clone path doesn't fuse the block *tail* — the select ran interpreted,
dodging the bug. The real fused path is the *pure* HOF.)

Fix: bind `name` to the scrutinee via `arm_ctx.known_consts.insert(name,
KnownConst { expr: scrut.clone() })` — exactly the inline-expr channel
the sibling variant-payload binds already use. The arm body's
`Ref(name)` then resolves to the scrutinee through `find_const`. Backend-
agnostic (emit-layer), so both interp and JIT benefit. Plus a **shadow
guard**: the `Ref` arm checks `lookup_local` (inputs) before `find_const`,
so an arm binding that shadows a same-named kernel *input* can't win —
that case bails to the interpreter (no wrong value) rather than silently
reading the outer input.

Regression suite (`lang::fusion`): the two #162 tests un-`#[ignore]`'d
(`clone_select_let_bound`, `region_select_let_bound`) + four
`fused_select_*` that assert the value AND `fusion_invocations > 0`
(catch-all, typed capture, guard+capture, arith-wrapped) + the proptest
grammar gained an arm-binding `select` arm. 132 compiler + 1891
graphix-tests green.

### #167 FIXED — `Select::clone_rebind` per-arm scope leak

Stress-testing the #162 fix found that a `select` arm binding that
shadows an outer name a SIBLING arm references — `let n = 100;
… select x { 1 => n, n => n*2 }` in a per-slot HOF callback — HANGS,
producing no value. Reproduced under `CFlag::FusionDisabled` (no
fusion), so a **node-walk correctness bug**, not a fusion bug.

Root cause (traced by instrumenting `bind_variable`/`Ref::compile` with
`GRAPHIX_167DBG` and diffing shadow vs no-shadow; corroborated by a
3-agent workflow that reached the identical conclusion): `Select::compile`
gives each arm a FRESH unique sub-scope (`scope.append("sel{SelectId::
new()}")`), so an arm's pattern binding can never be seen by a sibling
arm's body (`lookup_bind` walks ancestors only, never siblings). But
`Select::clone_rebind` — which MapQ uses to build the per-slot node
graph (even with fusion off) — re-minted every arm's pattern + cloned
every arm's body in ONE shared `scope`. So arm 2's binding `n` polluted
the shared scope; a *later* clone (template → per-slot) then resolved
arm 1's `Ref(n)` to that stale sibling binding — which is only written
when arm 2 fires (`bind_event` runs for the selected arm only) — so when
arm 1 fires, the ref reads an unwritten BindId, the arm produces nothing,
and the map never emits.

Fix (`node/select.rs`): `Select::clone_rebind` appends a fresh per-arm
`sel<SelectId::new()>` sub-scope, exactly mirroring `Select::compile`.
Per-arm isolation is a correctness invariant of `select`, and
clone_rebind must preserve it — **the node walk is graphix's canonical
execution model and must always be correct; we never sidestep a node-
walk bug by fusing around it** (the user's principle —
[[feedback-node-walk-is-canonical]]). With #167 fixed, the #162 shadow
guard's bail-to-interp now yields the CORRECT value, not a hang — so the
guard and this fix compose cleanly.

Regression: `shadow_arm_binding_outer_ref` (un-`#[ignore]`'d → `[100,4,
6,8]`) + `shadow_arm_binding_node_walk` (the same under
`CFlag::FusionDisabled`, asserting the canonical model directly).
132 compiler + 1897 graphix-tests green, **0 ignored**.
