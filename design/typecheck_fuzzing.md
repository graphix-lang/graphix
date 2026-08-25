# Fuzzing the typechecker

**Status: DESIGNED 2026-08-25, not built. Build after P2 lands.**

The differential fuzzer cannot see typechecker bugs on the acceptance
plane: a program wrongly rejected never runs, and both engines agree it
was rejected (a vacuous agreement — the batch-path-drift lesson). The
2026-08-25 P2 arc flushed four latent inference bugs in one week, all
of the same family — order-sensitivity of unification — and all found
by hand or by `examples_compile` luck. This doc is the plan for finding
that family by machine.

## 1. The oracle problem

A decision oracle — "should this arbitrary program typecheck?" — is a
parallel typechecker. That is the GIR trap: a second implementation of
the same semantics that must be maintained in lockstep and drifts
(CLAUDE.md, "do NOT reintroduce a parallel typed IR"). We refuse it.

Instead: three weaker oracles, none of which can drift into a second
semantics, because none of them ever *decides* an arbitrary program.

The property under test is ACCEPTANCE ONLY (`--check` exit status, and
the error class on rejection). We never run a transformed program.
This is a large simplification: a transform may freely change runtime
semantics — firing cadence, effects, values — and remain a perfectly
good typing probe. Throughput is `--check`-bounded (~80ms/subject with
the stdlib), an order of magnitude cheaper than the runtime lanes.

The SOUNDNESS direction (wrongly *accepted* programs) needs no new
machinery: a program that typechecks and then goes wrong at runtime in
a type-shaped way is already a finding in the differential lanes, and
the JIT makes type lies loud (`abi_kind`/`freeze_for_abi` crash rather
than shrug). This doc is about the completeness direction — wrong
rejection and inconsistent acceptance — which nothing covers today.

## 2. The three oracles

### 2.1 Constructive: generate well-typed by construction

A generator that works FORWARD from types — pick a target type, build
an expression of that type (the Palka / Csmith move) — is a
constructive proof of well-typedness for every program it emits. It is
not a parallel typechecker: it never judges programs, it only builds
canonical ones, and its "model" is the generation grammar itself.
Rejection of a generated program is a finding with no oracle consulted.

graphix-fuzz's generator already is this in embryo (its programs
mostly compile). The gap is intent: it is tuned for runtime
divergence, so it annotates generously and keeps inference shallow. A
TYPE-STRESS mode inverts that: after generating the well-typed term,
strip annotations the generator certifies as inferable, thread values
through unannotated HOF callbacks, abstract subterms into generic
helpers (`|xs, f| array::filter_map(xs, |x| f(x))` — the exact P2
shape), build unions and match them. Annotation-stripping is legal
ONLY here, where the generator constrains shapes: on arbitrary
programs an annotation may be load-bearing by language rule (the
aug22c "free union member stays free — annotate the result" ruling),
so strip-and-expect-accept is not an oracle on the corpus.

### 2.2 Metamorphic: acceptance-preserving transforms

Take any program the checker accepts — generated, fixture, example —
apply a transformation that should preserve well-typedness, and check
acceptance again. A flip (accept→reject) is a finding. This is the
twin oracle of 2026-08-19 lifted from runtime state-routes to typing,
and it targets exactly the family all four P2-era bugs belong to:
unification ORDER. Extracting a callback into a `let` perturbs when
the declared parameter types reach it (pre-unify vs body-first —
today's same-cell bug was precisely this flip between compiler
versions; the transform triggers it between program spellings within
one version).

Transforms are graded, and the grade sets the triage default:

- **SOUND** — preservation follows from language rules; a flip is a
  compiler bug, auto-filed.
- **EXPECTED** — preservation is the obvious reading but no rule
  guarantees it; a flip files for triage (compiler bug, transform
  precondition bug, or a language-rule discovery → pending-ruling).
- **EXPLORATORY** — we genuinely don't know; flips are data for
  rulings, never auto-bugs.

A language-rule discovery is not noise: aug22c's "free union member
stays free" is the kind of rule this lane would have forced into words
earlier, with a corpus of witnesses attached.

### 2.3 Reflective: the checker's inferences as self-claims

Every inferred type the checker can print is a claim it can be held
to: re-insert the printed type as an annotation and recompile. Adding
a CORRECT annotation must never flip accept→reject. Inference and
annotation-checking cross-examine each other; nothing external is
trusted, and the printed-type path gets a second oracle for free — an
inferred type that fails to re-parse, or re-parses to something the
checker rejects, is a printer/parser finding.

Needs one small compiler hook: a post-typecheck dump of top-level
binding types (deref'd, name-compressed — the `format_with_flags`
printer). v1 inserts CLOSED types only: inserting open/generic
signatures walks straight into the standing "body-annotation tvars are
fresh, not the signature's" open item, which is Eric's to rule on
first.

## 3. The v1 transform catalog

All transforms are `Expr → Expr` on the parsed AST (no type
information needed in P1), printed back through the pretty printer —
whose round-trip proptest already guarantees re-parseability. Fresh
names are drawn from a reserved prefix (`__tm<N>`); a transform that
cannot apply at any site is skipped, never forced.

| transform | grade | what it does / preconditions | what it stresses |
|---|---|---|---|
| parens-wrap | SOUND | `e` → `(e)` at interior expression sites | smoke; ExplicitParens transparency |
| union-permute | SOUND | permute members of a `[A, B, ...]` annotation | Set normalization/ordering |
| block-wrap | EXPECTED | `e` → `{ let __t = e; __t }`; not on direct lambda-literal arguments (the pre-unify push changes) | bind publication, scope paths, generalization at let |
| let-extract | EXPECTED | `f(.., \|x\| body)` → `let __c = \|x\| body; f(.., __c)` | THE order probe: declared-param push vs body-first inference, per-instance elaboration, poly_binds. Today's bug's shape |
| let-inline | EXPECTED | substitute a single-use, non-shadowing `let x = e` into its use | the reverse order probe; gains the pre-unify push |
| stmt-permute | EXPECTED | swap adjacent independent binds (no name overlap in either's Refs, no shadowing) | tvar allocation order; program-order sensitivity (the jul22e flap's program-shape face) |
| alias-swap | EXPECTED | hoist an annotation's structural spelling into `type __T = ...;` and annotate `__T` (and the reverse: expand a typedef name in place) | Ref vs expansion channels: resolution cells, `ref_id` identity, `lookup_ref` — the aug24a skew family |
| eta-expand | EXPLORATORY | `f` in value position → `\|x\| f(x)`; positional-only, non-variadic sigs | first-class-value instantiation (`poly_binds`/`Ref::typecheck0`) vs direct call |

Findings are `TypeFlip { transform, direction, error_head }`; dedup
keys on (transform kind, normalized error head, statement-shape hash),
the TraceDiff pattern. Minimization reuses the typed-AST HDD reducer
with the property "the transform still flips acceptance on the reduced
base" — the transform is re-derived per candidate, never stored as an
artifact.

Nondeterminism guard: before filing a flip, recompile base and
transformed once each in a fresh process. A program whose OWN
acceptance flaps across processes is a DETERMINISM finding (the
jul22e class, acceptance face — detcheck covers fusion shapes, not
this), filed separately, never as a TypeFlip.

## 4. Program sources

- **The generator's compiling subjects** — already flowing in every
  campaign; the metamorphic scan rides them like the twin scan does.
- **`graphix-tests` fixtures** (~2200) and the **book examples**
  (100+, full-shell) — dense in exactly the vocabulary the generator
  lacks (traits, abstracts, selects over unions, GUI signatures).
  data_table_dashboard through let-extract IS this week's bug. These
  are static seed corpora, re-scanned when the transform catalog or
  the compiler changes.
- **The findings corpus** — every historical witness, same rationale.

## 5. Integration with graphix-fuzz

- `typemorph <file>` — one-shot: apply every applicable transform,
  report flips. The triage tool.
- `typemorph-scan [n] [seed]` — corpus + n generated subjects, the
  gate form (like detcheck).
- Campaign lane: each compiling subject gets k transform probes
  (k·~80ms, compile-only — cheap enough to run on every subject
  rather than a percentage lane; tune down only if it crowds the
  runtime lanes).
- Infrastructure reuse: sandbox_cwd children, BreakageWindow (a buggy
  transform floods findings and trips it — that is the transform-bug
  backstop), corpus conventions, the fleet. ONE Subject derivation
  (the batch-path-drift rule) — the transform runner consumes the
  same `Subject` the other lanes do.
- Self-check analog: transforms are deterministic functions; on the
  pinned corpus, `T(p)` must be byte-stable across runs (printer +
  transform determinism), enforced in the startup gate.

## 6. Would it have caught the known bugs?

Honest grading — ✓ = the mechanism directly triggers it, ~ = plausible.

- **Same-cell TVar pair** (2026-08-25): let-extract on the
  `array::init` callback flips it. ✓
- **Pre-unified return cell** (P2 bug 1): the generic-wrapper shape is
  a named target of type-stress generation, and let-extract creates it
  from plain filter_map calls. ✓
- **constrain_known alias chains** (P2 bug 2): order perturbation
  (let-extract / stmt-permute) reaches alias-chain shapes. ~
- **Select union narrowing** (P2 bug 3): needs union+wildcard seeds —
  fixture corpus carries them; alias-swap/union-permute perturb the
  scrutinee spelling. ~
- **jul22e settle-order flap**: the fresh-process determinism guard is
  purpose-built for it. ✓
- **Module finding 1** (def-site/use-site type-name asymmetry,
  admin campaign): alias-swap crosses def/use spellings. ~
- **aug24a seed_typedef_refs mode skew**: mode parity stays its own
  gate; alias-swap hammers the same Ref-identity channel. ~

## 7. Phases

- **P1 — metamorphic lane.** The transform engine + catalog above,
  typemorph/typemorph-scan, findings class + dedup + HDD property,
  seeds (generator, fixtures, examples, findings), determinism guard,
  startup-gate stability check. Zero compiler changes.
- **P2 — reflective + type-stress.** The binding-type dump hook;
  annotation-insert (closed types) + printed-type re-parse oracle;
  the generation mode (annotation stripping under generator
  certification, unannotated HOF chains, generic helpers).
- **P3 — must-reject.** Generator-known disjoint substitutions (a
  provably ill-typed mutation of a by-construction program must be
  rejected). Weakest direction — structural subtyping, unions and
  `Any` make "disjoint" narrow — so it comes last. Trait vocabulary
  in the generator (a standing gap) unlocks trait-shape morphing for
  every phase.

## 8. Risks and open questions

- **Transform-soundness noise.** The grading system + pending-ruling
  flow bounds it; BreakageWindow bounds a broken transform. Expect
  the first corpus sweep to force a few language rules into words —
  that is a feature, but it is triage time to budget.
- **Printer fidelity.** Transforms round-trip through the pretty
  printer; a printer bug becomes a TypeFlip. Label such findings
  (parse-of-print failure) rather than letting them masquerade as
  inference bugs.
- **Annotation-insert vs fresh body-annotation tvars.** Deliberately
  deferred behind Eric's open ruling; v1 closed types only.
- **Seed-corpus staleness.** Fixture/example scans are only as strong
  as the last sweep; wire typemorph-scan into the same cadence as the
  regress gate rather than trusting campaign incidence.
