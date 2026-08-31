# Or-patterns

**Status: P1 (syntax) + P2 (both-engine semantics) + docs/generator/
tree-sitter LANDED 2026-08-31 (`7f68d184`); P3 (native emission —
`emit_or_chain`, see the JIT bullet) built the same day, gates in
flight. Open: the overnight 20k round-trip proptest (Eric runs it). Ruled by Eric 2026-08-31:
functional-programming orthodoxy, no deviations — exactly equal types,
same binds, single guard. Two Graphix-specific syntax rulings (same
day): top-level or-patterns are SELECT-ARM-ONLY (let and lambda params
refuse at parse — the lambda arg list is `|`-delimited); `@`-captures
are PER-ALTERNATIVE (`|` binds loosest, no new precedence rule).**

## Syntax

```graphix
select x {
    `A | `B => 0,                    // arm-level alternation
    `C(1 | 2, y) => y,               // nested — any bracketed position
    (0, y) | (y, 0) if y > 10 => y,  // binds in both; ONE guard per arm
    t@ `D(_) | t@ `E(_) => f(t),     // capture repeated per alternative
    _ => 3
}
```

- `|` is the loosest pattern operator. An arm parses as
  `[typ as] sp1 | sp2 | … [if guard]` — the optional TYPE predicate
  prefixes the whole alternation (one per arm; type alternation is
  already spelled `[i64, string] as v`), and the single guard covers
  the whole alternation (orthodoxy).
- Nested alternation is legal in every bracketed element position:
  slice/list-slice elements, tuple elements, variant payloads, struct
  field sub-patterns, abstract payloads. These are all delimiter-closed,
  so `|` is unambiguous there — including inside lambda params
  (`|(a | b, c)| e` is fine; the tuple owns the input to `)`).
- **Top-level or-patterns are select-arm-only.** `let a | b = e` and a
  lambda param `|a | b| e` refuse: the positions are irrefutable (an
  or-pattern there is useless outside degenerate cases) and the lambda
  arg list's own delimiter is `|`. `let` refuses with a direct message;
  a lambda param fails naturally at the phantom close (combine's
  error merging buries custom messages there anyway — the known
  refusal-diagnostic limitation).
- **`@`-capture is per-alternative.** There are no grouping parens in
  patterns, so there is no `x@ (p1 | p2)`; instead the capture is just
  another bind and the same-binds rule enforces symmetry:
  `x@ `A(_) | x@ `B(_)` is legal, `x@ `A(_) | `B(_)` is a bind-set
  mismatch. No precedence interaction between `@` and `|` exists.

## Semantics (orthodox)

- Alternatives try LEFT TO RIGHT; first structural match selects, and
  only its binds deliver. One value channel per name — see BindId
  sharing below.
- **Same binds, exactly equal types**: every alternative must bind
  exactly the same name set, and each name's type must be the same in
  every alternative — enforced as bidirectional `contains` between the
  per-alternative inferred types (the compiler's spelling of "the same
  type"). Checked at pattern compile, before any coverage math.
- **A shadowed alternative is a dead-arm error** (the house select
  rule, applied within the arm): an alternative structurally covered
  by the alternatives to its left can never match — `` `A | `A ``,
  `_ | p`, and `[x, r..] | [a, b, c]` are all errors. The check is the
  arm-level dead walk's subtraction run over the alternative list.
- **Coverage is the union**: the arm's type predicate is the union of
  the alternatives' inferred predicates (`infer_type_predicate(Or) =
  union`), so exhaustiveness, dead-arm analysis against earlier arms,
  and scrutinee narrowing all work unchanged. The slice LENGTH-LADDER
  pool takes one claim per alternative (an or-arm may claim several
  lengths — `[] | [_, ..]` alone covers 0..∞; note the BOUND spelling
  `[] | [x, rest..]` is ill-typed first, by same-binds).
- Sleep/wake, selection memory, guard consultation: unchanged — the
  alternation is inside ONE arm; the arm's identity, guard, and body
  are singular.

## Representation

`StructurePattern::Or(Arc<[StructurePattern]>)`, appended LAST in the
enum (Pack tag stability). Flat by construction: the parser folds a
chain into one `Or` (≥ 2 alternatives; a 1-element Or is never built),
and an alternative is never itself an `Or` (the grammar has no way to
spell one). The printer emits `p1 | p2 | …` and needs no parens (there
are none to need).

`StructPatternNode::Or { alts: Box<[StructPatternNode]> }` on the node
side.

## BindId sharing (the one real design point)

The arm body references each bound name by ONE BindId, so all
alternatives must bind the SAME id per name. AS BUILT (better than the
rewrite plan below deserved): compile threads a reborrowable
`BindMode` — alternative 0 compiles under `Record` (allocating ids via
`env.bind_variable` and recording `name → (id, type)`), later
alternatives under `Reuse` (each leaf LOOKS UP the id instead of
allocating and binds NOTHING in the env — no shadowing, no cleanup).
Exactly-equal-types enforces at each reused leaf: open cells unify
(one cell serves every alternative), concrete mismatches err. A nested
Or composes: under `Record` its first alternative records into the
outer map; under `Reuse` every alternative reuses.

Runtime: `is_match(Or) = any`, `bind(Or) = bind of the FIRST matching
alternative` (both walks already take `&Value`); `ids`/`unbind`/
`delete` walk alternative 0 only (the others share its ids — walking
all would double-visit). `is_refutable(Or) = true` always (a
degenerate irrefutable alternative is dead-alt-refused first, except
`p | irrefutable-last` — which is legal orthodox spelling; refutable
stays the safe answer for let/lambda, which can't parse an Or anyway).
`matches_anything(Or) = any alternative matches_anything` (such an
arm's later alternatives are dead — the dead-alt check fires first).

## Engines

- **Interp**: `PatternNode::arm_match` consults `is_match` and `bind`
  exactly as today — Or is one more recursive case in the nine
  `StructPatternNode` walks. The shallow-discriminant seal
  (`seal_shallow`) treats an Or arm as deep (v1; a per-alternative
  shallow set is a later optimization).
- **JIT (P3, as built 2026-08-31)**: or-arms emit natively via
  `emit_or_chain` (fusion/emit/select.rs) — the alternatives' structure
  conditions run left to right in their own block runs (each via the
  extracted `emit_structure_cond`, against its member of the arm's raw
  inferred Set), the FIRST match materializes ITS binds through the
  ordinary `install_arm_binds` into a temporary env scope and forwards
  the values (ownership transferred) to ONE `done` block, whose params
  bind the arm's canonical locals once under the shared BindIds — the
  layout (sorted ids + kinds, from alternative 0) is total by
  same-binds; a mismatch Errs (de-fuse), never miscompiles. The guard
  prologue uses the same chain with `nomatch: None`: the no-match path
  feeds `done` matched=0 with tainted drop-safe placeholders (the
  masked-install semantics, so the guard still evaluates every
  invocation); the take chain passes the arm's fail block and `done`
  is match-only. The arm's env mark is taken BEFORE the chain, so the
  select-arm-exit scope drops (the same-day leak-fix machinery) cover
  the chain's owned binds on every edge. Explicit type predicates on
  or-arms refuse (rare; `[T, U] as v` covers type alternation).
  Refusal residue = whatever an alternative's own shape refuses
  (nested variant payloads, @-binds, ... — the single-arm vocabulary,
  per alternative).

## Blast radius

- Parser + printer + **the round-trip proptest overnight** (the
  standing syntax-change rule); `expr/test.rs`'s pattern generator
  learns Or.
- Pack: enum variant appended last (`graphix-ast-pack` round-trips it
  via the same derive).
- `expr/pattern.rs`: `with_names`, `binds_uniq` (per-alternative — the
  SAME name across alternatives is required, not a duplicate;
  uniqueness applies within one alternative), `infer_type_predicate`
  (union), `complete_type_predicate` (per-alt), `single_bind` (None),
  Display.
- `node/pattern.rs`: compile (BindId sharing + equal-types check),
  `realign`, `ids`, `bind`, `unbind`, `is_match`, `is_refutable`,
  `matches_anything`, `array_len_range`/`array_len_coverage` (multi-
  claim: return per-alternative claims), `delete`.
- `node/select.rs`: the ladder pool takes per-alternative claims; the
  dead-ALTERNATIVE walk (within-arm) joins the dead-arm walk; nothing
  else changes (coverage rides the union predicate).
- Fixtures: `lang/select.rs` (match/bind/guard/coverage/dead-alt/
  equal-types-refusal), `#[native]` de-fuse pin.
- graphix-fuzz: the generator emits or-shapes (distinct-literal
  alternations — bool excluded, `true | false` completes coverage;
  grouped variant tags with bind-free payloads; the shared-bind tuple
  twin exercising `Reuse`); typemorph and the twin scan carry over.
  A dead-ALTERNATIVE beyond duplicate/post-wildcard/range/type-dead
  (structural overlap like `` `A(1) | x@ `A(_) ``) is not yet refused
  — follow-up.
- tree-sitter grammar, book (`core/select.md` gains an Or-patterns
  section), CLAUDE.md select bullet.

## Phases

1. **P1**: AST + parser + printer + proptest generator; `--check`
   level tests (parse/refuse/print round-trip). Gate: `cargo test` +
   the overnight proptest starts.
2. **P2**: node compile + all walks + select coverage/dead-alt +
   equal-types enforcement; interp fixtures green; JIT refusal
   (de-fuse) + pins. Gate: `cargo test`, FUSE audit for the de-fuse.
3. **P3**: JIT v2 emission (chained tests, one body block). Gate:
   fixtures flip to `FuseExpect::Jit`, bench sanity.
4. **P4**: fuzz generator + typemorph + tree-sitter + book; fleet
   redeploy soaks the syntax (gates are not the fuzzer).
