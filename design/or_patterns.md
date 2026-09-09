# Or-patterns

Status: built 2026-08-31
Pins: `stdlib/graphix-tests/src/lang/select.rs` (`or_*`: `or_first_match`, `or_same_binds_err`, `or_equal_types_err`, `or_dead_alt_err`, `or_dup_alt_err`, `or_slice_ladder`, `or_variant_exhaust`, `or_capture_union`, `or_payload_unequal_rejected`, `or_native`, `or_owned_binds`, `or_guard_prologue`), `graphix-compiler/src/expr/parser/test.rs` (`or_patterns_parse`)

Functional-programming orthodoxy, no deviations: same binds, one guard,
first structural match wins. Two Graphix-specific syntax rulings:
top-level or-patterns are select-arm-only, and `@`-captures are
per-alternative.

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
  `[typ as] sp1 | sp2 | … [if guard]`: the optional TYPE predicate
  prefixes the whole alternation (type alternation is already spelled
  `[i64, string] as v`), and the single guard covers the whole
  alternation.
- Nested alternation is legal in every bracketed element position:
  slice and list-slice elements, tuple elements, variant payloads,
  struct field sub-patterns, abstract payloads. All are
  delimiter-closed, so `|` is unambiguous there — including inside
  lambda params (`|(a | b, c)| e`; the tuple owns the input to `)`).
- **Top-level or-patterns are select-arm-only.** `let a | b = e` and a
  lambda param `|a | b| e` refuse: the positions are irrefutable (an
  or-pattern there is useless outside degenerate cases) and the lambda
  arg list's own delimiter is `|`. `let` refuses with a direct message;
  a lambda param fails at the phantom close.
- **`@`-capture is per-alternative.** There are no grouping parens in
  patterns, so there is no `x@ (p1 | p2)`; the capture is another bind
  and the same-binds rule enforces symmetry: `x@ `A(_) | x@ `B(_)` is
  legal, `x@ `A(_) | `B(_)` is a bind-set mismatch. No precedence
  interaction between `@` and `|` exists.

## Semantics

- Alternatives try LEFT TO RIGHT; the first structural match selects,
  and only its binds deliver. One value channel per name.
- **Same binds.** Every alternative binds exactly the same name set.
  PAYLOAD binds must have EXACTLY EQUAL types (bidirectional
  `contains` between the per-alternative inferred types) — the body
  reads through the slot at one type. An `@`-capture types as the
  UNION of its per-alternative narrowed types (`t: [`D(..), `E(..)]`
  above): Graphix narrows captures where Rust binds at the enum type,
  so exact equality refused ``kk@ `Up | kk@ `Char("k")``, the form
  orthodox code writes; the capture is the whole matched value, so the
  union is exact. Checked at pattern compile, before coverage math.
- **A shadowed alternative is a dead-arm error** (the house select
  rule applied within the arm): `` `A | `A ``, `_ | p`, and
  `[x, r..] | [a, b, c]` are errors — the arm-level dead walk's
  subtraction run over the alternative list.
- **Coverage is per coverage atom** (`arm_atoms`, `node/select.rs`):
  an or-arm claims once per alternative against its own member of the
  raw inferred Set, so `true | false` completes bool and `[] | [_, ..]`
  feeds the slice length ladder (the bound spelling `[] | [x, rest..]`
  is ill-typed first, by same-binds). Exhaustiveness, dead-arm analysis
  against earlier arms and scrutinee narrowing ride the union predicate
  (`infer_type_predicate(Or) = union`).
- Sleep/wake, selection memory and guard consultation are unchanged —
  the alternation is inside ONE arm; the arm's identity, guard and body
  are singular.

## Representation

`StructurePattern::Or(Arc<[StructurePattern]>)` (`expr/pattern.rs`),
appended last in the enum for Pack tag stability. Flat by
construction: the parser folds a chain into one `Or` (≥ 2
alternatives; a 1-element Or is never built) and an alternative is
never itself an `Or` (the grammar cannot spell one). The printer emits
`p1 | p2 | …` and needs no parens.

`StructPatternNode::Or { alts }` on the node side (`node/pattern.rs`).
`is_match(Or)` = any; `bind(Or)` = bind of the FIRST matching
alternative; `ids`/`unbind`/`delete` walk alternative 0 only (the
others share its ids — walking all would double-visit);
`is_refutable(Or)` = true; `matches_anything(Or)` = any alternative
does (its later alternatives are then dead, caught first).
`binds_uniq` applies within one alternative — the SAME name across
alternatives is required, not a duplicate.

## BindId sharing — the one real design point

The arm body references each bound name by ONE `BindId`, so every
alternative must bind the SAME id per name. Compile threads a
reborrowable `BindMode`: alternative 0 compiles under `Record`
(allocating ids via `env.bind_variable` and recording `name → (id,
type)`), later alternatives under `Reuse` (each leaf LOOKS UP the id
instead of allocating and binds nothing in the env — no shadowing, no
cleanup). Equal types enforce at each reused leaf: open cells unify
(one cell serves every alternative), concrete mismatches err, captures
widen to the union. A nested Or composes: under `Record` its first
alternative records into the outer map; under `Reuse` every
alternative reuses.

## Engines

- **Interp**: `PatternNode::arm_match` consults `is_match` and `bind`
  as for any pattern. The shallow-discriminant seal (`seal_shallow`)
  treats an Or arm as deep; a per-alternative shallow set is a possible
  later optimization.
- **JIT**: or-arms emit natively via `emit_or_chain`
  (`fusion/emit/select.rs`). The alternatives' structure conditions
  run left to right in their own block runs (each via
  `emit_structure_cond` against its member of the arm's raw inferred
  Set); the FIRST match materializes ITS binds through the ordinary
  `install_arm_binds` into a temporary env scope and forwards the
  values (ownership transferred) to ONE `done` block, whose params bind
  the arm's canonical locals once under the shared BindIds. The layout
  (sorted ids + kinds, from alternative 0) is total by same-binds; a
  mismatch Errs (de-fuse), never miscompiles. The guard prologue uses
  the same chain with no fail target: the no-match path feeds `done`
  with matched=0 and tainted drop-safe placeholders, so the guard still
  evaluates every invocation; the take chain passes the arm's fail
  block and `done` is match-only. The arm's env mark is taken BEFORE
  the chain, so the select-arm-exit scope drops cover the chain's owned
  binds on every edge. Explicit type predicates on or-arms refuse
  (`[T, U] as v` covers type alternation). Refusal residue is whatever
  an alternative's own shape refuses — the single-arm vocabulary, per
  alternative.

## Open

A dead ALTERNATIVE beyond duplicate/post-wildcard/range/type-dead
(structural overlap like `` `A(1) | x@ `A(_) ``) is not yet refused.
