# Type Operations Must Scale as DAGs, Not Trees

Status: **built (2026-07-13); residual RESOLVED (2026-07-14) by
env-independent TypeRefs** — see `design/env_independent_typerefs.md`.
The `contains`-over-large-unions residual below is kept for the record;
carried resolution cells keep instance signatures name-compressed, so
the expanded-giant comparisons that hung the ~8 GUI tests no longer
exist (163/163 GUI tests pass in ~5s; the wedge is back at the
pre-instance-check baseline). The DAG/memo machinery in this document
remains load-bearing for the walks that still run.

## The defect

Graphix types are DAGs: structural nodes share `Arc`s, TVar cells are
shared by construction, and named `Ref`s/abstracts point at common
definitions. Every core type operation nevertheless walked and rebuilt
them as trees, and several carried per-PATH cycle guards (insert →
recurse → remove) that do not deduplicate a node reached along many
paths. Tree-cost equals DAG-cost only while types are small. The GUI
widget union is the first type population deep enough for the costs to
diverge, and `3983453c` (static-instance typechecking — the right
design) put these operations on the per-call-site hot path: the GUI
package went from compiling in under a second to a 41GB OOM.

Why only now — the old `setup_bind` DID recheck instance bodies, for
ages, and eagerly after static resolution. But three shields kept the
walks off large types: (1) it never called `resolve_internal_type` —
every type stayed NAME-compressed (`Type::Ref` is one word regardless
of what it names; `contains` expands lazily and one level at a time),
and a check that hit the private↔public abstract boundary swallowed
`AbstractOpaque` (the old code's own comment names gui `Color` as the
known artifact); (2) the arg/return checks were gated
(`has_unbound()` skip, `is_concrete_recheck_endpoint`) and body-tc0
errors were unconditionally swallowed; (3) since P4 step 1
(`27bcd43a`) `resolved_ftype` was a SHALLOW clone, so the recheck
unified types against their own shared cells — pointer identity
pruned everything. The static path removes all three at once, by
design: env-independent instance signatures require up-front
expansion (the first time the widget union ever existed in expanded
structural form), the checks are honest (failure is a probe, not a
swallow), and the instance is a detached copy (two pointer-distinct
materializations of the same giant shape, so identity never prunes).

The wedge was not one bug but one DISEASE in six walks, each exposed by
fixing the previous one (stack-sampled with gdb at every step):

1. `resolve_abstract_d` re-expanded every occurrence of the same named
   type (no memo at all).
2. Retained instance signatures were per-site deep copies.
3. `Type::normalize`/`flatten_set` re-walked shared subtrees per
   occurrence, with a restart-from-zero merge sweep (cubic in set
   width) and per-comparison re-flattening of nested sets.
4. `resolve_tvars` deep-cloned unconditionally, and its cell guard was
   per-path — bound cells and shared composites re-materialized per
   path (40GB alone once the others were fixed).
5. `would_cycle_seen` (the occurs check) and `mentions_abstract`
   deduped cells but re-scanned shared composites per path.
6. `check_contains{,_rigid}` eagerly FORMATTED both types into the
   failure message — and `check_instance_type` uses that failure as a
   PROBE (inspect for `AbstractOpaque`, resolve, retry), so widget-scale
   types were rendered to tree-scale strings and discarded, per
   instance arg.

## What is built

**Sharing-preserving rebuild walks (`Option<Type>` = unchanged).**
`resolve_tvars`, `normalize`, `replace_tvars`, `reset_tvars`,
`resolve_abstract_d`, and the parallel `FnType` walks (via
`FnType::cow_walk`) return `None` when nothing beneath changed; the
caller keeps the original Arc. `Type::cow_slice` is the shared
rebuild-only-if-changed helper. TVar-free (or substitution-irrelevant)
structure is returned SHARED, never copied.

**DAG traversal state per pass.**
- `NormCx` (normalize): visited-cell set + a pointer-identity memo
  keyed `(discriminant, content Arc address(es))` (`norm_key`),
  restricted to variants whose Arcs ARE the whole content (`Ref`/
  `Variant` carry extra fields; their composite children still memo).
- `ResolveTvarsCx` (resolve_tvars): bound cells snapshot ONCE per pass
  (`done`); unbound cells mint ONE fresh tvar per source cell
  (`fresh`) — preserving the source's alias topology, the same
  discipline as `reset_tvars`' cell-keyed map (per-occurrence minting
  was an artifact of the tree walk, not a contract); `in_progress` is
  the cycle guard; plus the `norm_key` composite memo.
- `would_cycle_seen` / `mentions_abstract`: pure existence queries —
  the visited set is permanent (explored-without-finding never needs
  re-exploring) and holds composite node addresses as well as cells.
  Variant-blind address dedup is sound for a query (the answer depends
  only on leaves reachable from the allocation); `Map` is excluded from
  node-level dedup (two Arcs — a one-address key could alias a
  different pairing and skip the value).

**`flatten_set` worklist.** The accumulator is merge-saturated by
invariant, so only the incoming (or just-merged) element can enable a
new merge — no restart from `(0,0)`. `merge` compares nested positions
via `flat_eq`, which flattens only when a side actually IS a Set.
`flatten_set_tracked` reports whether anything changed so `normalize`
can keep the original member slice.

**`resolve_abstract_d` is fully memoized** (fusion/lowering.rs):
- Expansion memo (`MemoEntry`): keyed by `ExpandKey`, entries carry the
  set of `Seen` keys their computation consulted — valid on any path
  containing those deps. Only CLOSED keys are admitted (`key_closed`):
  an unbound-TVar param compares equal to any other unbound cell
  (`TVar::eq`), so caching one would cross-wire cells between sites and
  skip `lookup_ref`'s constraint registration.
- Structural node memo (`NodeEntry`, keyed by `norm_key`): a shared
  composite BETWEEN expansions resolves once per dependency context
  (`ResolveCx::node_frame` captures the consulted-deps delta).
- Cross-call cache: `LambdaDef::resolve_cache` (`DefResolveCache`) —
  a definition's env snapshot is immutable, so closed expansions are a
  property of the DEFINITION; N static call sites share one resolution
  (write-through; REPL redefinition mints a new def = fresh cache, no
  invalidation protocol). Capped at `DEF_RESOLVE_CACHE_CAP` entries.
- Fingerprints: `expand_key_fp` (u64) on every `Seen` node, memo entry,
  and stored dep — membership and lookup scans compare u64s and fall
  back to full `TypeRef` equality only on match (the string equality
  itself dominated once the walks were DAG-shaped).
- Backstops (unchanged in spirit): the >256 per-path expansion length
  guard (non-regular recursion, `type T<'a> = T<Array<'a>>`) and a
  budget/size cap that only FUSION-side callers set
  (`resolve_abstract`, budget 2 048 / `FUSION_SIZE_CAP` 4 096):
  truncation there only de-fuses — no kernel encodes a type that large,
  so finishing the expansion buys nothing. Typecheck-side resolution
  (`resolve_internal_type`) is uncapped; truncation poisons memoization
  (`ResolveCx::poisoned`) so a partial result is never cached.

**Lazy mismatch reports.** `TypeMismatch { expected, actual }` formats
on `Display` (message text unchanged); probe errors cost two Arc
clones. `mentions_abstract` classification is unchanged.

## Measurements (GUI `empty_table` + `context_menu_renders`, debug build)

| configuration | result |
|---|---|
| `f10d343f` (before static instances — gated/swallowed rechecks over name-compressed types) | 0.92s |
| `3983453c` .. collection intrinsics (eager, unmitigated) | 41GB RSS, OOM killed |
| stopgap caps (bind-unresolved fallback — the hack) | 53s, then 0.64s by skipping the work |
| this design, FULL eager checking, no caps | **2.79s, 157MB peak** |

The remaining ~3x over the no-checking baseline is the honest cost of
per-instance typechecking plus residual constant factors (linear memo
scans, `node_frame` bookkeeping). Known further steps if it ever
matters: hash the expansion memo instead of fp-scanning, an Arc-ptr
fast path in `contains`, and interning/hash-consing `Type` (which would
subsume most of this design; deliberately out of scope — it touches
everything and interacts with mutable TVar cells).

## Invariants for future type walks

- A new rebuild walk over `Type` returns `Option<Type>` (None =
  unchanged) and uses `cow_slice`/`cow_walk` — never unconditional
  reconstruction.
- A new query walk uses a PERMANENT visited set covering composite
  addresses, not only cells, and never a per-path insert/remove guard
  unless the semantics genuinely depend on the path (cycle detection).
- Nothing formats a `Type` into an error that any caller uses as a
  probe; carry the types, format on `Display`.
- Cache keys involving `TypeRef`/`ExpandKey` must be CLOSED (no TVars)
  — `TVar::eq` treats distinct unbound cells as equal.

## Open residual: `contains` over large same-shape unions

~8 GUI interaction tests (button/container/children — signatures
carrying the full widget union) hang in `contains_int`. Measured: 52M
pure-probe calls AND 14M flagged (binding) calls in 45s, both growing
exponentially. Mitigations already in: content-identity fast path
(`same_content`), member-wise equality pre-pass in the Set×Set arm, a
pure-probe pair memo in `RefHist` (epoch-invalidated on flagged calls,
Arc-pinned keys), and raw (shared, un-reset) expansions for probe-side
ref crossings. None collapse it, because the FLAGGED lane — which binds
cells and therefore cannot be memoized — is exponential by algorithm:
Set⊇Set is O(|lhs|·|rhs|) recursive walks per nesting level, and the
two sides come from different provenances (declared instance signature
vs inferred arg type), so identity never prunes deep enough.

Candidate directions (needs a design ruling):

1. **Provenance-aware check skipping**: when the declared instance arg
   type and the site arg type derive from the SAME resolution artifact
   (common case — both sides come through one `DefResolveCache` entry),
   the check is a tautology; track provenance and skip. Sound by
   construction, narrow.
2. **Canonical unions**: normalize + sort at construction (they already
   sort in `flatten_set`) and match members by merge-scan over the
   canonical order instead of pairwise search; with hash-consed member
   identity Set⊇Set approaches linear. A real change to `contains`'
   member-matching semantics ordering.
3. **Hash-consing `Type`** (the endgame): O(1) equality and canonical
   representation subsume 1–2 and most of this design's machinery.

Until ruled, the tree is left WITHOUT stopgaps: the ~8 tests hang
honestly rather than pass by skipping the check.
