# Type operations must scale as DAGs, not trees

Status: built 2026-07-13
Pins: `stdlib/graphix-package-gui/src/test/data_table_test.rs` (`empty_table`), `stdlib/graphix-package-gui/src/test/widgets_test.rs` (`context_menu_renders`)

## The defect

Graphix types are DAGs: structural nodes share `Arc`s, TVar cells are
shared by construction, and named `Ref`s/abstracts point at common
definitions. Every core type operation nevertheless walked and rebuilt
them as trees, and several carried per-PATH cycle guards (insert →
recurse → remove) that do not deduplicate a node reached along many
paths. Tree cost equals DAG cost only while types are small. The GUI
widget union was the first type population deep enough for the costs
to diverge, and static-instance typechecking put these operations on
the per-call-site hot path: the GUI package went from compiling in
under a second to a 41GB OOM.

It was one disease in six walks: `expand_ref_d` re-expanded
every occurrence of the same named type; retained instance signatures
were per-site deep copies; `normalize`/`flatten_set` re-walked shared
subtrees per occurrence with a restart-from-zero merge sweep (cubic in
set width); `resolve_tvars` deep-cloned unconditionally with a
per-path cell guard (40GB alone once the others were fixed);
`would_cycle_seen` deduped cells but re-scanned shared composites per
path; and `check_contains` eagerly FORMATTED both types into a failure
message that its caller used only as a probe.

## What is built

**Sharing-preserving rebuild walks (`Option<Type>` = unchanged).**
`resolve_tvars`, `normalize`, `replace_tvars`, `reset_tvars`,
`expand_ref_d` and the parallel `FnType` walks (via
`FnType::cow_walk`) return `None` when nothing beneath changed; the
caller keeps the original Arc. `Type::cow_slice` is the shared
rebuild-only-if-changed helper. TVar-free (or substitution-irrelevant)
structure is returned SHARED, never copied.

**DAG traversal state per pass.**
- `NormCx` (normalize): a visited-cell set plus a memo of each shared
  node's result, keyed by `Type::part_key`: the node's address
  (`node_addr`) and what two nodes over one allocation may differ by (a
  variant's tag, an abstract type's id). A tag, an abstract type and a
  name with no parameter are leaves and take no key.
- `ResolveTvarsCx` (resolve_tvars): bound cells snapshot ONCE per pass;
  unbound cells mint ONE fresh tvar per source cell — preserving the
  source's alias topology, the same discipline as `reset_tvars`'
  cell-keyed map (per-occurrence minting was an artifact of the tree
  walk, not a contract); an `in_progress` set is the cycle guard; plus
  the same `part_key` memo.
- A named type applied to parameters is a shared node like any composite
  (`node_addr`: its `Arc<TypeRef>`), so every walk that visits a node once
  takes a chain `T<T<..>>` a level at a time. Two walks give it no key:
  `ReadWalk`, which remembers no named type's copy, and
  `Type::copied_in_two_places`, which counts an application wherever it
  is met, since its count depends on the definitions being expanded.
- A search for a part (`Type::holds`: `holds_abstract`, `holds_ref`,
  `holds_bottom` and the rest) keeps what it found under each shared node
  (`cast::Verdicts::search_once`). An application is one of them: its
  parameters are all searched at every meeting, so a DAG of applications
  walked as a tree is Fibonacci many searches, and the fusion gate asks
  of every node's type whether it holds an abstract value
  (`lowering::meets_abstract`). What a search finds is found for good;
  that it finds nothing is kept only where it read no application still
  being decided, and inside one only an application is kept. Each kept
  node is held, so its address stays its own. Pins:
  `typ::cast::tests::a_search_walks_a_dag_of_applications_once`,
  `a_search_keeps_no_verdict_that_rests_on_a_definition_being_decided`,
  `deep_nesting::
  a_bound_dag_through_a_union_typedef_fuses_in_polynomial_time`.
- `contains` over such a chain (`ContainsHist`) keeps three things for the
  whole call, each by node. A committed pair's application is keyed by its
  definition and an id of its parameters' keys, and an application under a
  parameter by its own id (`app_id`), so a chain nested `n` deep is keyed
  in a word a level. A new key's cells are held for the walk
  (`TypeRef::key_cells`), so no later cell takes an address it reads;
  the cells of an application the key holds by id are that
  application's key's, held already, so holding is linear in the chain
  too. `RefHist::shape_id` keeps the shape hash of every
  node whose hash read no cell (`fixed_hashes`); one that read a cell is
  hashed again. `expand_ref` asks `closure` whether an application holds a
  cell, and a committing walk copies a cached expansion only where a copy
  differs from it: where it holds a cell or a function type
  (`Closure::Plain` is neither).
- The unions a `contains` walk builds to ask about (a distribution's pool
  of its candidates' parts, a placement's residue) are normalized in one
  pass kept for the call (`ContainsHist::normal_union`), so a node many
  pools share is normalized and flattened once, not once a question. The
  pass takes only members that are `Closure::Plain`: normalize rewrites a
  bound cell's binding and a merge reads it, so a member holding a cell is
  normalized in a pass of its own, as the cells stand. What the pass
  keeps of a name is its form as the name's resolution stood (two
  references to one definition merge once both are filled): the same
  type either way. Pins: `typ::contains::tests::
  a_walk_normalizes_the_nodes_its_pools_share_once`,
  `a_union_over_a_cell_is_normalized_at_each_question`.
- `would_cycle_seen` (the occurs check) is a pure existence query: the
  visited set is permanent (explored-without-finding never needs
  re-exploring) and holds composite node addresses as well as cells.
  Variant-blind address dedup is sound for a query (the answer depends
  only on leaves reachable from the allocation); `Map` is excluded from
  node-level dedup (two Arcs — a one-address key could alias a
  different pairing and skip the value).
- A literal's element cell (an array's, a list's, a map's key and
  value, a `&`'s referent: `TVar::bind_unshared`) binds to its
  elements' union without an occurs walk when nothing but the literal
  holds it, or, when something does (a call pre-unifies its formal
  with the argument's type before checking it; a `let` holds its
  value's type), when the union reaches no open cell
  (`reaches_no_open_cell`). That walk marks the cell it binds and
  every bound cell it walked (`TCell::ground`) with the `GROUND_EPOCH`
  that proved them, and a later walk stops at a cell marked with the
  current epoch, so a chain of lets over literals checks in linear
  time. A change to a bound cell between the lets (the check of a
  generic call makes one) costs each later literal over the chain one
  walk of what it reaches: a chain with such a call after every let
  checks in time quadratic in its depth (5,000 levels: about 3x the
  instructions of a walk-free check), accepted since only a type that
  deep pays it. The epoch advances on every change that can make a type reach
  a cell it did not (a bound cell's binding replaced, a constraint or
  lower bound added to one, an unbind); a normal form's write-back
  reaches only its source's cells and does not advance it. It is one
  counter for the process: a change in another context costs the next
  walk a re-walk of what it reaches, once.
- The same marks cut two walks of a settled chain (a call settles a
  chain of bounds last link first, so each link binds over links already
  bound and proved ground). Handing a `Discernible` or `Ordered` bound to
  a binding's open cells (`require_everywhere`) stops at a cell marked
  ground now: it reaches none. The comparison's judgment of a binding
  (`cast::rep_ambiguity`) keeps, in the ground cell, the epoch at which a
  judgment of its binding found nothing (`TCell::judged`, one slot each
  for `Discernible` and `Ordered`), read and written only where no
  application or abstract representation is in progress on the walk's
  path, so the verdict rests on nothing the walk assumed. A walk that
  read the environment's word on a type (an abstract's `Eq`/`Ord`
  implementation or representation, a head it could not resolve) keeps
  none from then on: environments are not ordered (a REPL input that
  fails takes its impls with it), so such a verdict is not the
  binding's alone. A key judgment (`map_key_failure`) keeps none.

- A chain of OPEN cells (a signature's bounds, each naming the next
  variable) has no ground mark to stop at, and every bind or merge of
  one of its links writes a cell. The occurs check numbers what it
  walks instead (`tvar::ReachOrder`). A walk that ends without finding
  its cell has walked a set closed under the edges it takes; where it
  met no cell twice on its own path (the cells form no cycle), it
  numbers them, each after every cell it reaches (`CycleWalk::number`,
  the number in `TCell::place`). A walk that met no standing numbering
  starts one only where it walked `ReachOrder::MIN_NUMBERED` (32) cells
  or more: a shorter chain is walked again by each of its links, a
  bounded cost, and takes no number, so neither the pass-by nor a
  standing probe verdict (`tvar::Standing`) applies to it, and a test or
  a generator of chains under 32 cells exercises no numbering. A walk
  that passed standing cells numbers what it walked above them whatever
  its length, one cell included. The invariant: while a cell's number
  stands, every cell it reaches holds a lower number of the same
  numbering, standing too, and none of them has been written. So a
  later walk passes by a standing cell unless the cell it looks for
  holds a lower number of that numbering. Every write to a cell
  (`CellLock::write`, whatever it changes) takes the cell's number and
  records the range from that number to the next one to be given:
  every cell that reached the written cell was numbered in that range,
  and stands no more; the cells below it stand. A walk that passes
  standing cells of one numbering gives the cells it walked numbers
  above them in that numbering; one that met standing cells of two
  numbers nothing. A numbering keeps few ranges: a range joins the one
  before it, taking every number above it, once as many writes are
  joined in it, so the links of a chain merged last link first are
  renumbered a few times each. The var's link is no edge of its own: a
  merge re-points a var only from a cell it writes, and a shortcut
  along forwarding cells reaches a subset. Compile tasks on other
  threads read an earlier task's cells at once, so two walks may number
  the same cells, and a task's write to a cell of its own takes numbers
  of a numbering another thread's walk is passing. Three rules keep the
  invariant then, each a no-op on one thread: a cell takes its number
  only while it holds the place the walk read there, the cells it
  reaches first, and the first that another walk numbered ends this
  walk's numbering (`CycleWalk::number`); a walk numbers nothing above
  a numbering a write has taken a number of since the walk first met it
  (`ReachOrder::take`); and a walk passes by a standing cell only while
  the cell it looks for holds the place it held when the walk began
  (`Numbering::met`). Pins: `typ::tvar::tests::
  two_threads_walking_one_chain_keep_one_order`,
  `threads_numbering_shared_cells_answer_as_a_fresh_walk_does`. With
  the numbering an impl whose head
  carries the chain merges each head cell with the method's copy
  without walking the links below (pins `deep_nesting::
  an_impl_head_with_a_chain_of_bounds_checks_in_linear_time`, and
  `typ::tvar::tests::numbered_cells_answer_as_a_fresh_walk_does`,
  which compares the check with a walk that trusts no number over
  random writes).
- The probe memo (`ContainsHist::probe_pairs`) holds a verdict while
  the thread writes no cell, under the flags the probe ran with
  (`contains::probe_flags`): a plain and a rigid probe ask the same in
  either polarity, and a probe under `Lower`, `IntoMember` or
  `LowerInArgs` (the guide of a commit that takes lower bounds) is its
  own question in its own polarity, kept as the others are. Walked at
  every meeting, such a guide triples the walk at each link of a DAG of
  bounds through a union typedef (`'a{i}: P<'a{i+1}, 'a{i+2}>` over
  `P<'x, 'y> = [Array<'x>, Array<'y>, null]`), which an instance's
  signature check walks though the definition's check does not. A
  verdict over two types that hold no cell read no cell, so it stands
  through every write. `place_in_order` measures each shared node's
  depth once per ordering (`contains::nesting`). Pins:
  `typ::contains::tests::
  an_instance_over_a_dag_of_bounds_through_a_typedef_takes_polynomial_steps`,
  `a_members_depth_measures_each_shared_node_once`,
  `a_probe_verdict_is_kept_under_its_flags`, `deep_nesting::
  a_bound_dag_through_a_union_typedef_builds_in_polynomial_time`. A
  verdict over an open var and a type that
  holds no cell read only the cells the var reaches, so once such a
  pair is probed a second time the memo keeps the var's cell and its
  number beside it (`probe_standing`, `tvar::Standing`, numbering what
  the var reaches where it must), and the verdict holds while that
  number stands, whatever else is written, under any flags (pin
  `typ::contains::tests::
  a_flagged_verdict_stands_while_no_cell_its_var_reaches_is_written`).
  A walk that asked whether a
  type implements a trait keeps and reads no such verdict from then on
  (`ContainsHist::impls_read`): the answer read the heads of the
  environment's impls, whose cells no var reaches. A call's argument pre-unifies
  part by part under one `ContainsHist` (`Type::pre_unify_seen`), as the
  whole would, so a call passing a value for every link of a chain of
  bounds asks each link once whether it admits the value (pin
  `deep_nesting::
  a_call_passing_values_to_a_chain_of_bounds_checks_in_linear_time`).

**A select's walks over two recursive unions.** The runtime-form check
(`cast::rep_collision`) keys the pair it is at, a side of it a named
type, by the two sides' `RefHist::param_id`s: an id hashes each shared
node of a side once, where the types themselves as the key hashed a
residual's DAG as a tree at every pair. A narrowing
(`Type::narrow_pattern`) holds the pairs it is narrowing whose sides
reach no open cell (`tvar::Narrowing::open`, by the same ids): one met
again inside itself holds, as a containment's does, and a pair held on
the word of one still open is not remembered, so a test for `U0 =
[Array<[U0, U1]>, ..]` over a value of `T0 = [Array<[T0, `A1]>, ..]`
ends. Pins: `typ::cast::tests::
a_form_walk_keys_a_shared_side_by_its_nodes`, `typ::setops::tests::
narrowing_over_two_recursive_unions_ends`, `deep_nesting::
a_type_test_over_two_recursive_unions_checks_in_time`.

**`flatten_set` worklist.** The accumulator is merge-saturated by
invariant, so only the incoming (or just-merged) element can enable a
new merge — no restart. `merge` compares nested positions via
`flat_eq`, which flattens only when a side actually IS a Set.
`flatten_set_tracked` reports whether anything changed so `normalize`
can keep the original member slice.

**`expand_ref_d` is memoized** (`fusion/lowering.rs`):
- Expansion memo (`MemoEntry`) keyed by the expanded `TypeRef` (by its
  fingerprint); entries carry the
  set of `Seen` keys their computation consulted, so they are valid on
  any path containing those deps. Only CLOSED keys are admitted
  (`key_closed`): an unbound-TVar param compares equal to any other
  unbound cell (`TVar::eq`), so caching one would cross-wire cells
  between sites and skip `lookup_ref`'s constraint registration.
- Structural node memo (`NodeEntry`, keyed by `norm_key`): a shared
  composite BETWEEN expansions resolves once per dependency context
  (`ResolveCx::node_frame` captures the consulted-deps delta).
- Fingerprints (`expand_key_fp`, u64) on every `Seen` node, memo entry
  and stored dep — scans compare u64s and fall back to full `TypeRef`
  equality only on a match (the string equality itself dominated once
  the walks were DAG-shaped).
- No per-path expansion cut: an expansive typedef (`type T<'a> =
  [`Nil, `C(T<Array<'a>>)]`) is refused at its definition
  (`typ/expansive.rs`), so a path holds finitely many distinct
  expansions. The backstop is a budget/size cap that only the
  fusion-side entry (`expand_refs`: budget 2 048,
  `FUSION_SIZE_CAP` 4 096) sets tight — truncation there only
  de-fuses, since no kernel encodes a type that large. Truncation
  poisons memoization (`ResolveCx::poisoned`) so a partial result is
  never cached.

**Lazy mismatch reports.** `TypeMismatch { expected, actual }` formats
on `Display`; a probe error costs two Arc clones.

## Measurement (GUI `empty_table` + `context_menu_renders`, debug build)

| configuration | result |
|---|---|
| before static instances (gated/swallowed rechecks over name-compressed types) | 0.92s |
| static instances, eager expansion, unmitigated | 41GB RSS, OOM killed |
| this design, full eager checking, no caps | 2.79s, 157MB peak |

The residual over the baseline was the honest cost of per-instance
typechecking plus the exponential `contains` walk over expanded
same-shape unions (flagged Set⊇Set is O(|lhs|·|rhs|) per nesting
level, and the two sides come from different provenances so identity
never prunes). That residual was closed by keeping instance signatures
NAME-COMPRESSED (`env_independent_typerefs.md`), which brought the
wedge back under the baseline; the DAG machinery here remains
load-bearing for the walks that still run. Hash-consing `Type` (O(1)
equality, canonical representation) would subsume most of this design
and is deliberately out of scope — it touches everything and interacts
with mutable TVar cells.

## Invariants for future type walks

- A new walk goes through the shared child walkers:
  `Type::try_for_each_child`/`for_each_child` for queries,
  `Type::cow_children` for rebuilds — write only the walk's
  interesting arms (TVar, and any arm whose traversal policy differs,
  e.g. skipping `Ref` params) and route the rest through the walker.
  `FnType::try_for_each_type`/`for_each_type` and `for_each_part`
  (which adds the signature cells' conjuncts) are the signature-side
  equivalents.
- A new rebuild walk over `Type` returns `Option<Type>` (None =
  unchanged) and uses `cow_children`/`cow_slice`/`cow_walk` — never
  unconditional reconstruction.
- A new query walk uses a PERMANENT visited set covering composite
  addresses, not only cells, and never a per-path insert/remove guard
  unless the semantics genuinely depend on the path (cycle detection).
- Nothing formats a `Type` into an error that any caller uses as a
  probe; carry the types, format on `Display`.
- Cache keys involving a `TypeRef` must be CLOSED (no TVars)
  — `TVar::eq` treats distinct unbound cells as equal.
