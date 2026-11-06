# Select: sets, coverage and patterns

Status: current principle.
Pins: `stdlib/graphix-tests/src/lang/select.rs`, `select_undecided.rs`.

What reaches each arm, exhaustiveness, undecided types at a select, patterns inside a declared variable, `name@` captures.

- **Sets and coverage**: one walk (`select.rs::Reach`) decides what
  reaches each arm: its binds narrow to it (over a union, each to the
  union of its parts over every member the arm can match, never to the
  first member's), an arm that can match none of
  it, or that tests what an earlier unguarded arm tests, is dead, and the
  select is exhaustive when nothing reaches past the last arm. Select exhaustiveness is enforced; slice-pattern
  length ladders count as coverage, one ladder per array or list member
  (a slice arm claims its lengths of each member whose element type its
  elements cover), and exact-length slice arms pool their elements' heads per length
  (`[true]` with `[false]` claims length 1); a `null` literal covers `null`, a
  constructor at a union position the members it takes, and a written test
  over a closed type the part of it its structure takes
  (`StructPatternNode::matched_part`: `` ([`A, `B], i64) as (`B, n) ``
  covers `` (`B, i64) ``); a collection type test (`Array<T> as`)
  narrows later arms only of collections whose every element it
  covers, since a mixed one that fails it may still hold a `T`;
  bool literals, variant heads
  (payload irrefutable) and or-patterns of them pool per position
  inside composite patterns, keyed by path (a partial struct pattern
  against the one member holding its fields, the rest any value; an open
  scrutinee's members are its structural bound's; a union position's one
  tuple or struct member holding no open cell is a head beside them, its
  own positions counted under that head alone; an open cell at the
  position, a declared variable included, is a head of its own that only
  a name or `_` there admits, `Head::Open`, as is a primitive member's
  value but `bool` and `null`, `Head::Rest`), and an arm under
  a type test pools only a member the test holds; a group whose combinations of
  heads pass 1024 (`MAX_POOL_COMBINATIONS`: eleven independent bool
  positions) pools nothing, so its arms cover nothing by pooling; an or-arm narrows later arms per
  alternative; a structure that matches anything is a wildcard only over
  a scrutinee its shape covers, and a written test covers a declared
  variable held rigid where it covers every member of its bound; set
  coverage distributes over product
  heads (`` [`P(A), `P(B)] ⊇ `P([A, B]) ``), and in every containment a
  rigid cell at the position it splits is its one typed bound's members,
  each also taken by a part holding the cell, else a member of its own,
  and one in that bound splits the same way, an open cell there held
  rigid while it probes (`Type::covers_by_parts`); a probe in progress for the
  same scrutinee ref claims nothing on re-entry. **A type still
  undecided at a select's end is one unknown type, equal only to itself**
  (`Select::check_undecided`): once the arms have decided what they will,
  an open cell the scrutinee held before them (never one their own
  patterns brought: a bind's, a `_`'s) in what reaches an arm (an earlier
  unguarded arm may take its member whole) that the arm tests, a
  pattern other than a name or `_` standing over its values, is refused,
  naming the binding to annotate (`StructurePattern::meets_undecided`,
  the walk the declared-variable rule below takes), and so is any open
  cell of a written test that its structure tests, and a written test
  that takes such a member only whatever the cell becomes, not with the
  cell one type of its own (`written_tests_undecided`); a name over it takes
  the open part beside the members' others (`TVar::narrow_per_member`).
  A cell an arm's pattern decided (the arms' settle, a completion, the
  narrowing, the coverage) counts as one still open there, a binding the
  decision rewrote read as it was (`Deciding`,
  `StructurePattern::meets_decided`, `written_tests_decided`), except a
  whole open scrutinee, which the arms decide together, unless an arm
  holds a partial struct pattern; a pattern there that only takes apart
  every value of the shape it decides (a tuple, a struct naming every
  field, a slice or list with a rest, an abstract payload, of names, `_`
  and such patterns: `StructurePattern::only_destructures`) decides that
  shape as an annotation would and tests nothing, except beside a member
  of that shape, where it chooses between them. An arm the coverage
  finds dead has the arms up to it judged so first, against the cells
  still open and those decided (`still_open`); any other error in an
  arm's check is that error.
  Exhaustiveness holds those cells rigid on copies first
  (`Type::contains_holding`), so a part only names and `_` meet covers
  whatever it becomes, else where the cover decides them. What reaches
  the next arm (`Type::diff`) and `could_match` unfold a pair of
  named types' definitions (arguments ignored) once on a path, a pair
  with a side that is no named type uncounted; met again, diff
  subtracts nothing and could_match answers an overlap
  (`setops::UNFOLDINGS`), as they do past `setops::WALK_PAIRS` pairs
  (either side named) in one walk: a wider residual, so fewer dead arms
  are found and a missing case may be refused that `_` fixes. What reads
  the part a difference left out (`?`'s strip and raised type) and
  `?`/`$`'s result type split the operand by its members, each name
  expanded once per parameter list (open cells by identity), where the
  budget widened it (`Strip::taken`,
  `Strip::left`). A partial
  struct pattern completes against the one member it can match
  (`StructurePattern::complete_type_predicate`); members told apart only
  by undecided cells are two members, refused unless what reaches the
  arm leaves one: annotate the cells or name every field.
  Refutability is judged
  against a type (`StructPatternNode::covers`): a constructor pattern
  covers only a position whose type is that constructor, so under a
  written type (a type test, a `let`, an abstract payload) one at a union
  position narrows to its member and is refutable; a written-union atom
  covers each member it covers; the pool reads an abstract's payload.

- **Patterns inside a declared variable**: a pattern that tests or binds
  inside a value of a type variable its definition holds rigid needs a
  written test (`` `A(i64) as `A(z) ``, a destructuring `let`'s
  annotation); only a name or `_` stands over such a value, and keeps the
  variable. At a union holding the variable beside other members a part
  meets it where it may hold one of its values, by its typed bounds, any
  value without one, inside a member the whole pattern may hold
  (`StructurePattern::meets_rigid`, judged as the select, the `let` or a
  destructuring parameter is checked, a test written over the variable
  itself as the arm compiles; an abstract pattern names the type its
  compile resolved, `CompiledAbstracts`); a name there takes the whole
  union. A written test's part over the variable must be closed: one
  holding `_` (a typedef's body included) or an undeclared variable is
  refused
  (`StructurePattern::meets_rigid_written`). Under a written test a bind
  inside the variable takes the test's part (`Type::matching_parts`); a
  name over a value holding it keeps the variable where a part of the test
  around it holds the scrutinee's part there
  (`PatternNode::type_inside_rigid`), and one that stands inside it in
  another alternative or member takes both. A name over the variable's
  value at the top of the test (`[`A(i64), `B(i64)] as v` over `'b`) is
  typed by the test, never `'b`.

- **`name@ pattern` captures** are typed from the SCRUTINEE (a `let`'s
  from its value, `Bind::complete_partial`): under an
  inferred predicate a capture is a type variable that
  `PatternNode::bind_narrowed` binds, after the select narrows the arm,
  to its part of the narrowed predicate (a `_` slot, the fields a
  partial struct pattern leaves out and a slice's rest carry the
  scrutinee's types; shared
  or-alternative captures union); a whole capture (`a@ (_, _)`) is
  the union of its part of each reaching member the arm matches, not
  of each position's parts, so it fits the scrutinee's own type. Never type a capture from
  `infer_type_predicate`. A slice's inferred element type is a Set with
  one member per element and a cell for the rest, so each element
  narrows on its own; its run-time test admits any member at any
  position, so a name an element binds holds its part of every member
  the element's structure admits (`hold_slice_members`). Pins:
  `lang::select::capture_*`, `slice_elements_typed_apart`,
  `slice_rest_scrutinee_type`, `slice_element_binds_hold_every_member`,
  `let_partial_struct_capture_is_the_whole_value`.
