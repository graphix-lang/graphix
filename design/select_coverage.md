# Select: sets, coverage and patterns

Status: current principle.
Pins: `stdlib/graphix-tests/src/lang/select.rs`, `select_undecided.rs`.

What reaches each arm, exhaustiveness, undecided types at a select, patterns inside a declared variable, `name@` captures.

- **Sets and coverage**: one walk (`select.rs::Reach`) decides what
  reaches each arm: its binds narrow to it (over a union, each to the
  union of its parts over every member the arm can match, never to the
  first member's, each part the member's own and never a copy, so what
  the arm's body decides of a bind decides the member's cells; a bind whose part, or union of parts, is `Any`, written
  or named by typedefs, is `Any`, never left for the arm body to decide,
  `Type::open_over_any`; a part is looked through every name of a chain
  of typedefs, `Type::head`),
  an arm that can match none of
  it (by its type test, and by its structure where the test does not
  hold every part the structure tests: a slice's elements, each one the
  value must hold and judged by its shape, a tag, an arity or a struct's
  fields but never a literal, since the footprint is also the shape a
  written test's binds are typed by (`RuledOut::ByShape`); an abstract's
  payload; the structure under a written test; `Reach::structure_admits`,
  `StructPatternNode::footprint`), or
  that tests what an earlier unguarded arm tests, is dead, and the
  select is exhaustive when nothing reaches past the last arm. Select exhaustiveness is enforced; slice-pattern
  length ladders count as coverage, one ladder per array or list member
  (a slice arm claims its lengths of each member whose element type its
  elements cover), and one per slice position inside a tuple, struct or
  variant member, keyed by its path and by the member as the arm's shape
  narrows it, along positions whose every member the pattern admits a
  value of fits it with the same slices (`select.rs::NestedLadder`,
  `inner_slices`: a variant's payload over an undecided scrutinee is one
  array member an arm, `` `A([Array<'a>, Array<'b>]) ``; a member the
  pattern admits no value of (`StructPatternNode::admits`), the `null` of
  `` `A([Array<i64>, null]) `` under `` `A([h, ..]) ``, is left out, so
  the ladder stands on `` `A(Array<i64>) ``, its completion covers that,
  and the arm `` `A(null) `` the rest): an unguarded arm claims its slice's
  lengths there where that slice is all it tests, its elements and every
  other position taking any value of their type (`StructPatternNode::covers`:
  a tuple of names over `[(i64, i64), i64]` does not), and its predicate
  holds the member; an
  arm is dead where at each member it can match one of its slices
  matches only lengths claimed at that path, on a ladder whose member
  holds the member as the arm narrows it (`([a, b, ..], _)` after
  `([a, ..], _)`; `` `A([(a, b), ..]) `` after `` `A([h, ..]) `` over
  `` `A([Array<(i64, i64)>, Array<string>]) ``), and a complete ladder
  covers its member; exact-length slice arms pool their elements' heads per length
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
  a type test pools only a member the test holds, held rigid (a declared
  variable the test names holds only itself, never its bound); a tuple or
  struct member a written test can match and does not hold is also cut
  into parts (`PartPool`), each one member of every position's union (a
  primitive set one per primitive, a declared variable at the position
  its one typed bound's members, each also held by a test naming the
  variable), at most 64 (`MAX_POOL_PARTS`) or it stays whole, and each
  part pools on its own the arms whose test holds it: a complete part
  covers itself, a declared variable's parts together the variable, and
  an arm whose every part it reaches already matches what it matches
  there is dead (`(true, _)`, `(bool, i64) as (false, n)` and `(bool,
  null) as (false, _)` cover `(bool, [i64, null])`); neither pool finds
  dead an arm whose written test can match a scrutinee member it does
  not see (an open cell, read as its one typed bound, or `Any`:
  `Reach::reaches_unpooled`); a group whose combinations of
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
  the walk the declared-variable rule below takes), and so is any cell
  of a written test open before the arms (its own variable, nothing
  declares it) that its structure tests, whether the narrowing against
  what reaches the arm (an array's element, `Any`) bound it since or
  not: a bound one is met as a decided cell is, below
  (`Deciding::hold_written`), and
  a written test that takes such a member only whatever the cell becomes,
  not with the cell one type of its own (`written_tests_undecided`); a
  name over it takes the open part beside the members' others
  (`TVar::narrow_per_member`).
  A cell an arm's pattern decided (the arms' settle, a completion, the
  narrowing, the coverage) counts as one still open there, a binding the
  decision rewrote read as it was (`Deciding`,
  `StructurePattern::meets_decided`, `written_tests_decided`), one the
  scrutinee holds only through an open cell's lower bounds (`*r` over
  `&mut (x, 0)`, an abstract constructor's parameter) met in the
  scrutinee read as those bounds hold it (`lower_view`), less each
  member of that union a type the unguarded arms before the arm cover
  holds whole, every cell still open one type equal only to itself
  (`Uncovered`: over the referent of `` &mut (select b { true => (`K,
  x), false => (`L, 0) }) ``, `(_, 0)` behind `` (`K, _) `` meets no
  value of `x`; a member inside a part, `select (*r, 1)`, is not taken
  out), and so does
  one those bounds hold that the arms leave open (`Deciding::left_open`:
  over `&mut [x, 0]`, `` [`J, _] `` beside `_` decides the referent
  `Array<Any>`, which holds `x`'s values without `x`), except a
  whole open scrutinee, or the one cell its bounds hold whole
  (`W(x).0`), which the arms decide together, unless an arm
  holds a partial struct pattern; a pattern there that only takes apart
  every value of the shape it decides (a tuple, a struct naming every
  field, a slice or list with a rest, an abstract payload, of names, `_`
  and such patterns: `StructurePattern::only_destructures`) decides that
  shape as an annotation would and tests nothing, except beside a member
  of that shape, where it chooses between them. An arm the coverage
  finds dead has the arms up to it judged so first, against the cells
  still open and those decided (`still_open`), the dead arm itself in
  what reaches it alone, never in the bounds' reading, which holds what
  the arms before it took; any other error in an
  arm's check is that error.
  Exhaustiveness holds those cells rigid on copies first
  (`Type::contains_holding`), so a part only names and `_` meet covers
  whatever it becomes, else where the cover decides them. What reaches
  the next arm (`Type::diff`) loses a tuple, struct or variant only
  whole, where what an arm covers holds every position of it by shape
  (`setops::held_by_shape`: `Any` or the same type there, two open
  cells the same only where they are one, primitives its flags hold,
  through parts of one shape at any depth, its own union there member by
  member and its own name read through, to the expansion a bind's cell
  holds too; a union the arm writes there holds what one of its members
  holds, a primitive flag by flag, and a name it writes is read through
  against the walk's pair budget, never again on a path once the other
  side has read a name since, so two recursive names hold nothing of
  each other), so `` `A((a, _)) `` takes all of `` `A(P) `` with
  `type P = (i64, i64)`, `([i64, string], i64)` and `(Number, i64)`
  take all of `(i64, i64)`, and a written `` `A(('b, Any)) `` takes
  nothing of `` `A(('a, i64)) ``. What reaches the next arm
  (`Type::diff`) and `could_match` unfold a pair of named types'
  definitions (arguments ignored) once on a path, a pair with a side
  that is no named type uncounted; met again, diff subtracts nothing
  and could_match answers an overlap (`setops::UNFOLDINGS`), as they do
  past `setops::WALK_PAIRS` pairs
  (either side named) in one walk, and an Error keeps its payload whole
  where the payload's difference nests `setops::ERRORS_OVER_CUT` Errors
  over a pair met again: a wider residual, so fewer dead arms
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
  (`StructurePattern::meets_rigid_written`). A select and a
  destructuring `let` meet the variable in the value as it stands and as
  its open cells' lower bounds hold it (`lower_view`: `*r` over `&mut [x,
  0]`, an abstract constructor's parameter), a whole open scrutinee
  included, since no pattern decides a declared variable. Under a written
  test a bind
  inside the variable takes the test's part (`Type::matching_parts`); a
  name over a value holding it keeps the variable where a part of the test
  around it holds the scrutinee's part there
  (`PatternNode::type_inside_rigid`), and one that stands inside it in
  another alternative or member takes both. A name over the variable's
  value at the top of the test (`[`A(i64), `B(i64)] as v` over `'b`) is
  typed by the test, never `'b`. A part of the test holding a variable of
  the test's own types none of the declared variable's values (a run-time
  test passes every value at the test's own variable), so a name under it
  is the declared variable (`Type::matching_type`): `Array<['c, null]> as
  [x, ..]` over `[Array<'b>, i64]` beside a `_` arm gives `x: 'b`. Where
  the arms' settle against the scrutinee has made `'c` the declared
  variable first (`Reach::new`, which runs with no wildcard arm or over a
  scrutinee that is no union), the part is closed and `x` is `['b,
  null]`. The name does not hold `'c`, so the arm's body cannot decide it
  (`x$ + 1` is refused, as it is under the closed test `Array<[i64,
  null]>`). The narrowing of what reaches the arm decides a `'c` that
  another part of the test stands over a decided type with, and the
  run-time test tests it: `(Array<'c>, Array<['c, null]>)` over
  `[(Array<i64>, Array<'b>), i64]` passes an `Array<[i64, null]>` alone
  at the second part, as it does where the settle runs and where an
  instance checks again, and `x` is `'b` all the same (pin
  `lang::select::written_test_own_variable_another_part_decides_is_tested_over_a_declared_one`).
  A slice over an open constructor's
  application (`'c<'b>`, a `c: Collection` parameter's element) stands
  over the slice's collection, `Array<'b>` or `List<'b>`, the values its
  run-time test passes: an inferred predicate's narrowing, its binds and
  the undecided-type walks read what reaches the arm so
  (`StructurePattern::collection_view`), so a name over the element is
  `'b`; a written test reads it so in `Type::matching_type`.

- **A written test's own variable is a part the arm binds, not one it
  tests.** Nothing declares it, and where the narrowing leaves it open it
  settles to ⊥, which a run-time test passes every value at
  (`Type::is_a`, `IsAFlags::Strict` apart). A named type applied to it
  would lose that: `Opt<'b>` over `type Opt<'x> = ['x, null]` expands to
  `null` once `'b` is ⊥, a union dropping its ⊥ member, where `['b,
  null]` written out keeps the cell. So from `typecheck1` on an arm's
  written predicate holds `Any` for every ⊥ in a named type's arguments
  (`Type::blind_named_args`, `Select::typecheck1`), which both engines
  read: `Array<Opt<'b>> as [x, ..]` matches what `Array<['b, null]> as
  [x, ..]` matches, under an array, a typedef's parameter and a
  recursive typedef's, and a closed argument (`Opt<i64>`) is tested. The
  union written out under a recursive typedef's argument (`U<['b, null]>`
  over `type U<'a> = [`Nil, `C('a, U<Opt<'a>>)]`) is rewritten alike: its
  tail's argument is the alias over it, which would drop the ⊥ a link
  down (pins `lang::select::written_test_alias_over_its_own_variable`,
  `written_test_union_over_its_own_variable`, each over a value three
  links deep).

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
  the element's structure admits (`hold_slice_members`). The structure
  admits a member where a value of it may pass every test the pattern
  makes (`StructPatternNode::admits`): a tag, a literal, an arity, a
  struct's fields and width, an abstract's type, at any depth, and a
  part that is `Any` or undecided passes any of them. A member one of
  them rules out gives the binds nothing (`` [(`A, h), ..] `` over
  `` Array<[(`A, i64), (`B, string)]> `` binds `h: i64`), whether it is
  a member of the element type or of a union inside one
  (`StructPatternNode::leaves`, `RuledOut::ByAnyTest`). Under a written
  test (`` P as (`A, h) ``) a bind takes nothing from a type the
  pattern's shape rules out (`RuledOut::ByShape`): a tag, an arity, a
  field, an abstract's type, in a union or alone, so an instance
  checked again (`GRAPHIX_NO_SUBST`), whose scrutinee may be that one
  member, types its binds as its definition did. A literal rules out
  nothing there (`P as (0, h)` over `[(i64, i64), (f64, string)]` binds
  `h: [i64, string]`). An or-alternative's binds are ruled out by shape
  as well, wherever it stands, so
  `` [`V(h, "a") | {t: null, c: h}, ..] `` over
  `` Array<[`V(bool, string), {t: f64, c: f64}]> `` binds
  `h: [bool, f64]`. A kernel reads a bind from the scrutinee's own part
  and a struct's field by its index, so it emits no arm, or-alternative
  or composite member whose structure admits no value of what it stands
  over (`StructPatternNode::may_admit`: `admits` at every part, a
  slice's elements included, each field at the index the pattern reads):
  an instance's scrutinee may be one member of a written test's type,
  one the test's structure rules out. A value read as the one composite
  member a structure admits is tested to be that member wherever the
  arm's own test passes every value (`emit_member_cond`). A guarded arm
  may be the last one such a kernel emits, the arms below it dead at
  the instance: a value its guard refuses matches no arm, as in the
  node-walk. A
  slice inside a
  tuple, struct, variant or or-pattern takes those members from each
  reaching member's part at its position, so a name over the element of
  `Array<'b>` there keeps `'b`, as at the top; where a member's part is
  `Any`, the union there is `Any` and the elements are the arm's own
  cells, as over an `Any` scrutinee. Where nothing types the elements (an
  `Any` scrutinee or part, `Array<Any>`, a scrutinee the arms decide) the
  predicate's own element union is the whole run-time test
  (`hold_untyped_elements`): a rest takes the type of a name beside it
  (`[h, ..] => h + 1` reads every element as an `i64`), and with no name
  it is `Any` (`[7, ..]` takes any array that starts with 7); and where
  the union holds `Any` (a `_` element, such a rest, a rest over
  `Array<Any>`) the test passes any value at every position, so the arm
  decides nothing of an element and every cell the union leaves open is
  `Any` (`[(a, b), _] => a + 1` and `[(a, b), ..] => a + 1` are refused;
  the written test `Array<[(i64, i64), string]> as [(a, b), _]` types
  them; pins `slice_rest_over_any_takes_a_names_type_or_any_value`,
  `slice_*_beside_a_wildcard_*`, `slice_bind_beside_a_rest_*`). Where
  the elements are typed above a part that is `Any` (`Array<(Any, i64)>`,
  `Array<{k: string, v: Any}>`) the same holds of that part
  (`hold_names_inside_any`): a slice of more than one position tests each
  against every member of its element union, so a name an element takes
  out of the `Any` part is `Any` wherever another member admits the
  element there, a rest, a name or `_` (each the whole element type) or
  an element whose own part there is `Any` (`[((a, _), n), ..] => a + 1`
  is refused, `Array<((i64, i64), i64)> as [((a, _), n), ..]` types it).
  Elements of one shape share their names' types, a slice of one element
  is its own test, and a tag tells elements apart (pins
  `slice_bind_inside_an_any_*`, `list_bind_inside_an_any_part_*`,
  `slice_binds_inside_an_any_part_*`). A partial struct pattern over a part
  that may be `Any`, the scrutinee or a part at any depth (`(_, {a, ..})`
  over `(i64, Any)`), has no struct to complete it, so its test would be
  the exact struct of the fields it names: it is refused where it is
  completed (`StructurePattern::unique_completion`), naming the written
  test (pins `*partial_struct_over_an*_refused`). Only where the pattern
  could match the member that holds the part: a tag, a literal or an
  arity beside the struct that the member cannot have leaves the pattern
  the other member's struct (`` (`K, {a, ..}) `` over `` [(`K, {a: i64,
  b: i64}), (`J, Any)] ``; pins `partial_struct_beside_an_any_member_*`,
  `partial_struct_under_a_name_beside_an_any_member_refused`). Pins:
  `lang::select::capture_*`, `slice_elements_typed_apart`,
  `slice_rest_scrutinee_type`, `slice_element_binds_hold_every_member`,
  `slice_element_bind_*`, `slice_element_or_alternative_bind_*`,
  `written_union_bind_beside_a_tag_*`,
  `written_union_bind_beside_a_literal_*`,
  `inferred_bind_beside_a_literal_*`,
  `written_test_over_an_instance_its_structure_rules_out`, the shell's
  `no_subst::an_instance_checked_again_takes_no_bind_*`,
  `nested_slice_*`, `let_partial_struct_capture_is_the_whole_value`.
