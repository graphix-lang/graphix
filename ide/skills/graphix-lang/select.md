# Select: patterns over types not fully known

Beyond `SKILL.md`'s Select: declared type variables, types undecided at the
select, and runtime forms. Each rule refuses and names the fix.

- A pattern that tests or binds inside a value of a declared type
  variable needs a written type test; a name or `_` over the whole value
  keeps the variable. Under `'a: [{a: i64, b: string}, i64]`, `{a, ..} =>
  a` is refused, naming the test: write `{a: i64, b: string} as {a, ..}
  => a`, which covers the struct only, so an `i64 as n` arm (or a
  catch-all) must follow. So too `null` over `['a, null]` (write `null as
  _`), a literal or constructor at an element of `Array<['a, i64]>` that
  `'a` may hold (bound `'a` apart), and a destructuring `let` (annotate
  it: `let {x, y}: {x: i64, y: i64} = p`, or a parameter, `|(n, s):
  (i64, string)|`). A variable with no bound is bounded first (`'a:
  (bool, i64)`, then `(bool, i64) as (true, k)`). A value read through
  a reference or out of an abstract's payload is judged the same
  (`select *r` over `let r = &mut [x, 0]`, `select W([x, 0]).0`, `let
  ((a, b), c) = *r`). The test is a closed
  type: `` `A(_) as `A(z) `` (`_` is bottom, through a typedef too:
  `` `A(T) `` over `type T = _`) and `` 'c as `A(z) `` (an undeclared
  variable) are refused. Under a written test a name over the
  variable's value keeps it where the test holds the scrutinee around it:
  `([`A(i64), `B(i64)], i64) as (v, n)` over `('b, i64)` gives `v: 'b`;
  at the top, `[`A(i64), `B(i64)] as v` over `'b` types `v` by the test,
  not as `'b`. A variable of the test's own passes every value, so a
  name under it holds the declared variable's values: `Array<['c, null]>
  as [x, ..]` over `[Array<'b>, i64]` gives `x: 'b` (`['b, null]` where
  no `_` arm follows). The arm's body does not decide `'c` through `x`
  (`x$ + 1` is refused: test the name, `select x { i64 as n => .. }`);
  another part of the test does, and then `'c` is tested: `(Array<'c>,
  Array<['c, null]>)` over `(Array<i64>, Array<'b>)` passes only an
  `Array<[i64, null]>` there. A slice over a constructor variable's application
  (`'c<'b>`, a `c: Collection` parameter) stands over `Array<'b>` (a list
  pattern, `List<'b>`): `[h, ..]` gives `h: 'b`, and a pattern inside the
  element needs `Array<..> as [..]`.
  An abstract type's parameter takes no annotation wider than the
  variable, and a type test of an abstract compares its parameters:
  `W<(i64, string)> as ..` matches only a `W` built at exactly that
  type, covering nothing of `W<'b>`. Bind the payload by a name and test
  that, `W(p) => select p { (i64, string) as (n, s) => n }`, or declare
  no `'b`. Beside other members (`(bool,
  ['b, null])`, `'b: [i64, string]`) the
  variable's values are covered by a name or `_` at its position, or by
  written tests that split its bound: `(false, null)` leaves `(false, v)`
  needing an arm of its own. A written test that holds part of a tuple
  or struct covers that part beside other arms' bool literals and
  variant heads; one naming a declared variable holds only that
  variable, never its bound, and covers nothing of another declared
  variable: `Array<'b> as` over `Array<'a>` (a map's, a list's, an
  error's or an abstract's parameter alike) needs an `Array<'a> as` arm
  or a catch-all after it, since `'a` and `'b` may differ in a call.
- A type not decided yet where a select is checked (a `never()` let
  written after it, a function's unannotated parameter) is one unknown
  type there, equal only to itself. An arm that tests a value of it
  reaching the arm is refused, naming the binding (an earlier arm that
  takes its member whole, `` (`J, _) `` over `` [(`K, i64), (`J, z)] ``,
  leaves the later arms free of it): over `let v = select b { true => {a: [1],
  c: 1}, false => z }`, `{a, ..} => ..` is refused until `z` is annotated.
  An arm whose pattern would decide it is refused the same way: `|x|
  select (x, 0) { (`J, _) => 1, (`M, _) => 2 }` (write `x: [`J, `M]`),
  and through a reference or an abstract's payload alike (`select *r`
  over `let r = &mut (x, 0)`, `select W((x, 0)).0`), whether the arms
  would decide the part or leave it open (``[`J, _] => 1, _ => 3`` over
  `let r = &mut [x, 0]`), and there too an earlier unguarded arm that
  takes a member whole frees the later arms of it (`(_, 0)` behind
  `` (`K, _) `` over `` &mut (select b { true => (`K, x), false => (`L,
  0) }) ``, where `x` stays open: the function takes any `x`), and so do
  arms that take it together (`(true, _)` and `(false, _)` over `(b,
  x)`); so is a literal, `null`, `[]`, a fixed-length slice or an
  or-pattern there. A name at
  a position of a tuple, struct or variant takes the part whole as a `_`
  does, so `` (`K, n, _) `` leaves `x` and `y` open beside `(_, 0, _)`
  and `(_, _, "a")` over `` (`K, x, y) ``; a member built into the
  reference that no test takes whole and two could take is refused,
  "could go to", until its cells are annotated. A pattern
  that only takes it apart decides its shape: `((a, b), _)`, `({a, b},
  _)`, `([h, ..], _)`, `([<h, t..>], _)`, `(W(p), _)` check, unless
  the part stands beside a member of that shape (`y` is `x` or a pair):
  the destructure then chooses between them, refused.
  Where the tests beside a catch-all cannot hold every member built
  into the reference, it is what was built, and each bind takes its part
  of the members its arm takes, as over the select written in place:
  `` (`K, n, _) => n + 1, (_, _, m) => str::len(m) `` over `` &mut
  (select b { true => (`K, "s", "t"), false => (`L, 0, "a") }) `` is
  refused, since `n` is a string.
  The referent of `&mut (x, 0)` is a type of its own, which ``x: [`J,
  `M]`` leaves open: where the scrutinee holds it as a part (`select
  (*r, 1)`), the check names "the type `r` refers to", to annotate as
  ``r: &mut ([`J, `M], i64)``.
  A rest leaves the other lengths to a later name or `_`: a later `([],
  _)` is a length choice, refused.
  Only a scrutinee wholly of such a type is the arms' to decide together
  (`|x| select x { `J => 1, `M => 2 }`, and `select W(x).0` alike), never
  by a partial struct pattern (`{a, ..}`).
  A `_` at a part another arm of the same shape binds a name at stands
  as an unused name does: `(1, _) => 0, (_, b) => b + 1` takes `(i64,
  i64)`, and a call whose part is wider (`(2, "a")`) is refused as the
  name's twin is, so annotate the parameter. The same holds for a `_`
  inside a tuple, struct or variant standing at that part: `(1, v) => 0,
  (2, (_, "a")) => 1, (4, v) => v.0 + 1` makes `v` `(i64, string)`.
  A part only a name or `_` meets passes, and a name there holds that
  type beside the others: `(_, x)` over `` [(`A, z), (`B, "s")] `` with
  `str::len(x)` makes `z` a string, and `(a, _)` over `[(e1, 1), (e2,
  "s")]` with `a[0]$ + 1` makes `e1` and `e2` both `Array<i64>`, so a
  later `e2 <- ["s"]` is refused. A written test is closed: `` ['b,
  `A(i64)] as `A(n) `` with `'b` declared nowhere is refused, whatever
  reaches the arm (`Any`, an array's element: `` Array<['b, `A(i64)]> as
  [`A(n)] ``); a name over `'b` passes, and so does a destructure that
  takes apart all of it (`Array<'b> as [(a, b), ..]`). A written test
  that tests the undecided part is refused as a pattern is:
  `` (`J, [i64, string]) as (`J, s) `` over `` (`J, z) ``. A written
  test over a scrutinee two of whose members are undecided (`u`, an `x`
  or a `y` both written later, with `i64 as n => n, _ => 0`) could go
  to either, and is refused, "could each take the test": annotate one,
  and the other is the test's to decide, or stays open where the
  annotation holds the test. With no catch-all the tests decide both,
  each the union of the tests. Two such members that have no binding of
  their own (two `str::parse` results in one `let u`) are refused by
  the name of the binding that holds them: annotate it, `let u: [i64,
  string] = ..`.
- Arms that bind one part of a scrutinee the arms decide bind one type
  there: `(1, a) => .., (2, a) => ..` and `` `A(1, a) => .., `A(2, a) =>
  .. `` give both `a` the part's one type, since no literal or guard
  beside a bind narrows it. Using it as an `i64` in one arm and a
  `string` in the other is refused where the select is written
  ("string does not contain i64"). Annotate the scrutinee with the
  union and test the bind inside the arm.
- A bind behind arms that test its part holds that part of every member
  reaching its arm, whatever the order of the arms: `(1, 1) => .., (3,
  "x") => .., (4, v) => ..` over an unannotated parameter gives `v`
  `[i64, string]`, so `v + 1` is refused; test it inside the arm
  (`select v { i64 as n => n + 1, s => str::len(s) }`). In a variant's
  payload too: `` `A(1, a) => a + 1, `A(2, "x") => 0, _ => 0 `` is
  refused, since `` `A(1, "x") `` reaches the first arm; an earlier
  unguarded arm that takes a member whole leaves it out (`` `A(null) =>
  0, `A(a) => a + 1 `` checks), one that tests a literal at another part
  does not (`` `A(1, null) => 0, `A(1, a) => a + 1 `` is refused).
- A slice arm with a rest over a `never()` let the arms decide leaves the
  other elements' type to later writes, and takes a value of whatever
  they bring at every position. A write that brings a member an element's
  pattern takes and its binds were not typed over is refused at the
  pattern: after `let v = never(); let z = select v { [`J(n), ..] => n +
  1, _ => 3 }`, where `n` is `i64`, `` v <- [`J("s"), `K] `` is refused
  ("takes a value of `` `J(string) `` too"). Annotate the let, ``let v:
  Array<[`J(i64), `J(string), `K]> = never()``, and `n` is `[i64,
  string]`. A member the pattern cannot take (`` v <- [`J(1), `K] ``)
  checks. A write of a declared variable brings every member of its
  bound (`v <- x` over `x: Array<'a>`, ``'a: [`J(string), `K]``), and
  one of a variable with no bound is refused wherever the pattern binds
  a part.
- Over `Any` (a scrutinee, a tuple position, a field, a payload) a name
  is `Any`, and a pattern that takes the value apart is a run-time test
  of the types its arm decides: `((a, b), n) => a + b + n` over `(Any,
  i64)` takes only a pair of `i64`. The arm is its guard and body, and
  nothing outside it reaches the test (a return type, a later statement,
  a caller): a part the arm leaves open is `Any` (`(a, b) => a` gives
  `Any`), and one it constrains without deciding is refused, `(a, b) =>
  a + b` being a number of no one type. Write the test, `(i64, i64) as
  (a, b)`, or decide it in the arm (`a + b + 1`). A part the arm decides to a
  function type is refused, as a written test of one is: a test tells a
  function from a value, not one signature from another. A written test's own
  variable there is the arm's alike (`('q, i64) as (r, n) => r + n`
  tests an `i64`). A declared variable with a bound is no test there (a
  call that leaves it undecided would test nothing): `('t, 't) as (a,
  b)` under `'t: Number + Singleton` is refused, and so is a bind used
  at it (`(a, b) => a + y` beside `y: 't`); one with no bound is each
  call's type for it. A slice tests every element against
  the union of its elements' types, so with a `_` among them, or a rest
  beside no name, it tests none and every name under it is `Any`: `[(a,
  b), _] => a + 1` and `[(a, b), ..] => a + 1` are refused. Write the
  test, `Array<[(i64, i64), string]> as [(a, b), _]`. A rest beside a
  name takes the name's type: `[h, ..] => h + 1` takes an array of `i64`
  only. The same holds inside elements typed above an `Any` part: over
  `Array<(Any, i64)>`, `[((a, _), n), ..] => a + 1` is refused (`a` is
  `Any`, `n` is `i64`); write `Array<((i64, i64), i64)> as [((a, _), n),
  ..]`. A slice of one element (`[((a, _), n)]`) is its own test. A
  partial struct pattern there has no struct type to complete it and is
  refused, under a tuple, variant or slice too: `(_, {a, ..})` over
  `(i64, Any)`. Write the test, `(i64, {a: i64, b: i64}) as (_, {a,
  ..})`. Where the rest of the pattern cannot match the member whose
  part is `Any`, the struct is the other member's and nothing is
  refused: `` (`K, {a, ..}) `` over `` [(`K, {a: i64, b: i64}), (`J,
  Any)] ``.
- An abstract's constructor pattern tests the tag and the payload's
  shape, never the type of a part, so its arm decides nothing: over
  `W<Any>` or `Any`, each name `W((a, b))` takes out is `Any`, and `a +
  1` is refused. Select on the name (`select a { i64 as i => .. }`).
  A whole capture over `Any` (`w@ W(_)`) is `W<Any>` the same way; a
  parameter with a declared bound is its bound there. Where the
  representation holds a parameter under a `&mut`, a function's argument
  or another abstract type (`Abstract<&mut 'a>`), `W<Any>` does not hold
  a `W<i64>`, so a name taken out of `W(..)` over `Any` is refused: write
  the instance, `W<i64> as W(r)`. `W(_)` naming nothing still tests the
  tag.
- A partial struct pattern over members whose left-out fields differ
  only in types not decided at the select takes them for two members and
  is refused: `{a, ..}` over `{a: [1], c: x}` and `{a: ["s"], c: y}`,
  `x` and `y` written later. Name every field (`{a, c: _}`), annotate the
  member, or annotate `x` and `y`. The refusal shows a type no binding
  names (a `[]`'s element, a call's result) as `'_1`: annotate where the
  struct is built. Two calls of one generic function are two types,
  `'_1` and `'_2`, though the function wrote one `'a`. A destructuring
  `let` (`let {a, ..} = v`, "not decided at this let") and a parameter
  are refused alike: annotate `x` and `y`, the binding (`let {a, ..}: T
  = v`), or name every field.
- A partial struct pattern over members whose completions differ
  (`{a, ..}` over `{a: i64, b: string}` and `{a: i64, c: f64}`) matches
  more than one member too. A select takes the member (`{a: i64, b:
  string} as {a, ..}`); a `let` or a parameter takes every member at
  once, so it names every field where the members share theirs, and
  binds the value whole and selects on it where they do not.
- No arm may tell apart two types that share a runtime form: two
  reference types, two function types. Wrap them in distinct variants.
  A name tests nothing, so an arm may bind such a part, and the bind
  holds every member the arm's own tests admit: `(r, n)` over two tuples
  that hold different function types gives `r` both, `` (`A, f) `` the
  `` `A `` member's alone. A written test's own variable binds
  such a part the same way (`('q, i64) as (r, n)`, with no arm beside
  it, or a `_` arm another member reaches), written once or twice
  (`('q, 'q) as (r, s)` over `(fa, fa)` and `(fb, fb)`); written twice it
  is tested at one part as the type another decided, and refused where
  that part holds a function type the variable did not take. Where no member that
  reaches the arm holds it at one type at both parts (`('q, 'q)` over
  `(null, i64)`) and the arm does not decide it, a name bound at it is
  refused ("binds a0 at 'q"): write a variable for each part, `('q,
  'r)`, and each binds every member's part. A union the test writes binds every
  member it holds (`([&mut i64, &mut string], i64) as (r, n)`), and is
  refused where a reference it does not hold reaches the arm.
  A test over a tuple, struct or variant is refused for such a part only
  where every other part of it passes a value of the other member: over
  `[(&i64, i64), (&string, string)]`, `(&i64, i64) as (r, n)` checks,
  since no value of the second member passes the `i64`. An array, list or
  map part tells nothing (an empty one passes the test for any element
  type), nor does a part the members share (`[i64, null]` beside
  `[string, null]`), a declared variable or `Any`.
  Everything else is told apart: a string from a bare variant, an array
  from a tuple, struct, variant or list, `[]` from `[<>]`. A type
  variable may be any type, so a test beside one is refused where an
  instance could share the tested form: `` `Keep(r) `` over `` ['b,
  `Keep('b)] ``, and a slice element alike, `` [`Keep(r), ..] `` over
  `` Array<['b, `Keep('b)]> `` (`[h, ..]` tests nothing and checks). Over
  an unannotated parameter a slice's rest is such a variable: annotate
  the parameter.

```graphix,cases
no "matches more than one member" |x: [{a: i64, b: string}, {a: i64, c: f64}]| select x { {a, ..} => a }
no "do not share their fields" let v: [{a: i64, b: string}, {a: i64, c: f64}] = {a: 1, b: "s"}; let {a, ..} = v; a
no "name every field of { a, .. }" let t = true; let x: string = never(); let y = never(); let v = select t { true => {a: [1], c: x}, false => {a: ["s"], c: y} }; let {a, ..} = v; a
ok let t = true; let x: string = "q"; let y = never(); let v = select t { true => {a: [1], c: x}, false => {a: ["s"], c: y} }; let {a, c} = v; a
ok let v: [{a: i64, b: string}, {a: i64, c: f64}] = {a: 1, b: "s"}; select v { {a: i64, b: string} as {a, ..} => a, _ => 0 }
ok let f = |s| select s { (1, _) => 0, (_, b) => b + 1 }; f((2, 5))
ok let f = |s| select s { ([], _) => 0, ([x, ..], b) => str::len(b) }; f(([1], "q"))
ok let f = |s| select s { (1, v) => 0, (2, (_, "a")) => 1, (_, (_, "a")) => 2, (4, v) => v.0 + 1, _ => 9 }; f((4, (1, "a")))
no "type mismatch" let f = |s| select s { (1, _) => 0, (2, b) => b + 1, (_, b) => str::len(b) }; f((2, 5))
no "tests inside" 'a: [{a: i64, b: string}, i64] |x: 'a| select x { {a, ..} => a, _ => 0 }
ok 'a: [{a: i64, b: string}, i64] |x: 'a| select x { {a: i64, b: string} as {a, ..} => a, i64 as n => n }
no "tests inside" |x: ['a, null]| select x { null => 0, _ => 1 }
ok |x: ['a, null]| select x { null as _ => 0, _ => 1 }
no "types `w` at `_`" |z: 'a| select z { _ as w => w + 1, _ => 0 }
no "types `w` at `_`" 'a: [i64, string] |z: 'a| select z { _ as w => w + 1, _ => 0 }
no "types `w` at `_`" |z: [(i64, 'a), (string, 'a)]| select z { (i64, _) as (n, w) => n + w, _ => 0 }
no "types `p` at `_`" |z: [(i64, 'a), (string, 'a)]| select z { (i64, _) as p => p.0, _ => 0 }
ok |z: [(i64, 'a), (string, 'a)]| select z { (i64, Any) as p => p.0, _ => 0 }
ok |z: 'a| select z { Any as w => 1, _ => 0 }
ok |z: ('a, i64)| select z { (_, i64) as (w, n) => n, _ => 0 }
no "types none of it" 'b: [`A(i64), `B] |x: 'b| select x { 'c as `A(z) => 1, _ => 2 }
no "types none of it" 'b: [`A(i64), `B] |x: 'b| select x { `A(_) as `A(z) => 1, _ => 2 }
no "[`J, _] tests inside a value of 'a" |x: 'a| { let r = &mut [x, 0]; select *r { [`J, _] => 1, _ => 3 } }
no "[`J] tests inside a value of 'a" |x: 'a| select [x] { [`J] => 1, _ => 3 }
ok 'a: [`J, `M] |x: 'a| { let r = &mut [x, 0]; select *r { Array<[i64, `J, `M]> as [`J, _] => 1, _ => 3 } }
no "[`J, _] tests inside a value of 'a" type W<'a> = Abstract<'a>; |x: 'a| select W([x, 0]).0 { [`J, _] => 1, _ => 3 }
no "types none of it" 'b: [`A(i64), `B] |x: 'b| { let r = &mut (x, 0); select *r { (`A(_), i64) as (`A(z), _) => 1, _ => 2 } }
no "`J tests inside a value of the trait Show" trait Show { val show: fn(self) -> string }; |s: Show| select s { `J => 1, _ => 2 }
ok 'a: [`J, i64] |s: 'a| select s { [`J, i64] as `J => 1, _ => 2 }
no "`J tests inside a value of self" trait Tag { val tag: fn(self) -> i64 = |x| select x { `J => 1, _ => 2 } }; 1
no "((a, b), c) binds inside a value of 'a" 'a: (i64, i64) |x: 'a| { let r = &mut (x, 0); let ((a, b), c) = *r; a + b }
ok 'a: (i64, i64) |x: 'a| { let r = &mut (x, 0); let ((a, b), c): ((i64, i64), i64) = *r; a + b }
ok |x: (bool, Any)| select x { (_, `Z) => 3, (true, _) => 1, (false, _) => 2 }
ok |b: bool, c: bool| { let r = &mut (select c { true => (b, "s"), false => (`L, `Z) }); select *r { (true, _) => 1, (false, _) => 2, (_, `Z) => 3 } }
no "does not decide the type of `a`" |x: Any| select x { (a, b) => a + b, _ => 0 }
no "does not decide the type of `a`" |x: Any| -> i64 select x { (a, b) => a + b, _ => 0 }
ok |x: Any| select x { (i64, i64) as (a, b) => a + b, _ => 0 }
ok |x: Any| select x { (a, b) => a + b + 1, _ => 0 }
no "can't match on a type holding a function" |x: Any| select x { (g, b) => { let h: fn(x: i64) -> i64 = g; h(1) }, _ => 0 }
no "can't match on a type holding a function" |x: Any| select x { (g, b) => array::map([1], g), _ => [] }
ok |x: Any| select x { (g, b) => 1, _ => 0 }
no "cannot compute Any + i64" let f = |x: Any| select x { (a, b) => a, _ => never() }; let v: Any = (1, 2); f(v) + 1
no "tests the declared variable 't inside a value that may be Any" 't: Number + Singleton |x: Any| -> 't select x { ('t, 't) as (a, b) => a + b, _ => never() }
no "tests the declared variable `a` is of" 't: Number + Singleton |x: Any, y: 't| select x { (a, b) => a + y, _ => y }
ok |x: Any, y: 't| select x { ('t, i64) as (r, n) => [r, y], _ => [y] }
no "cannot compute [i64, string] + i64" |s| select s { (1, 1) => 0, (3, "x") => 2, (4, v) => v + 1, (_, _) => 9 }
ok let f = |s| select s { (1, 1) => 0, (3, "x") => 2, (4, v) => select v { i64 as n => n + 1, s => str::len(s) }, (_, _) => 9 }; f((4, "yy"))
no "cannot compute string + i64" |s| select s { `A(1, a) => a + 1, `A(2, "x") => 0, _ => 0 }
no "cannot compute string + i64" |s| select s { `A(2, "x") => 0, `A(1, a) => a + 1, _ => 0 }
ok let f = |s| select s { `A(null) => 0, `A(a) => a + 1 }; (f(`A(null)), f(`A(3)))
ok let f = |s| select s { `Ok(null) => "none", `Ok(v) => v, `Err(e) => e }; f(`Ok("x"))
no "cannot compute null + i64" |s| select s { `A(1, null) => 0, `A(1, a) => a + 1, _ => 5 }
no "cannot compute string + i64" let b = true; let r = &mut (select b { true => (`K, "s", "t"), false => (`L, 0, "a") }); select *r { (`K, n, _) => n + 1, (_, _, m) => str::len(m) }
ok let b = true; let r = &mut (select b { true => (`K, "s", "t"), false => (`L, 0, "a") }); select *r { (`K, n, _) => str::len(n), (_, _, m) => str::len(m) }
no "not decided at this select" |x| select (x, 0) { (`J, _) => 1, (`M, _) => 2 }
ok |x: [`J, `M]| select (x, 0) { (`J, _) => 1, (`M, _) => 2 }
no "could each take the test" let x = never(); let y = never(); let u = select true { true => x, false => y }; select u { i64 as n => n, _ => 0 }
ok let x = never(); let y: string = never(); let u = select true { true => x, false => y }; select u { i64 as n => n, _ => 0 }
ok let x = never(); let y = never(); let u = select true { true => x, false => y }; select u { i64 as n => n, string as s => 0 }
no "'u, a type not decided at this select, members of which could each take the test" let u = select true { true => str::parse("1")$, false => str::parse("s")$ }; select u { i64 as n => n, _ => 0 }
ok let u: [i64, string] = select true { true => str::parse("1")$, false => str::parse("s")$ }; select u { i64 as n => n, _ => 0 }
no "string does not contain" 'c: Collection, 'b: (i64, i64) |x: 'c<'b>| -> [string, null] select x { [h, ..] => { let q: string = h; q }, _ => null }
ok let g = 'c: Collection, 'b: (i64, i64) |x: 'c<'b>| -> [i64, null] select x { [h, ..] => h.0, _ => null }; g([(3, 4)])
no "string does not contain" 'b: (i64, i64) |x: (Array<'b>, i64)| -> [string, null] select x { ([h, ..], n) => { let q: string = h; q }, _ => null }
ok let g = 'b: (i64, i64) |x: {a: Array<'b>, n: i64}| -> i64 select x { {a: [h, ..], n} => h.0 + n, _ => 0 }; g({a: [(3, 4)], n: 1})
no "not decided at this select" type W<'a> = Abstract<'a>; |x| select W((x, 0)).0 { (`J, _) => 1, (`M, _) => 2 }
no "Array<(i64, i64)> as [(a, b), ..]" 'c: Collection, 'b: (i64, i64) |x: 'c<'b>| -> i64 select x { [(a, b), ..] => a, _ => 0 }
no "a type its scrutinee was given after this select" let v = never(); let z = select v { [`J(n), ..] => n + 1, _ => 3 }; v <- [`J("s"), `K]; z
ok let v = never(); let z = select v { [`J(n), ..] => n + 1, _ => 3 }; v <- [`J(1), `K]; z
ok let v: Array<[`J(i64), `J(string), `K]> = never(); let z = select v { [`J(n), ..] => select n { i64 as i => i + 1, string as s => str::len(s) }, _ => 3 }; v <- [`J("abc"), `K]; z
no "takes a value of `J(string) too" let f = 'a: [`J(string), `K] |x: Array<'a>| { let v = never(); let z = select v { [`J(n), ..] => n + 1, _ => 3 }; v <- x; z }; f([`J("s")])
ok let f = 'a: [`L(string), `K] |x: Array<'a>| { let v = never(); let z = select v { [`J(n), ..] => n + 1, _ => 3 }; v <- x; z }; f([`L("s")])
no "typed the pattern's binds for W<i64> alone" type W<'a> = Abstract<'a>; let f = |x| { let v = never(); let z = select v { [W(p), ..] => p + 1, _ => 3 }; v <- [x, W(1)]; z }; f(W("s"))
no "string does not contain i64" let f = |s| select s { `A(1, a) => a + 1, `A(2, a) => str::len(a), _ => 0 }; f(`A(1, "q"))
ok let f = |s| select s { `A(1, a) => a, `A(2, a) => a, _ => never() }; (f(`A(1, "q")), f(`A(2, 5)))
ok let f = |s: `A(i64, [i64, string])| select s { `A(_, a) => select a { i64 as n => n + 1, string as t => str::len(t) } }; (f(`A(1, 5)), f(`A(2, "q")))
no "is a part of 'x" |x| select (x[0]$, 0) { (`J, _) => 1, _ => 2 }
ok |x: Array<[`J, `M]>| select (x[0]$, 0) { (`J, _) => 1, _ => 2 }
ok |x: (bool, [i64, null])| select x { (true, _) => 0, (bool, i64) as (false, n) => n, (bool, null) as (false, _) => 1 }
no "missing match cases" |x: (bool, [i64, string, null])| select x { (true, _) => 0, (bool, i64) as (false, n) => n, (false, null) => 3 }
no "unreachable arm" type E = [{a: i64, d: i64}, {c: i64}]; |x: (i64, E)| select x { (n, {a: _, d: _}) => 2, (n, {a: h, d: _}) => h, (n, {c: _}) => 1 }
no "unreachable arm" type E = [{a: i64, d: i64}, {c: i64}]; |x: (i64, E)| select x { (n, {a: _, d: _}) => 2, (n, {c: _}) => 1, _ => 3 }
ok type E = [{a: bool, d: i64}, {c: bool}]; |x: (i64, E)| select x { (n, {a: true, d: _}) => 1, (n, {a: false, d: _}) => 2, (n, {c: true}) => 3, (n, {c: false}) => 4 }
ok |x: [`A([bool, Array<i64>])]| select x { `A(true) => 0, `A(false) => 1, `A(Array<i64>) as `A(a) => 2 }
ok |x: [`A([bool, Array<i64>])]| select x { `A(true) => 0, `A(false) => 1, `A([a, ..]) => 2, `A([]) => 3 }
ok |x: ([bool, Array<i64>], i64)| select x { (true, _) => 0, (false, _) => 1, ([a, ..], _) => 2, ([], _) => 3 }
no "missing match cases: no arm covers `A(bool)" |x: [`A([bool, Array<i64>])]| select x { `A(true) => 0, `A(Array<i64>) as `A(a) => 2 }
ok 'b: [i64, string] |x: (bool, ['b, null])| select x { (true, _) => 0, (bool, i64) as (false, n) => 1, (bool, string) as (false, s) => 2, (false, null) => 3 }
ok 'b: [i64, string] |x: (bool, ['b, null])| select x { (true, _) => 0, (bool, 'b) as (false, v) => 1, (false, null) => 3 }
no "missing match cases" 'b: [i64, u8] |x: (bool, [i64, u8]), y: 'b| select x { (bool, 'b) as (true, a) => 1, (false, _) => 2 }
ok |x| select (x, 0) { ((a, b), _) => a }
no "not decided at this let" let b = true; let x = never(); let y = never(); let v = select b { true => {a: [1], c: x}, false => {a: [2], c: y} }; let {a, ..} = v; x <- 1; y <- "w"; a
ok let b = true; let x = never(); let y = never(); let v = select b { true => {a: [1], c: x}, false => {a: [2], c: y} }; let {a, c: _} = v; x <- 1; y <- "w"; a
no "not decided at this select" |x| { let r = &mut (x, 0); select *r { (`J, _) => 1, (`M, _) => 2 } }
ok |x: [`J, `M]| { let r = &mut (x, 0); select *r { (`J, _) => 1, (`M, _) => 2 } }
ok |x| { let r = &mut (x, 0); select *r { ((a, b), _) => a } }
no "[`J, _] tests a value of 'x" |x| { let r = &mut [x, 0]; select *r { [`J, _] => 1, _ => 3 } }
ok |x: [`J, `M]| { let r = &mut [x, 0]; select *r { [`J, _] => 1, _ => 3 } }
ok |x| { let r: &mut Array<[`J, `M, i64]> = &mut [x, 0]; select *r { [`J, _] => 1, _ => 3 } }
ok |x, b: bool| { let r = &mut (select b { true => (`K, x), false => (`L, 0) }); select *r { (`K, _) => 1, (_, 0) => 2, _ => 3 } }
ok let f = |x, b: bool| { let r = &mut (select b { true => (`K, x), false => (`L, 0) }); select *r { (`K, _) => 1, (_, 0) => 2, _ => 3 } }; (f("s", true), f(0, true))
ok let f = |x, b: bool| { let r = &mut (select b { true => (x, `K), false => (0, `L) }); select *r { (_, `K) => 1, (0, _) => 2, _ => 3 } }; (f("s", true), f(0, true))
ok |x, y, b: bool| { let r = &mut (select b { true => (`K, x, y), false => (`L, 0, "a") }); select *r { (`K, n, _) => n, (_, 0, _) => 2, (_, _, "a") => 3, _ => 4 } }
ok |x: i64, y: string, b: bool| { let r = &mut (select b { true => (`K, x, y), false => (`L, 0, "a") }); select *r { (`K, n, _) => n, (_, 0, _) => 2, (_, _, "a") => 3, _ => 4 } }
ok let f = |x, b: bool, c: bool| { let r = &mut (select c { true => (b, x), false => (`L, 0) }); select *r { (true, _) => 1, (false, _) => 2, (_, 0) => 3, _ => 4 } }; (f("s", true, true), f(0, false, true))
no "(_, 0) tests a value of 'x" |x, b: bool, c: bool| { let r = &mut (select c { true => (b, x), false => (`L, 0) }); select *r { (true, _) => 1, (_, 0) => 3, _ => 4 } }
no "(_, 0) tests a value of 'x" |x, b: bool| { let r = &mut (select b { true => (`K, x), false => (`L, 0) }); select *r { (_, 0) => 2, (`K, _) => 1, _ => 3 } }
no "(_, 0) tests a value of 'x" |x, b: bool| { let r = &mut (select b { true => (`K, x), false => (`L, 0) }); select *r { (`K, _) if b => 1, (_, 0) => 2, _ => 3 } }
no "the type `r` refers to" |x| { let r = &mut (x, 0); select (*r, 1) { ((`J, _), _) => 1, ((`M, _), _) => 2 } }
no "the type `r` refers to" |x: [`J, `M]| { let r = &mut (x, 0); select (*r, 1) { ((`J, _), _) => 1, ((`M, _), _) => 2 } }
ok |x| { let r: &mut ([`J, `M], i64) = &mut (x, 0); select (*r, 1) { ((`J, _), _) => 1, ((`M, _), _) => 2 } }
ok type W<'a> = Abstract<'a>; |x| select W(x).0 { `J => 1, `M => 2 }
ok |x| select x { `J => 1, `M => 2 }
no "a type variable its type test" |xs: Array<[i64, `A(i64)]>| select xs { Array<['b, `A(i64)]> as [`A(n)] => n, _ => 0 }
no "a type variable its type test" |x: Any| select x { ['b, `A(i64)] as `A(n) => 1, _ => 0 }
ok |xs: Array<[i64, `A(i64)]>| select xs { Array<[i64, `A(i64)]> as [`A(n)] => n, _ => 0 }
ok |xs: Array<[i64, `A(i64)]>| select xs { Array<'b> as [a, ..] => 1, _ => 0 }
ok let f = |v: [Array<'b>, i64]| -> 'b select v { Array<['c, null]> as [x, ..] => x, _ => never() }; f(["s"])
no "[i64, null] does not contain string" let f = |v: [Array<'b>, i64]| select v { Array<['c, null]> as [x, ..] => x, _ => never() }; let y: [i64, null] = f(["s"]); y
no "not known here" let f = |v: [Array<'b>, i64]| select v { Array<['c, null]> as [x, ..] => x$ + 1, _ => 0 }; f([1])
ok let f = |v: [Array<'b>, i64]| select v { Array<['c, null]> as [x, ..] => select x { i64 as n => n + 1, _ => 0 }, _ => 0 }; f([1])
ok |xs: Array<(i64, i64)>| select xs { Array<'b> as [(a, b), ..] => a + b, _ => 0 }
no "no arm covers Array<'a" |x: [Array<'a>, i64], y: 'b| select x { Array<'b> as a => 1, i64 as n => n }
ok let f = |x: [Array<'a>, i64], y: 'b| select x { Array<'b> as a => 1, Array<'a> as a => 2, i64 as n => n }; [f([1], "s"), f([1], 1)]
no "cannot hold Array<string>" let e1 = []; let e2 = []; let b = true; let t = select b { true => (e1, 1), false => (e2, "s") }; let z = select t { (a, n) => a[0]$ + 1 }; e1 <- [1]; e2 <- ["s"]; z
ok let e1 = []; let e2 = []; let b = true; let t = select b { true => (e1, 1), false => (e2, "s") }; let z = select t { (a, n) => a[0]$ + 1 }; e1 <- [1]; e2 <- [2]; z
no "same runtime form" let a: [&i64, &string] = &1; select a { &i64 as x => 1, &string as y => 2 }
ok let b = true; let t = select b { true => (|x: i64| x, 1), false => (|y: string| y, "s") }; select t { (r, n) => 0 }
ok let b = true; let t = select b { true => (`A, |x: i64| x), false => (`B, |y: string| y) }; select t { (`A, f) => "[f(1)]", (`B, g) => g("s") }
no "this callee is one of" let b = true; let t = select b { true => (|x: i64| x, 1), false => (|y: string| y, 2) }; select t { (r, n) => r(n) }
no "binds a0 at 'q" let u = select 2 { 0 => (null, 7), 1 => (null, "s"), _ => (7, [1, 2]) }; select u { ('q, 'q) as (a0, a1) => a0, _ => null }
ok let u = select 2 { 0 => (null, 7), 1 => (null, "s"), _ => (7, [1, 2]) }; select u { ('q, 'r) as (a0, a1) => a0 }
ok let u = select 2 { 0 => (null, 7), 1 => (null, "s"), _ => (7, 8) }; select u { ('q, 'q) as (a0, a1) => a0 + 1, _ => 0 }
ok let v = 0; let t = select v { 0 => null, 1 => (|x: i64| x, 1), _ => (|y: string| y, 2) }; select t { ('q, i64) as (r, n) => n, _ => 0 }
no "unreachable arm" let b = true; let t = select b { true => (|x: i64| x, 1), false => (|y: string| y, 2) }; select t { ('q, i64) as (r, n) => n, _ => 0 }
ok let b = true; let t = select b { true => (|x: i64| x, 1), false => (|y: string| y, 2) }; select t { ('q, i64) as (r, n) => n }
no "this callee is one of" let b = true; let t = select b { true => (|x: i64| x, 1), false => (|y: string| y, 2) }; select t { ('q, i64) as (r, n) => r(n), _ => 0 }
ok let b = true; let fa = |x: i64| x; let fb = |y: string| y; let t = select b { true => (fa, fa), false => (fb, fb) }; select t { ('q, 'q) as (r, s) => 7 }
no "same runtime form" let b = true; let fa = |x: i64| x; let fb = |y: string| y; let t = (fa, select b { true => fa, false => fb }); select t { ('q, 'q) as (r, s) => 0, _ => 1 }
ok let a = 1; let b = "s"; let v = 0; let t = select v { 0 => (&mut a, 1), _ => (&mut b, 2) }; select t { ([&mut i64, &mut string], i64) as (r, n) => n }
no "same runtime form" let a = 1; let b = "s"; let c = 2.5; let v = 0; let t = select v { 0 => (&mut a, 1), 1 => (&mut b, 2), _ => (&mut c, 3) }; select t { ([&mut i64, &mut string], i64) as (r, n) => n, _ => 0 }
ok let a = 1; let b = "s"; let v = true; let t = select v { true => (&a, 1), false => (&b, "s") }; select t { (&i64, i64) as (r, n) => *r + n, _ => 0 }
ok let v = true; let s = select v { true => {f: |x: i64| x, n: 1}, false => {f: |y: string| y, n: "s"} }; select s { {n: 1, ..} => 1, _ => 0 }
no "same runtime form" let a = 1; let b = "s"; let v = true; let e: Array<string> = []; let t = select v { true => ([1], &a), false => (e, &b) }; select t { (Array<i64>, &i64) as (x, r) => *r, _ => 0 }
no "same runtime form" let a = 1; let b = "s"; let v = true; let t = select v { true => (&a, 1), false => (&b, 2) }; select t { (&i64, i64) as (r, n) => *r + n, _ => 0 }
no "'_1 and '_2 types no binding names" let b = true; let v = select b { true => {a: [1], c: []}, false => {a: ["s"], c: []} }; select v { {a, ..} => 0 }
no "c: Array<'_1> } and { a: Array<string>, c: Array<'_2> }" let b = true; let f = |x: 'a| -> Array<'a> []; let v = select b { true => {a: [1], c: f(never())}, false => {a: ["s"], c: f(never())} }; select v { {a, ..} => 0 }
ok let b = true; let v: [{a: Array<i64>, c: Array<string>}, {a: Array<string>, c: Array<string>}] = select b { true => {a: [1], c: []}, false => {a: ["s"], c: []} }; select v { {a: Array<string>, c: Array<string>} as s => 0, {a, ..} => 1 }
ok |x: (Any, i64)| -> i64 select x { ((a, b), n) => a + b + n, _ => 0 }
no "cannot compute Any + i64" |x: Any| select x { [(a, b), _] => a + 1, _ => 2 }
no "cannot compute Any + i64" |x: Array<Any>| select x { [(a, b), ..] => a + 1, _ => 2 }
ok |x: Any| select x { Array<[(i64, i64), string]> as [(a, b), _] => a + 1, _ => 2 }
no "cannot compute Any + i64" |x: Any| select x { [(a, b), ..] => a + 1, _ => 2 }
ok |x: Any| select x { [h, ..] => h + 1, _ => 2 }
ok |x: Any| select x { Array<(i64, i64)> as [(a, b), ..] => a + 1, _ => 2 }
no "cannot compute Any + i64" |x: Array<(Any, i64)>| select x { [((a, _), n), ..] => a + 1, _ => 0 }
ok |x: Array<(Any, i64)>| select x { [((a, _), n), ..] => n + 1, _ => 0 }
ok |x: Array<(Any, i64)>| select x { [((a, _), n)] => a + n, _ => 0 }
ok |x: Array<(Any, i64)>| select x { Array<((i64, i64), i64)> as [((a, _), n), ..] => a + 1, _ => 0 }
no "has no struct type to complete it" |x: (i64, Any)| select x { (_, {a, ..}) => 1, _ => 0 }
ok |x: (i64, Any)| select x { (i64, {a: i64, b: i64}) as (_, {a, ..}) => a, _ => 0 }
ok |x: [(`K, {a: i64, b: i64}), (`J, Any)]| select x { (`K, {a, ..}) => a, _ => 0 }
no "has no struct type to complete it" |x: [(`K, {a: i64, b: i64}), (`J, Any)]| select x { (_, {a, ..}) => a, _ => 0 }
no "cannot compute Any + i64" type W<'a> = Abstract<'a>; |x: W<Any>| select x { W((a, b)) => a + 1, _ => 0 }
no "cannot compute Any + i64" type W<'a> = Abstract<'a>; |x: Any| select x { (W((a, b)), n) => a + 1, _ => 0 }
ok type W<'a> = Abstract<'a>; |x: W<Any>| select x { W((a, b)) => select a { i64 as i => i + 1, _ => 0 }, _ => 0 }
ok type W<'a> = Abstract<'a>; |x: W<(i64, i64)>| select x { W((a, b)) => a + 1 }
no "cannot compute Any + i64" type W<'a> = Abstract<Map<string, 'a>>; |x: W<Any>| select x { W(m) => m{"k"}$ + 1 }
no "cannot compute Any + i64" type W<'a> = Abstract<'a>; |x: Any| select x { w@ W(_) => w.0 + 1, _ => 0 }
ok type W<'a> = Abstract<'a>; |x: Any| select x { w@ W(_) => select w.0 { i64 as i => i + 1, _ => 0 }, _ => 0 }
ok type W<'a: Number> = Abstract<'a>; |x: Any| select x { W(p) => "[p]", _ => "o" }
no "no instance of W holds every other's payload" type W<'a> = Abstract<&mut 'a>; |x: Any| select x { W(r) => *r <- "s", _ => never() }
ok type W<'a> = Abstract<&mut 'a>; |x: Any| select x { W<i64> as W(r) => *r <- 5, _ => never() }
ok type W<'a> = Abstract<&mut 'a>; |x: Any| select x { W(_) => 1, _ => 0 }
```
