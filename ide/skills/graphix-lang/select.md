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
  (bool, i64)`, then `(bool, i64) as (true, k)`). The test is a closed
  type: `` `A(_) as `A(z) `` (`_` is bottom, through a typedef too:
  `` `A(T) `` over `type T = _`) and `` 'c as `A(z) `` (an undeclared
  variable) are refused. Under a written test a name over the
  variable's value keeps it where the test holds the scrutinee around it:
  `([`A(i64), `B(i64)], i64) as (v, n)` over `('b, i64)` gives `v: 'b`;
  at the top, `[`A(i64), `B(i64)] as v` over `'b` types `v` by the test,
  not as `'b`. A slice over a constructor variable's application
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
  variable, never its bound.
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
  over `let r = &mut (x, 0)`, `select W((x, 0)).0`); so is a literal,
  `null`, `[]`, a fixed-length slice or an or-pattern there. A pattern
  that only takes it apart decides its shape: `((a, b), _)`, `({a, b},
  _)`, `([h, ..], _)`, `([<h, t..>], _)`, `(W(p), _)` check, unless
  the part stands beside a member of that shape (`y` is `x` or a pair):
  the destructure then chooses between them, refused.
  The referent of `&mut (x, 0)` is a type of its own, which ``x: [`J,
  `M]`` leaves open: where the scrutinee holds it as a part (`select
  (*r, 1)`), the check names "the type `r` refers to", to annotate as
  ``r: &mut ([`J, `M], i64)``.
  A rest leaves the other lengths to a later name or `_`: a later `([],
  _)` is a length choice, refused.
  Only a scrutinee wholly of such a type is the arms' to decide together
  (`|x| select x { `J => 1, `M => 2 }`, and `select W(x).0` alike), never
  by a partial struct pattern (`{a, ..}`).
  A part only a name or `_` meets passes, and a name there holds that
  type beside the others: `(_, x)` over `` [(`A, z), (`B, "s")] `` with
  `str::len(x)` makes `z` a string. A written test is closed: `` ['b,
  `A(i64)] as `A(n) `` with `'b` declared nowhere is refused, whatever
  reaches the arm (`Any`, an array's element: `` Array<['b, `A(i64)]> as
  [`A(n)] ``); a name over `'b` passes, and so does a destructure that
  takes apart all of it (`Array<'b> as [(a, b), ..]`). A written test
  that tests the undecided part is refused as a pattern is:
  `` (`J, [i64, string]) as (`J, s) `` over `` (`J, z) ``.
- A partial struct pattern over members whose left-out fields differ
  only in types not decided at the select takes them for two members and
  is refused: `{a, ..}` over `{a: [1], c: x}` and `{a: ["s"], c: y}`,
  `x` and `y` written later. Name every field (`{a, c: _}`), annotate the
  member, or annotate `x` and `y`.
- No arm may tell apart two types that share a runtime form: two
  reference types, two function types. Wrap them in distinct variants.
  Everything else is told apart: a string from a bare variant, an array
  from a tuple, struct, variant or list, `[]` from `[<>]`. A type
  variable may be any type, so a test beside one is refused where an
  instance could share the tested form: `` `Keep(r) `` over `` ['b,
  `Keep('b)] ``, and a slice element alike, `` [`Keep(r), ..] `` over
  `` Array<['b, `Keep('b)]> `` (`[h, ..]` tests nothing and checks). Over
  an unannotated parameter a slice's rest is such a variable: annotate
  the parameter.

```graphix,cases
no "tests inside" 'a: [{a: i64, b: string}, i64] |x: 'a| select x { {a, ..} => a, _ => 0 }
ok 'a: [{a: i64, b: string}, i64] |x: 'a| select x { {a: i64, b: string} as {a, ..} => a, i64 as n => n }
no "tests inside" |x: ['a, null]| select x { null => 0, _ => 1 }
ok |x: ['a, null]| select x { null as _ => 0, _ => 1 }
no "types none of it" 'b: [`A(i64), `B] |x: 'b| select x { 'c as `A(z) => 1, _ => 2 }
no "types none of it" 'b: [`A(i64), `B] |x: 'b| select x { `A(_) as `A(z) => 1, _ => 2 }
no "not decided at this select" |x| select (x, 0) { (`J, _) => 1, (`M, _) => 2 }
ok |x: [`J, `M]| select (x, 0) { (`J, _) => 1, (`M, _) => 2 }
no "string does not contain" 'c: Collection, 'b: (i64, i64) |x: 'c<'b>| -> [string, null] select x { [h, ..] => { let q: string = h; q }, _ => null }
ok let g = 'c: Collection, 'b: (i64, i64) |x: 'c<'b>| -> [i64, null] select x { [h, ..] => h.0, _ => null }; g([(3, 4)])
no "string does not contain" 'b: (i64, i64) |x: (Array<'b>, i64)| -> [string, null] select x { ([h, ..], n) => { let q: string = h; q }, _ => null }
ok let g = 'b: (i64, i64) |x: {a: Array<'b>, n: i64}| -> i64 select x { {a: [h, ..], n} => h.0 + n, _ => 0 }; g({a: [(3, 4)], n: 1})
no "not decided at this select" type W<'a> = Abstract<'a>; |x| select W((x, 0)).0 { (`J, _) => 1, (`M, _) => 2 }
no "Array<(i64, i64)> as [(a, b), ..]" 'c: Collection, 'b: (i64, i64) |x: 'c<'b>| -> i64 select x { [(a, b), ..] => a, _ => 0 }
no "is a part of 'x" |x| select (x[0]$, 0) { (`J, _) => 1, _ => 2 }
ok |x: Array<[`J, `M]>| select (x[0]$, 0) { (`J, _) => 1, _ => 2 }
ok |x: (bool, [i64, null])| select x { (true, _) => 0, (bool, i64) as (false, n) => n, (bool, null) as (false, _) => 1 }
no "missing match cases" |x: (bool, [i64, string, null])| select x { (true, _) => 0, (bool, i64) as (false, n) => n, (false, null) => 3 }
ok 'b: [i64, string] |x: (bool, ['b, null])| select x { (true, _) => 0, (bool, i64) as (false, n) => 1, (bool, string) as (false, s) => 2, (false, null) => 3 }
ok 'b: [i64, string] |x: (bool, ['b, null])| select x { (true, _) => 0, (bool, 'b) as (false, v) => 1, (false, null) => 3 }
no "missing match cases" 'b: [i64, u8] |x: (bool, [i64, u8]), y: 'b| select x { (bool, 'b) as (true, a) => 1, (false, _) => 2 }
ok |x| select (x, 0) { ((a, b), _) => a }
no "not decided at this select" |x| { let r = &mut (x, 0); select *r { (`J, _) => 1, (`M, _) => 2 } }
ok |x: [`J, `M]| { let r = &mut (x, 0); select *r { (`J, _) => 1, (`M, _) => 2 } }
ok |x| { let r = &mut (x, 0); select *r { ((a, b), _) => a } }
no "the type `r` refers to" |x| { let r = &mut (x, 0); select (*r, 1) { ((`J, _), _) => 1, ((`M, _), _) => 2 } }
no "the type `r` refers to" |x: [`J, `M]| { let r = &mut (x, 0); select (*r, 1) { ((`J, _), _) => 1, ((`M, _), _) => 2 } }
ok |x| { let r: &mut ([`J, `M], i64) = &mut (x, 0); select (*r, 1) { ((`J, _), _) => 1, ((`M, _), _) => 2 } }
ok type W<'a> = Abstract<'a>; |x| select W(x).0 { `J => 1, `M => 2 }
ok |x| select x { `J => 1, `M => 2 }
no "a type variable its type test" |xs: Array<[i64, `A(i64)]>| select xs { Array<['b, `A(i64)]> as [`A(n)] => n, _ => 0 }
no "a type variable its type test" |x: Any| select x { ['b, `A(i64)] as `A(n) => 1, _ => 0 }
ok |xs: Array<[i64, `A(i64)]>| select xs { Array<[i64, `A(i64)]> as [`A(n)] => n, _ => 0 }
ok |xs: Array<[i64, `A(i64)]>| select xs { Array<'b> as [a, ..] => 1, _ => 0 }
ok |xs: Array<(i64, i64)>| select xs { Array<'b> as [(a, b), ..] => a + b, _ => 0 }
no "same runtime form" let a: [&i64, &string] = &1; select a { &i64 as x => 1, &string as y => 2 }
```
