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
  not as `'b`.
  An abstract type's parameter takes no annotation wider than the
  variable, and a type test of an abstract compares its parameters:
  `W<(i64, string)> as ..` matches only a `W` built at exactly that
  type, covering nothing of `W<'b>`. Bind the payload by a name and test
  that, `W(p) => select p { (i64, string) as (n, s) => n }`, or declare
  no `'b`. Beside other members (`(bool,
  ['b, null])`, `'b: [i64, string]`) the
  variable's values are covered by a name or `_` at its position, or by
  written tests that split its bound: `(false, null)` leaves `(false, v)`
  needing an arm of its own.
- A type not decided yet where a select is checked (a `never()` let
  written after it, a function's unannotated parameter) is one unknown
  type there, equal only to itself. An arm that tests a value of it
  reaching the arm is refused, naming the binding (an earlier arm that
  takes its member whole, `` (`J, _) `` over `` [(`K, i64), (`J, z)] ``,
  leaves the later arms free of it): over `let v = select b { true => {a: [1],
  c: 1}, false => z }`, `{a, ..} => ..` is refused until `z` is annotated.
  An arm whose pattern would decide it is refused the same way: `|x|
  select (x, 0) { (`J, _) => 1, (`M, _) => 2 }` (write `x: [`J, `M]`);
  so is a literal, `null`, `[]`, a fixed-length slice or an or-pattern
  there. A pattern that only takes it apart decides its shape: `((a, b),
  _)`, `({a, b}, _)`, `([h, ..], _)`, `([<h, t..>], _)`, `(W(p), _)` check,
  unless the part stands beside a member of that shape (`y` is `x` or a
  pair): the destructure then chooses between them, refused.
  A rest leaves the other lengths to a later name or `_`: a later `([],
  _)` is a length choice, refused.
  Only a scrutinee wholly of such a type is the arms' to decide together
  (`|x| select x { `J => 1, `M => 2 }`), never by a partial struct
  pattern (`{a, ..}`).
  A part only a name or `_` meets passes, and a name there holds that
  type beside the others: `(_, x)` over `` [(`A, z), (`B, "s")] `` with
  `str::len(x)` makes `z` a string. A written test is closed: `` ['b,
  `A(i64)] as `A(n) `` with `'b` declared nowhere is refused, and a
  written test that tests the undecided part is refused as a pattern is:
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
