# Select: patterns over types not fully known

Beyond `SKILL.md`'s Select: values not decided where the select stands,
declared type variables, `Any`, abstract types and runtime forms. What
the check refuses and what to write.

- A value not decided where a select stands (a `never()` let written
  after it, an unannotated parameter) is decided by what flows into it,
  anywhere in the program. The select waits, and its arms are judged
  against what the value turns out to be.
- A `let` is the union of what is written to it. An arm no written
  value reaches is dead, a `_` behind arms that take every written value
  included ("pattern `` `M `` will never match: nothing reaches it"),
  and a written value no arm takes is a missing case. Declare the let
  at the union its arms cover: ``let v: [`J, `M] = never()``.
- An unannotated parameter is what its arms take. With no arm that
  takes everything, a call that passes anything else is refused at the
  call ("missing match cases: nothing matches `` `K ``"); with a
  catch-all the parameter takes any value. A pattern that takes a value
  apart asks that shape of it: `((a, b), _)` over `(x, 0)` makes `x` a
  pair, `{a, ..}` any struct with a field `a`. An arm behind one that
  takes everything is dead whatever is passed. A scrutinee read through
  a reference or out of an abstract's payload is judged the same
  (`select *r` over `let r = &mut (x, 0)`, `select W((x, 0)).0`); a
  call that passes anything else is refused there as a type mismatch.
- A function a binding names is judged at each call. A lambda written
  in place is one instance, and a parameter of it that nothing is
  passed for and no arm decides is bottom: ``|x| select [x, 0] { [`J,
  _] => 1, _ => 3 }`` is refused, its first arm dead over an
  `Array<i64>`. Annotate the parameter, or name the function.
- A name an arm binds is the union of the parts that reach its arm. An
  earlier unguarded arm that takes a member whole leaves it out
  (`` `A(null) => 0, `A(a) => a + 1 `` checks); a literal or a guard
  beside a bind narrows nothing (`` `A(1, null) => 0, `A(1, a) => a + 1
  `` refuses a call that passes `` `A(1, null) ``). The arm's use of the
  name is asked of every such part: `(_, x) => str::len(x)` over
  `` [(`A, z), (`B, "s")] `` makes `z` a string, so a later `z <- 5` is
  refused.
- Arms that bind the same part of a parameter bind one type there, a
  `_` or a literal at that part included: `(1, a) => a + 1, (2, a) =>
  str::len(a)` checks and every call of it is refused, and `(1, _) =>
  0, (_, b) => b + 1` refuses `(1, "a")`. Each call is an instance, so
  a part the arms only pass on may differ between calls. Annotate the
  parameter with the union and test the bind inside the arm (`select a
  { i64 as n => .., string as s => .. }`).
- A write made after the select is held to how the arms use what they
  bind. After ``let v = never(); let z = select v { [`J(n), ..] => n +
  1, _ => 3 }``, `` v <- [`J(1), `K] `` checks and `` v <- [`J("s"),
  `K] `` is refused ("i64 does not contain string"). Annotate the let
  with the union, ``let v: Array<[`J(i64), `J(string), `K]> =
  never()``, and test `n` in the arm.
- A test asks no type of a value a type-directed operation makes: `let
  u = str::parse(s)$; select u { i64 as n => .. }` is refused,
  "annotate where its result goes". Annotate `u`.
- A value of a declared type variable is judged by its bound. Patterns
  test and bind inside it, and a name there is its part of the bound:
  under `'a: [{a: i64, b: string}, i64]`, `{a, ..} => a` gives an
  `i64`. Arms that cover the bound cover the variable: a `_` behind
  them is dead, and what they leave is named as the variable ("missing
  match cases: nothing matches 'a"). A name or `_` over the whole value
  keeps the variable (`v => v` returns `'a`, and `n => n + 1` is
  refused): to compute with it, test it (`i64 as n => n + 1`). A
  variable with no bound is `Any` inside: its parts bind `Any`, and
  only a name or `_` covers it. Beside other members (`(bool, ['b,
  null])`, `'b: [i64, string]`) a name or `_` at its position covers
  its values, and so do tests that split its bound (`(bool, i64) as
  (false, n)`, `(bool, string) as (false, s)`). A slice over a
  constructor variable's application (`'c<'b>`, `'c: Collection`) gives
  its element (`[h, ..]` gives `h: 'b`) and needs a catch-all. A
  destructuring `let` reads its parts from the bound alike; a
  destructuring parameter is refused ("the parameter (a, b) binds
  inside a value of 'a"): annotate it with the type it binds, or bind
  it whole and destructure in the body.
- A written test cannot name a declared type variable: `'a as v`,
  `Array<'b> as a`, `(bool, 'b) as (false, v)` are refused ("a test
  cannot name a declared type variable ('b): write `_` there, and the
  name takes the value's own type"). `(_, i64) as (w, n)` over `('a,
  i64)` gives `w: 'a`; a variable only the test writes (`'q`) passes
  every value the same way. At the top a `_ as w` types `w` by the
  bound: write the bare name. A test of a tuple, struct, variant or
  array over a variable with no bound, or over a union that holds one
  (`['b, (i64, i64)]`), is refused ("can't tell (i64, i64) from 'b:
  both have the same runtime form"), since an instance may be any type:
  declare the bound (`'b: i64`). A scalar test there (`i64 as n`)
  checks.
- A type test of an abstract type compares its parameters: over
  `W<'b>`, `W<(i64, string)> as W((n, s))` takes a `W` built at that
  type alone and covers nothing of `W<'b>`, so a catch-all follows.
  Declare the bound, `'b: (i64, string)`, and a plain `W((n, s))` reads
  the parts from it. Over a union of instances a plain `W(n)` takes
  each, `n` the union of their payloads; test one first to take it
  alone (`W<i64> as W(n)`).
- A written test that holds part of a tuple, struct or variant covers
  that part beside other arms' bool literals and variant heads: `(true,
  _)`, `(bool, i64) as (false, n)` and `(bool, null) as (false, _)`
  cover `(bool, [i64, null])`. What the arms leave is named ("missing
  match cases: nothing matches `` `A(bool) ``").
- Over `Any` (a scrutinee, a tuple position, a field, a payload) a
  pattern tests the shape alone and every name it binds is `Any`: `(a,
  b) => a + b` is refused ("Number does not contain Any"), as are `[h,
  ..] => h + 1` and a test's own variable (`('q, i64) as (r, n) => r +
  n`). Write the test, `(i64, i64) as (a, b)`, `Array<i64> as [h, ..]`,
  or select on the name (`select a { i64 as n => .. }`). A test may not
  hold a function type ("can't match on a type holding a function"): it
  tells a function from a value, not one signature from another. A
  partial struct pattern takes the struct members that have its fields,
  and `Any` is none, so `{a, ..}` there is dead ("will never match"),
  under a tuple, variant or slice too: write the test, `(i64, {a: i64,
  b: i64}) as (_, {a, ..})`. Beside a member that has the struct the
  pattern is that member's: `(_, {a, ..})` over `` [(`K, {a: i64, b:
  i64}), (`J, Any)] `` gives `a: i64`.
- An abstract's constructor pattern tests the tag and the payload's
  shape, never the type of a part: over `W<Any>` or `Any`, each name
  `W((a, b))` takes out is `Any`, and `a + 1` is refused. Select on the
  name (`select a { i64 as i => .. }`). A whole capture over `Any` (`w@
  W(_)`) is `W<Any>` the same way; a parameter with a declared bound is
  its bound there. Where the representation holds a parameter under a
  `&mut`, a function's argument or another abstract type
  (`Abstract<&mut 'a>`), `W<Any>` does not hold a `W<i64>`: `W(r)` over
  `Any` still takes every instance, and `r` is what every instance's
  payload fits, a read-only `&Any` (a function there takes no argument,
  a part inside another abstract type is `Any`). To write or call, test
  the instances first, `W<i64> as W(r)`, with `W(r)` after them for the
  rest. A whole capture there (`w@ W(_)`) is refused: name the
  scrutinee.
- A partial struct pattern in a select takes every struct member that
  has its fields, whatever else each holds, and a name is its field's
  type in each: `{a, ..}` over `[{a: i64, b: string}, {a: string, c:
  f64}]` binds `a: [i64, string]`. Test for one member first to take it
  alone (`{a: i64, b: string} as {a, ..}`). A `let` or a parameter
  reads one layout: over members whose fields differ it is refused ("it
  has no one layout to read: bind the value whole and take it apart in
  a select"). Members with the same fields check, whatever their types.
- No arm may tell apart two types that share a runtime form: two
  reference types, two function types. `&i64 as x` over a `&i64` or a
  `&string` is refused ("can't tell &i64 from &string: both have the
  same runtime form; wrap them in distinct variants"). A name tests
  nothing, so an arm may bind such a part, and the bind holds every
  member the arm's own tests admit: `(r, n)` over two tuples that hold
  different function types gives `r` both, so a call of it is refused,
  and `` (`A, f) `` the `` `A `` member's alone. A test's own variable
  binds it the same way (`('q, i64) as (r, n)`), and a union the test
  writes binds every member it holds (`([&mut i64, &mut string], i64)
  as (r, n)`). A test over a tuple, struct or variant tells such a part
  by its other parts: over `[(&i64, i64), (&string, string)]`, `(&i64,
  i64) as (r, n)` checks, since no value of the second member passes
  the `i64`. An array or list part tells nothing: an empty one passes
  the test for any element type. Everything else is told apart:
  a string from a bare variant, an array from a tuple, struct, variant
  or list, `[]` from `[<>]`.

```graphix,cases
no "pattern `M will never match: nothing reaches it" let v = never(); let z = select v { `J => 1, `M => 2 }; v <- `J; z
no "pattern _ will never match" let v = never(); let z = select v { `J => 1, _ => 2 }; v <- `J; z
no "missing match cases: nothing matches `K" let v = never(); let z = select v { `J => 1, `M => 2 }; v <- `K; z
ok let v: [`J, `M] = never(); let z = select v { `J => 1, `M => 2 }; v <- `J; z
ok let b = true; let z = never(); let v = select b { true => {a: [1], c: 1}, false => z }; let r = select v { {a, ..} => "[a]" }; z <- {a: ["s"], c: "w"}; r
no "missing match cases: nothing matches i64" let b = true; let z = never(); let v = select b { true => {a: [1], c: 1}, false => z }; let r = select v { {a, ..} => "[a]" }; z <- 5; r
ok let f = |x| select (x, 0) { (`J, _) => 1, (`M, _) => 2 }; f(`M)
no "missing match cases: nothing matches (`K, i64)" let f = |x| select (x, 0) { (`J, _) => 1, (`M, _) => 2 }; f(`K)
no "missing match cases: nothing matches `K" let f = |x| select x { `J => 1, `M => 2 }; f(`K)
ok let f = |x| select x { `J => 1, _ => 3 }; (f(`J), f(4))
no "pattern `M will never match" |x| select x { `J => 1, _ => 2, `M => 3 }
ok let f = |x| select (x, 0) { ((a, b), _) => a + b }; f((1, 2))
no "missing match cases" let f = |x| select (x, 0) { ((a, b), _) => a + b }; f(1)
ok let f = |x| select x { {a, ..} => a }; (f({a: 1}), f({a: "s", b: 2}))
no "missing match cases: nothing matches i64" let f = |x| select x { {a, ..} => a }; f(1)
ok let f = |s| select s { ([], _) => 0, ([x, ..], b) => str::len(b) }; f(([1], "q"))
ok let f = |x| { let r = &mut (x, 0); select *r { (`J, _) => 1, (`M, _) => 2 } }; f(`J)
no "does not contain (`K, i64)" let f = |x| { let r = &mut (x, 0); select *r { (`J, _) => 1, (`M, _) => 2 } }; f(`K)
ok |x| { let r = &mut (x, 0); select *r { ((a, b), _) => a } }
ok type W<'a> = Abstract<'a>; let f = |x| select W((x, 0)).0 { (`J, _) => 1, (`M, _) => 2 }; f(`M)
no "does not contain (`K, i64)" type W<'a> = Abstract<'a>; let f = |x| select W((x, 0)).0 { (`J, _) => 1, (`M, _) => 2 }; f(`K)
ok type W<'a> = Abstract<'a>; |x| select W(x).0 { `J => 1, `M => 2 }
no "pattern [`J, _] will never match" |x| select [x, 0] { [`J, _] => 1, _ => 3 }
ok |x: [`J, i64]| select [x, 0] { [`J, _] => 1, _ => 3 }
ok let f = |x| select [x, 0] { [`J, _] => 1, _ => 3 }; (f(`J), f(1))
ok let f = |s| select s { `A(null) => 0, `A(a) => a + 1 }; (f(`A(null)), f(`A(3)))
ok let f = |s| select s { `Ok(null) => "none", `Ok(v) => v, `Err(e) => e }; f(`Ok("x"))
no "i64 does not contain null" let f = |s| select s { `A(1, null) => 0, `A(1, a) => a + 1, _ => 5 }; f(`A(1, null))
no "i64 does not contain string" let f = |s| select s { (1, a) if a > 0 => a + 1, (_, a) => 0 }; f((2, "s"))
no "string does not contain i64" let b = true; let z = never(); let t = select b { true => (`A, z), false => (`B, "s") }; let r = select t { (_, x) => str::len(x) }; z <- 5; r
ok let b = true; let z = never(); let t = select b { true => (`A, z), false => (`B, "s") }; let r = select t { (`A, x) => x + 1, (`B, s) => str::len(s) }; z <- 5; r
ok |s| select s { (1, a) => a + 1, (2, a) => str::len(a) }
no "missing match cases" let f = |s| select s { (1, a) => a + 1, (2, a) => str::len(a) }; f((1, 1))
no "i64 does not contain string" let f = |s| select s { `A(1, a) => a + 1, `A(2, a) => str::len(a), _ => 0 }; f(`A(1, "q"))
no "string does not contain i64" let f = |s| select s { `A(1, a) => a + 1, `A(2, a) => str::len(a), _ => 0 }; f(`A(1, 5))
ok let f = |s| select s { (1, _) => 0, (_, b) => b + 1 }; f((2, 5))
no "i64 does not contain string" let f = |s| select s { (1, _) => 0, (_, b) => b + 1 }; f((1, "a"))
no "i64 does not contain string" let f = |s| select s { (1, 1) => 0, (3, "x") => 2, (4, v) => v + 1, (_, _) => 9 }; f((3, "x"))
ok let f = |s| select s { `A(1, a) => a, `A(2, a) => a, _ => never() }; (f(`A(1, "q")), f(`A(2, 5)))
ok let f = |s: `A(i64, [i64, string])| select s { `A(_, a) => select a { i64 as n => n + 1, string as t => str::len(t) } }; (f(`A(1, 5)), f(`A(2, "q")))
ok let f = |s| select s { (1, 1) => 0, (3, "x") => 2, (4, v) => select v { i64 as n => n + 1, s => str::len(s) }, (_, _) => 9 }; f((4, "yy"))
no "i64 does not contain string" let v = never(); let z = select v { [`J(n), ..] => n + 1, _ => 3 }; v <- [`J("s"), `K]; z
ok let v = never(); let z = select v { [`J(n), ..] => n + 1, _ => 3 }; v <- [`J(1), `K]; z
ok let v: Array<[`J(i64), `J(string), `K]> = never(); let z = select v { [`J(n), ..] => select n { i64 as i => i + 1, string as s => str::len(s) }, _ => 3 }; v <- [`J("abc"), `K]; z
no "i64 does not contain string" let e1 = []; let e2 = []; let b = true; let t = select b { true => (e1, 1), false => (e2, "s") }; let z = select t { (a, n) => a[0]$ + 1 }; e1 <- [1]; e2 <- ["s"]; z
ok let e1 = []; let e2 = []; let b = true; let t = select b { true => (e1, 1), false => (e2, "s") }; let z = select t { (a, n) => a[0]$ + 1 }; e1 <- [1]; e2 <- [2]; z
no "annotate where its result goes" let u = str::parse("1")$; select u { i64 as n => n, _ => 0 }
ok let u: [i64, string] = str::parse("1")$; select u { i64 as n => n, _ => 0 }
ok 'a: [{a: i64, b: string}, i64] |x: 'a| -> i64 select x { {a, ..} => a, i64 as n => n }
no "pattern _ will never match" 'a: [{a: i64, b: string}, i64] |x: 'a| select x { {a, ..} => a, i64 as n => n, _ => 0 }
no "missing match cases: nothing matches 'a" 'a: [{a: i64, b: string}, i64] |x: 'a| select x { {a, ..} => a }
ok |x: ['a, null]| -> 'a select x { null => never(), v => v }
no "i64 does not contain 'a" 'a: [{a: i64, b: string}, i64] |x: 'a| select x { {a, ..} => a + 1, n => n + 1 }
ok 'a: [{a: i64, b: string}, i64] |x: 'a| select x { {a, ..} => a + 1, i64 as n => n + 1 }
no "Number does not contain Any" |x: 'a| select x { (a, b) => a + b, _ => 1 }
no "missing match cases: nothing matches 'a" |x: 'a| select x { (a, b) => 0 }
ok |x: 'a| select x { (a, b) => 0, i64 as n => n, _ => 1 }
ok 'b: [i64, string] |x: (bool, ['b, null])| select x { (true, _) => 0, (false, null) => 3, (false, v) => 1 }
ok 'b: [i64, string] |x: (bool, ['b, null])| select x { (true, _) => 0, (bool, i64) as (false, n) => 1, (bool, string) as (false, s) => 2, (false, null) => 3 }
no "missing match cases: nothing matches (bool, 'b)" 'b: [i64, string] |x: (bool, ['b, null])| select x { (true, _) => 0, (false, null) => 3 }
ok let g = 'c: Collection, 'b: (i64, i64) |x: 'c<'b>| -> [i64, null] select x { [h, ..] => h.0, _ => null }; g([(3, 4)])
ok 'c: Collection, 'b: (i64, i64) |x: 'c<'b>| -> i64 select x { [(a, b), ..] => a, _ => 0 }
ok 'c: Collection |x: 'c<'b>| -> ['b, null] select x { [h, ..] => h, _ => null }
no "missing match cases: nothing matches 'c<'b>" 'c: Collection |x: 'c<'b>| -> i64 select x { [h, ..] => 1, [] => 0 }
ok let g = 'b: (i64, i64) |x: {a: Array<'b>, n: i64}| -> i64 select x { {a: [h, ..], n} => h.0 + n, _ => 0 }; g({a: [(3, 4)], n: 1})
ok 'a: (i64, i64) |x: 'a| { let (a, b) = x; a + b }
ok 'a: (i64, i64) |x: 'a| { let r = &mut (x, 0); let ((a, b), c) = *r; a + b }
no "the parameter (a, b) binds inside a value of 'a" 'a: (i64, i64) |(a, b): 'a| a + b
no "a test cannot name a declared type variable ('a)" |x: ['a, null]| select x { 'a as v => 0, _ => 1 }
no "a test cannot name a declared type variable ('b)" |x: [Array<'a>, i64], y: 'b| select x { Array<'b> as a => 1, i64 as n => n }
no "write `_` there, and the name takes the value's own type" 'b: [i64, string] |x: (bool, ['b, null])| select x { (true, _) => 0, (bool, 'b) as (false, v) => 1, (false, null) => 3 }
no "a test cannot name a declared type variable ('t)" 't: Number + Singleton |x: Any| -> 't select x { ('t, 't) as (a, b) => a + b, _ => never() }
ok |x: ('a, i64)| -> 'a select x { (_, i64) as (w, n) => w }
ok |z: [(i64, 'a), (string, 'a)]| -> ['a, null] select z { (i64, _) as (n, w) => w, _ => null }
ok 'b: [`A(i64), `B] |x: 'b| select x { 'c as `A(z) => z + 1, _ => 2 }
ok let f = |v: [Array<'b>, i64]| -> 'b select v { Array<['c, null]> as [x, ..] => x, _ => never() }; f(["s"])
no "does not contain Any" |z: 'a| -> 'a select z { _ as w => w }
ok |z: 'a| -> 'a select z { w => w }
no "can't tell (i64, string) from 'a" |x: 'a| select x { (i64, string) as (n, s) => n, _ => 0 }
no "can't tell Array<i64> from 'a" |x: 'a| select x { Array<i64> as a => 0, _ => 0 }
ok 'a: (i64, string) |x: 'a| select x { (i64, string) as (n, s) => n }
no "can't tell (i64, i64) from 'b: both have the same runtime form" |x: ['b, (i64, i64)]| select x { (i64, i64) as (a, b) => a + b, _ => 0 }
ok 'b: i64 |x: ['b, (i64, i64)]| select x { (i64, i64) as (a, b) => a + b, v => v }
ok |x: ['b, i64]| select x { i64 as n => n + 1, _ => 0 }
ok type W<'a> = Abstract<'a>; |x: W<'b>| select x { W<(i64, string)> as W((n, s)) => n, _ => 0 }
no "missing match cases: nothing matches W<'b>" type W<'a> = Abstract<'a>; |x: W<'b>| select x { W<(i64, string)> as W((n, s)) => n }
ok type W<'a> = Abstract<'a>; 'b: (i64, string) |x: W<'b>| select x { W((n, s)) => n }
ok type W<'a> = Abstract<'a>; |x: [W<i64>, W<string>]| -> [i64, string] select x { W(n) => n }
ok type W<'a> = Abstract<'a>; |x: [W<i64>, W<string>]| select x { W<i64> as W(n) => n, W(s) => str::len(s) }
ok |x: (bool, [i64, null])| select x { (true, _) => 0, (bool, i64) as (false, n) => n, (bool, null) as (false, _) => 1 }
no "missing match cases" |x: (bool, [i64, string, null])| select x { (true, _) => 0, (bool, i64) as (false, n) => n, (false, null) => 3 }
ok |x: [`A([bool, Array<i64>])]| select x { `A(true) => 0, `A(false) => 1, `A(Array<i64>) as `A(a) => 2 }
no "missing match cases: nothing matches `A(bool)" |x: [`A([bool, Array<i64>])]| select x { `A(true) => 0, `A(Array<i64>) as `A(a) => 2 }
no "will never match" type E = [{a: i64, d: i64}, {c: i64}]; |x: (i64, E)| select x { (n, {a: _, d: _}) => 2, (n, {c: _}) => 1, _ => 3 }
no "Number does not contain Any" |x: Any| select x { (a, b) => a + b, _ => 0 }
ok |x: Any| select x { (i64, i64) as (a, b) => a + b, _ => 0 }
ok |x: Any| select x { (a, b) => select a { i64 as n => n + 1, _ => 0 }, _ => 0 }
ok |x: (Any, i64)| -> i64 select x { ((a, b), n) => n, _ => 0 }
no "Number does not contain Any" |x: (Any, i64)| -> i64 select x { ((a, b), n) => a + b + n, _ => 0 }
no "Number does not contain Any" |x: `A(Any)| select x { `A((p, q)) => p + q, _ => 0 }
no "Number does not contain Any" |x: {a: Any}| select x { {a: (p, q)} => p + q, _ => 0 }
no "i64 does not contain Any" |x: Any| select x { [h, ..] => h + 1, _ => 2 }
ok |x: Any| select x { Array<i64> as [h, ..] => h + 1, _ => 2 }
ok |x: Any| select x { Array<[(i64, i64), string]> as [(a, b), _] => a + 1, _ => 2 }
no "i64 does not contain Any" |x: Array<(Any, i64)>| select x { [((a, _), n), ..] => a + 1, _ => 0 }
ok |x: Array<(Any, i64)>| select x { [((a, _), n), ..] => n + 1, _ => 0 }
ok |x: Array<(Any, i64)>| select x { Array<((i64, i64), i64)> as [((a, _), n), ..] => a + 1, _ => 0 }
no "Number does not contain Any" |x: Any| select x { ('q, i64) as (r, n) => r + n, _ => 0 }
no "can't match on a type holding a function" |x: Any| select x { (fn(x: i64) -> i64, i64) as (g, b) => g(b), _ => 0 }
ok |x: Any| select x { (g, b) => 1, _ => 0 }
ok |x: (bool, Any)| select x { (_, `Z) => 3, (true, _) => 1, (false, _) => 2 }
no "pattern { a, .. } will never match" |x: Any| select x { {a, ..} => 1, _ => 0 }
no "pattern (_, { a, .. }) will never match" |x: (i64, Any)| select x { (_, {a, ..}) => 1, _ => 0 }
no "pattern `A({ a, .. }) will never match" |x: `A(Any)| select x { `A({a, ..}) => 1, _ => 0 }
no "pattern [{ a, .. }, ..] will never match" |x: Array<Any>| select x { [{a, ..}, ..] => 1, _ => 0 }
ok |x: (i64, Any)| select x { (i64, {a: i64, b: i64}) as (_, {a, ..}) => a, _ => 0 }
ok |x: [(`K, {a: i64, b: i64}), (`J, Any)]| select x { (_, {a, ..}) => a + 1, _ => 0 }
no "i64 does not contain Any" type W<'a> = Abstract<'a>; |x: W<Any>| select x { W((a, b)) => a + 1, _ => 0 }
no "i64 does not contain Any" type W<'a> = Abstract<'a>; |x: Any| select x { (W((a, b)), n) => a + 1, _ => 0 }
ok type W<'a> = Abstract<'a>; |x: W<Any>| select x { W((a, b)) => select a { i64 as i => i + 1, _ => 0 }, _ => 0 }
ok type W<'a> = Abstract<'a>; |x: W<(i64, i64)>| select x { W((a, b)) => a + 1 }
no "i64 does not contain Any" type W<'a> = Abstract<Map<string, 'a>>; |x: W<Any>| select x { W(m) => m{"k"}$ + 1 }
no "i64 does not contain Any" type W<'a> = Abstract<'a>; |x: Any| select x { w@ W(_) => w.0 + 1, _ => 0 }
ok type W<'a> = Abstract<'a>; |x: Any| select x { w@ W(_) => select w.0 { i64 as i => i + 1, _ => 0 }, _ => 0 }
ok type W<'a: Number> = Abstract<'a>; |x: Any| select x { W(p) => "[p]", _ => "o" }
no "can't write through a read-only reference (&Any)" type W<'a> = Abstract<&mut 'a>; |x: Any| select x { W(r) => *r <- "s", _ => never() }
ok type W<'a> = Abstract<&mut 'a>; |x: Any| select x { W<i64> as W(r) => { *r <- 5; "wrote" }, W(r) => "[*r]", _ => "o" }
no "this function's parameter is of no type" type W<'a> = Abstract<fn(x: 'a) -> 'a>; |x: Any| select x { W(g) => g(1), _ => 0 }
no "a name over the whole of a W needs its instance" type W<'a> = Abstract<&mut 'a>; |x: Any| select x { w@ W(_) => 1, _ => 0 }
ok type W<'a> = Abstract<&mut 'a>; |x: Any| select x { W(_) => 1, _ => 0 }
ok |x: [{a: i64, b: string}, {a: i64, c: f64}]| select x { {a, ..} => a }
no "i64 does not contain [i64, string]" |x: [{a: i64, b: string}, {a: string, c: f64}]| select x { {a, ..} => a + 1 }
ok |x: [{a: i64, b: string}, {a: string, c: f64}]| select x { {a: i64, b: string} as {a, ..} => a + 1, {a, ..} => str::len(a) }
no "it has no one layout to read: bind the value whole and take it apart in a select" let v: [{a: i64, b: string}, {a: i64, c: f64}] = {a: 1, b: "s"}; let {a, ..} = v; a
no "it has no one layout to read" |{a, ..}: [{a: i64, b: string}, {a: i64, c: f64}]| a
ok let v: [{a: i64, b: string}, {a: string, b: string}] = {a: 1, b: "s"}; let {a, ..} = v; a
ok let b = true; let x = never(); let y = never(); let v = select b { true => {a: [1], c: x}, false => {a: [2], c: y} }; let {a, ..} = v; x <- 1; y <- "w"; a
no "same runtime form" let a: [&i64, &string] = &1; select a { &i64 as x => 1, &string as y => 2 }
no "can't tell &i64 from &string: both have the same runtime form; wrap them in distinct variants" let a = 1; let b = "s"; let v = true; let r = select v { true => &a, false => &b }; select r { &i64 as x => 1, _ => 2 }
ok let b = true; let t = select b { true => (|x: i64| x, 1), false => (|y: string| y, "s") }; select t { (r, n) => 0 }
no "string does not contain i64" let b = true; let t = select b { true => (|x: i64| x, 1), false => (|y: string| y, 2) }; select t { (r, n) => r(n) }
ok let b = true; let t = select b { true => (`A, |x: i64| x), false => (`B, |y: string| y) }; select t { (`A, f) => "[f(1)]", (`B, g) => g("s") }
ok let b = true; let t = select b { true => (|x: i64| x, 1), false => (|y: string| y, 2) }; select t { ('q, i64) as (r, n) => n }
ok let a = 1; let b = "s"; let v = 0; let t = select v { 0 => (&mut a, 1), _ => (&mut b, 2) }; select t { ([&mut i64, &mut string], i64) as (r, n) => n }
ok let a = 1; let b = "s"; let v = true; let t = select v { true => (&a, 1), false => (&b, "s") }; select t { (&i64, i64) as (r, n) => *r + n, _ => 0 }
no "same runtime form" let a = 1; let b = "s"; let v = true; let t = select v { true => (&a, 1), false => (&b, 2) }; select t { (&i64, i64) as (r, n) => *r + n, _ => 0 }
no "same runtime form" let a = 1; let b = "s"; let v = true; let e: Array<string> = []; let t = select v { true => ([1], &a), false => (e, &b) }; select t { (Array<i64>, &i64) as (x, r) => *r + 1, _ => 0 }
no "same runtime form" let a = 1; let b = "s"; let v = true; let e: List<string> = [<>]; let t = select v { true => ([<1>], &a), false => (e, &b) }; select t { (List<i64>, &i64) as (x, r) => *r + 1, _ => 0 }
no "can't match on Fn type" let v = true; let g = select v { true => |x: i64| x, false => |y: string| y }; select g { fn(x: i64) -> i64 as h => 1, _ => 2 }
ok let v = true; let s = select v { true => {f: |x: i64| x, n: 1}, false => {f: |y: string| y, n: "s"} }; select s { {n: 1, ..} => 1, _ => 0 }
ok |x: [string, `A]| select x { `A => 1, s => 2 }
ok |x: [Array<i64>, (i64, i64)]| select x { (a, b) => 1, [_, ..] => 2, [] => 3 }
ok |x: [Array<i64>, List<i64>]| select x { [<_, ..>] => 0, [<>] => 1, [_, ..] => 2, [] => 3 }
```
