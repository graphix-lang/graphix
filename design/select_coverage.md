# Select: routing, coverage and patterns

Status: as built, by the check by bounds.
Code: `graphix-types/src/typ/bounds/select.rs` (the routing), `graphix-compiler/src/node/select.rs`
(`check_by_bounds`), `node/pattern.rs` (a pattern as the solver's `Pat`).
Pins: `stdlib/graphix-tests/src/lang/select.rs`, `select_undecided.rs`.
The model in one paragraph, and the rules beside the other typing rules:
`design/tvar_constraints.md`. The author's view: `ide/skills/graphix-lang/select.md`.

A ```` ```graphix,cases ```` fence holds one program a line as `--check` judges it: `ok`,
or `no "text"`, refused with that text in its message.

## Routing

A select registers its arms on its scrutinee (`Solving::select`, `Select`). Nothing
walks "the scrutinee's type" once: **each type that reaches the scrutinee is routed down
the arms in order** (`select_route`, `route`).

- An arm takes the part of the value its pattern admits and passes on the rest
  (`Routed { part, rest, binds }`). A guarded arm passes everything on.
- A name is given the union of the parts routed to it, over every value that reaches
  the select, as one type.
- What no arm takes is a missing case. With no arm that admits everything, the arms'
  tests together bound a scrutinee nothing has decided (`check_by_bounds`), so a value
  that arrives later and fits no arm is refused where it arrives.
- A select in a generic definition is a template: each instance routes its own values
  down a copy (`Select::instance`), so `f(1)` and `f("s")` are judged apart.
- What an arm leaves of a value that is no plain type is kept as a marker around the
  type it is part of (`marked`): the arrays of the lengths not yet taken (`#elems:`), what
  is left of an abstract type's payload, the values of a declared variable earlier arms
  left (`#within`).
- A pair of typedef names met again inside its own walk shares no value until the walk
  shows one, and is walked again if that was wrong (`Walks`): values are finite, so the
  answer is the least one.

```graphix,cases
ok let f = |x| select (x, 0) { (`J, _) => 1, (`M, _) => 2 }; f(`M)
no "missing match cases: nothing matches (`K, i64)" let f = |x| select (x, 0) { (`J, _) => 1, (`M, _) => 2 }; f(`K)
no "missing match cases: nothing matches string" let v: [i64, string] = 1; select v { i64 as n => n }
ok let v: [i64, string] = 1; select v { i64 as n => n, s => str::len(s) }
ok let v: [(i64, i64), (string, i64)] = (1, 2); select v { (a, n) => n }
ok let f = |x| select x { i64 as n => n, string as _ => 0 }; (f(1), f("s"))
no "missing match cases" let f = |x| select x { i64 as n => n, string as _ => 0 }; f(true)
```

## A value not decided yet

A value may reach a select while part of it is still an open cell.

- A name or `_` takes whatever is there, decided or not.
- A pattern that looks into the cell (a literal, a tag, a test, a structure over it)
  cannot be routed until the cell is decided: the value waits (`Select::waiting`,
  `looked`) and is routed when the cell is, or at the input's end, where what nothing
  decided is fixed (`select_waiting`).
- A pattern looks into a member only where its tags could match (`of_another_tag`): the
  literal of a `` `L `` arm does not wait on what a `` `K `` member holds.

```graphix,cases
no "will never match" let x = never(); let y = select x { null as _ => 0, i64 as i => i }; x <- 1; y
ok |x, b: bool| select (select b { true => (`K, x), false => (`L, 0) }) { (`K, _) => 1, (`L, 0) => 2, _ => 3 }
```

## Dead arms

Dead arms are judged once, at the end, over everything that was routed (`dead_arms`).

- An arm nothing reached is refused, naming the pattern and what the select was given.
- An arm an earlier unguarded arm takes whole by its structure (`Pat::covers`) is dead
  whatever the scrutinee turns out to be.
- In a definition's own check an arm that looks into a part nothing has decided may be
  reached by an instance, and an instance's arm that nothing reaches is no dead arm:
  another instance may reach it.

```graphix,cases
no "pattern 5 will never match" let f = |x| select x { _ => 3, 5 => 1 }; f(5)
no "pattern _ will never match" |x: i64| select x { _ => 0, _ => 1 }
no "pattern (`M, _) will never match" |x, b: bool| select (select b { true => (`K, x), false => (`L, 0) }) { (`K, _) => 1, (`L, 0) => 2, (`M, _) => 4, _ => 3 }
```

## Written tests and runtime forms

A written test (`T as p`) is a run-time test, and a run-time test takes every value of
its runtime form (`both`, `less` with `test`): any reference passes a reference test,
any function a function test, a Rust-backed abstract type any of its instances.

- No arm may tell apart two types of one runtime form (`Type::rep_collision`). Where a
  test takes a member it does not hold, the refusal names the two types, wherever it is
  found: at the scrutinee's bound when no arm admits everything (`check_by_bounds`,
  `by_form`, over `bounds::standing_whole`), at the bind the member cannot fit
  (`select_route_at`), or as the dead arm below it (`dead_arms`).
- A written test cannot name a declared type variable: write `_` there, and the name
  takes the value's own type.
- A test of a reference over a scrutinee that may hold `Any` is refused: every reference
  has one runtime form.
- A pattern that takes an `Any` part apart binds `Any`, which its body cannot compute
  with: write the test.

```graphix,cases
no "can't tell &i64 from &string" let b = "s"; let t = &b; select t { &i64 as r => 0 }
no "the test of an arm above can't tell &i64 from &string" let a = 1; let b = "s"; let v = 0; let t = select v { 0 => (&a, 1), _ => (&b, 2) }; select t { (&i64, i64) as (r, n) => n, _ => 0 }
no "a test cannot name a declared type variable ('b)" let f = |x: [`A(('a, i64)), `B], y: 'b| select x { `A(('b, Any)) as `A((p, _)) => 1, `B => 100, _ => 7 }; 0
no "Number does not contain Any" |x: Any| select x { (a, b) => a + b + 1, _ => 0 }
```

## Patterns

- **A declared variable.** A pattern over a value of a declared variable routes it as
  any other part: a name keeps the variable, and a constructor around it takes its
  member.
- **Or-patterns** (`Pat::Or`, `design/or_patterns.md`): each alternative takes its part,
  and a name the alternatives share is the union of them.
- **`name@ pattern`**: the name takes the whole part its pattern admits, as one type.
- **Partial struct patterns** take every member that has the fields they name; the
  members one arm reads must lay those fields out alike (`one_layout`).
- **Slices**: arms of different lengths cover an array between them, the lengths left
  kept as a marker.
- **Abstract types** (`Pat::Abs`): a constructor pattern takes the payload apart where
  the definition is visible. Where the value's instance is not known the payload is read
  as every instance's fits it (`AbstractRep::at_any_instance`), and a name over the whole
  of a type no one instance holds needs the instance written, `W<T> as ..` (`blind`).

```graphix,cases
ok let f = 'a: Any |x: [`A('a), `B]| select x { `A(v) => 1, `B => 0 }; f(`A("s"))
ok let v: [`A(i64), `B(i64), `C] = `A(1); select v { `A(n) | `B(n) => n, `C => 0 }
ok let v = (1, "s"); select v { a@ (n, _) => a.0 + n }
ok let a: [{a: i64, x: i64}, {x: i64, z: i64}] = {a: 1, x: 2}; select a { {x, ..} => x + 1 }
ok let a = [1, 2]; select a { [] => 0, [h, ..] => h }
no "missing match cases" let a = [1, 2]; select a { [h, ..] => h }
ok type W = Abstract<(i64, string)>; let w = W((1, "s")); select w { W((n, _)) => n }
```

## Where a refusal is

A refusal made while the select's own expression is checked is placed at the arm
(`Select::at_arm`). One found later, routing a value that arrived after the select was
checked or judging an instance's select, is placed at the select (`Select::placed`).
