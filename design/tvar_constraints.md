<!-- CR claude for eric: [readability/medium] This file is the type checker's rule book
and the document changed most since the last review (391 commits). It is 37,450 words in
113 paragraphs: the longest paragraph is 3,445 words, the median sentence 42, and 195
sentences run past 60 words, one to 833. A rule cannot be found in it and no sentence is
tied to the pin that fails when it stops being true; `architecture.md` and `builtins.md`
have the same shape. The skill already has the remedy: a `graphix,cases` fence, one
program a line, which `examples_compile.rs` checks. Give each section its model in a
page, then its rules as a table of rule, enforcing function and a cases line, and let
the gate read design/*.md as it reads the skill. probe:
design/review-2026-11-16/repro/a-compiler-doc-stats.py (a-compiler-09) -->
<!-- 2026-11-23 claude: this file is rewritten for the check by bounds in the shape
proposed here: the model in a page, then the rules as tables of rule, enforcing function
and a case line (`python3 tools/mdcases.py`-style: every line was run through `graphix
--check` on the day). What remains of the CR: `architecture.md` and `builtins.md` keep
their shape, and the gate does not read design/*.md yet. -->
# The type check: bounds

The checker solves by bounds. This is the model and the rules as built; the skill
(`ide/skills/graphix-lang/`) is the author's view of the same rules, and its case lines
are checked by the gate. Rulings behind the rules: `~/ptmp/cr-discuss.md`, D47 to D53.

Pins: `stdlib/graphix-tests/src/lang/{types,select,select_undecided,quantified,traits,repl_inputs,interfaces}.rs`,
`graphix-shell/tests/lsp/`, `graphix-shell/tests/deep_nesting.rs` (cost),
`graphix-fuzz/src/mustreject.rs` (`design/must_reject.md`).

## The model

**Cells.** A type the program does not write is a cell (`TVar`). A cell holds lower
bounds (what flows into it) and upper bounds (what its uses demand), never a guess. A
relation `a <= b` ("a value of `a` may stand where a `b` is wanted") between a type and a
cell records a bound and passes it on: a new lower bound meets every upper bound, a new
upper bound every lower one (`Solving::relate_in`, `cells`, `arrive`, `record`). Nothing
is decided when a bound is recorded, so no verdict depends on the order the program is
read in.

**Fixing.** At the end of a check every cell is fixed, once: the union of its lower
bounds, or with none the type its uses demand, or with neither bottom (`Solving::finish`,
`fix_round`, `fix`, `fixed`). What waits on a cell's type (a `?` or `$`, a field read, a
call through a cell, a select's arms, a union's member choice) is a use registered on
the cell, run when a value reaches it or when it is fixed. A predicate (`Number`,
`Singleton`, `Ordered`, `Discernible`, `NoError`, `Function`, a trait) is judged when its
cell is fixed (`judge_cell`).

**Levels, schemes, instances.** A definition's check runs one level deeper than its
surroundings. Closing it (`Solving::generalize`) makes the cells its type reaches the
scheme's variables and fixes the rest. A read of a generic binding copies the scheme
(`instantiate_into`, `copy_in`): each variable's copy takes copies of its bounds and of
what the definition left to each instance (`Left`: a comparison, a `Singleton`, a select
still waiting, a held choice). A bound is kept by the deeper of its two sides; a type
too deep for a cell is copied out to the cell's level first (`extrude`).

**A declared variable.** A variable a definition writes, in its header (`'a: B |..|`)
or bare in a position (`|x: 'a|`), is one type each call picks, and its bound is exactly
what is written for it, `Any` where nothing is. It is rigid while the body is checked:
the body may relate it only to itself, to its bound and to what holds it, and asks no
predicate of it the bound does not imply, `Singleton` included (`Solving::relate_in`,
`TVar::add_body_bound`). A refusal names what to write: "the body needs 'a within
Number + Singleton, which the signature does not declare: write 'a: Number +
Singleton" (`TVar::undeclared_bound`, `undeclared_needs`). Where one type is asked, a
declared variable beside another type is refused in the definition, since a call picks
the variable (`Solving::one_at_every_call`). A predicate over data that holds declared
variables is judged by their bounds (`Solving::hold_declared`), and a cell demanded as
one declared variable hands it every predicate asked of the cell (`Solving::fixed`). A
typedef the signature applies writes its parameter's bound for the variable it is
applied to, alone or as a member of a union (`bounds::bounded_by_names`, before the
body is checked and in `bounds::fits`). Inference is for what is left unwritten: an
untyped parameter's bound is what its body needs.

**Unions.** A value two members of a union could take is held until something decides
(`below_union`, `held`); a member that can never take it is out at once. At the check's
end a held value goes to the decided member that takes it, and one nothing decides is
refused.

**Selects.** A select registers its arms on its scrutinee (`bounds::select`). Each value
that reaches the scrutinee is routed down the arms (`route`): an arm takes the part its
pattern admits and passes on the rest, a name is bound to the union of the parts that
reach it, and what no arm takes is a missing case. With no arm that takes everything
the arms bound the scrutinee. Dead arms are judged at the end, over everything that was
routed (`dead_arms`). A pair of typedef names met again inside its own walk shares no
value until the walk shows one (`Walks`): values are finite.

**Inputs.** A REPL input, a script and a dynamic module's load are each checked alone,
and every cell an input makes is fixed at its end (`CellFact::Settled`). A later input
relates to what an earlier one left as to any decided type.

**Where a refusal is.** `Node::typecheck0` marks the expression being checked
(`bounds::Site`). A refusal found later (fixing a cell, routing a value that arrived
after its select was checked, judging an instance's select) is placed where the cell
was first bounded, at the select, or at the use that made the instance.

**Interfaces and traits.** An implementation is held to a declared signature by one
instance of it fitting the declaration with the declaration's variables as constants
(`bounds::fits`, `written_fits`): it may take more and give less.

## Rules

Each line is `ok PROGRAM` or `no "text" PROGRAM`, as `--check` judges it.

### Bindings

| rule | where |
|---|---|
| an unannotated binding is the union of what is written to it | `Solving::fixed` |
| an annotation is the binding's type, and a wider write names the union to declare | `node/bind.rs`, `Connect` |
| a use decides nothing | `Solving::arrive` |
| a value a type-directed operation makes takes its type from what is asked of it | `Solving::fixed` (`made_by_type`) |

```graphix,cases
ok let x = never(); x <- 1; x <- "s"; x
no "i64 does not contain string" let x = never(); x <- 1; x <- "s"; let y: i64 = x; y
no "declare x: [i64, string]" let x: i64 = 0; x <- "s"; x
ok let v: i64 = str::parse("1")$; v
no "must be known here" let f = |x: 'a, y: 'a| -> 'a y; f(str::parse("1"), 1)
```

### Functions

| rule | where |
|---|---|
| a function is generic where a binding names it; each read is an instance | `node/bind.rs` (`Held`), `Solving::generalize` |
| anything else is one instance, whose variables all its uses decide between them | `Solving::fixed` |
| a call's arguments of one variable widen it; a bound refuses | `Solving::call_at` |
| arithmetic takes one numeric type, judged at each instance | `node/op.rs` (`arith_rule`), `Left::Single` |
| a comparison takes operands where one's type holds the other's; any two numbers order | `node/op.rs`, `Left::Compared` |
| a variable a definition writes is rigid in its body, and its bound is exactly what is written (`Any` if nothing) | `node/lambda.rs` (`DefGate`), `Solving::relate_in` |
| one type is asked of a declared variable only where it declares `Singleton`, and never beside another type | `Solving::one_at_every_call` |
| an instance's variable that nothing meets is bottom | `Known::copied` |
| `Function` is judged by shape where the value arrives | `Solving::relate_in` |
| a let a block writes is one value, which every instance of what the block defines shares | `node/bind.rs` (`binds_definition`, `outside_parts`) |
| a function held as a function type is called as that type lists its arguments; a variadic one is called as one wherever its own type reaches the call | `Solving::fn_below`, `Use::Call` |
| `?` and `$` take the errors off their operand, or with none the null, and it holds one of the two | `bounds::strip` |
| what a function reads out of a binding outside it (a field, an element, a call's result, a pattern's name, what a held `?` or `$` leaves) is that binding's at every instance, and takes what is written to the binding after an instance is made | `Solving::quantify` (`produced_from_outside`, `given_late`), `copy_in` |
| an order asked of a function is refused where the definition asking it closes, called or not | `Solving::generalize` (`hold_shape`) |

```graphix,cases
ok let f = |x| x; (f(1), f("s"))
ok let mk = |z| |x| x; let h = mk(0); (h(1), h("s"))
no "cannot compute with [i64, f64]" let mk = |z| |x| x + x; let h = mk(0); (h(1), h(1.5))
ok let f = |x: 'a, y: 'a| -> 'a x; f(1, "s")
no "Number does not contain string" let f = 'a: Number |x: 'a| -> 'a x; f("s")
no "cannot compute with [i64, f64]" let f = |x, y| x + y; f(1, 2.5)
no "write 'a: Number + Singleton" let f = |x: 'a, y: 'a| x + y; f(1, 2)
no "write 'a: Number + Singleton" let f = 'a: Number |x: 'a, y: 'a| x + y; f(1, 2)
ok let f = 'a: Number + Singleton |x: 'a, y: 'a| x + y; f(1, 2)
no "write 'c: i64" let g = |x: i64| x; let f = |y: 'c| g(y); f(1)
no "must be one type, but the declared variable 'a may differ" let f = 'a: Number + Singleton |x: 'a| x + 1; f(1)
no "add Discernible to the bound of 'a" let f = 'a: Any |x: 'a, y: 'a| (x, 1) == (y, 1); f(1, 2)
ok let x: [i64, null] = 3; x == 3
no "cannot compare i64 with string" 1 == "b"
ok 1 < 2.5
no "add Discernible to the bound of 'a" let f = 'a: Any |x: 'a, y: 'a| x == y; f(1, 2)
ok let f = |g: fn(x: 'a) -> i64, h: fn(x: 'a) -> i64| 0; f(|x: i64| 0, |x: null| 0)
no "it has a defaulted label" queuefn(#trigger: never(), |#scale: i64 = 10, x: i64| -> i64 x * scale)(5)
no "u64 does not contain string" let f = { let tm = |x| x; let r = &mut tm; *r <- |x: u64| x + u64:1; tm }; f("s")
ok let f = { let acc = []; |x| { acc <- array::push(acc, x); acc } }; (f(1), f("s"))
no "a variadic function where one of 1 arguments is wanted" let f: fn(x: i64) -> i64 = all; f(1)
ok let s = {f: all}; s.f(2, 2)
no "it has no error and no null" let x = 1; x$
ok let x: Any = 1; x$
no "i64 does not contain string" let v = {a: 0}; let f = |x: i64| v.a; let t: i64 = f(1); v <- {a: "s"}; t
no "null does not contain i64" let v = 0; let f = |x: i64| (v +? x)$; let t: null = f(1); t
ok let v = {a: 0}; let f = |x| (x, v.a); (f(1), f("s"))
no "functions have no order" let ge = |p| p >= |n| n; 1
```

### Select

| rule | where |
|---|---|
| a select over a value not decided where it stands is judged against what the value turns out to be | `Solving::select_waiting` |
| over an unannotated parameter the arms say what a call may pass, refused at the call | `node/select.rs` (`check_by_bounds`), `select_route` |
| a name is the union of the parts that reach its arm | `select::route` |
| a partial struct pattern takes every member that has its fields | `select::route` |
| an arm nothing reaches is refused, over an undecided parameter too | `Solving::dead_arms` |
| a written test cannot name a declared type variable | `bounds/select.rs` |
| no arm tells apart two types of one runtime form | `Type::rep_collision` |

```graphix,cases
ok let f = |x| select (x, 0) { (`J, _) => 1, (`M, _) => 2 }; f(`M)
no "missing match cases: nothing matches (`K, i64)" let f = |x| select (x, 0) { (`J, _) => 1, (`M, _) => 2 }; f(`K)
ok let a: [{a: i64, x: i64}, {x: i64, z: i64}] = {a: 1, x: 2}; select a { {x, ..} => x + 1 }
no "pattern 5 will never match" let f = |x| select x { _ => 3, 5 => 1 }; f(5)
no "will never match" let x = never(); let y = select x { null as _ => 0, i64 as i => i }; x <- 1; y
no "Number does not contain Any" |x: Any| select x { (a, b) => a + b + 1, _ => 0 }
```

### Unions, traits and interfaces

| rule | where |
|---|---|
| a value two undecided members could take is held; one nothing decides is refused | `Solving::below_union`, `fix_round` |
| a trait dispatch over a union tests each member; a folded member no impl holds whole is taken apart | `Type::dispatch_members_of` |
| a method or a val is held to its declaration by one instance fitting it, the declaration's variables constants | `bounds::fits`, `node/traits.rs`, `node/module.rs` |

```graphix,cases
no "the implementation requires ['a, `X] to be NoError" trait T { val m: fn(self, x: ['a, `X]) -> i64 }; impl T for i64 { let m = 'q: NoError |s, x: 'q| -> i64 0 }; T::m(1, error(`E))
ok trait T { val m: fn(self, x: 'a) -> 'a }; impl T for i64 { let rec m = |s, x| select s { 0 => x, n => m(n - 1, x) } }; (T::m(2, 3), T::m(1, "a"))
```

## Cost

The check must stay instant. A walk over a type meets each shared part once (a type is
a graph: `map_cells_in`, `map_parts`, `negative_cells` keep what they have met), and a
part that comes out of a walk as it went in is the part itself, so sharing survives.
Budgets: `graphix-shell/tests/deep_nesting.rs`, `lang::types::holder_chain_*`.

## Debugging

`GRAPHIX_BOUNDS_DEBUG=1` prints each relation and each fixed cell (same-named copies
print alike). `GRAPHIX_ELAB_AUDIT=1` reports a build refusing what the check accepted,
which is a checker bug.
