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
| a comparison no instance can satisfy is refused where its definition closes, called or not: operands made of declared variables alone are what the signature says of them, and two constructors of different kinds (a reference and a function, a number and an abstract type) hold one another whatever is under them | `Solving::judge_declared`, `bounds::kinds_apart` |
| a comparison is each instance's to judge where an operand holds a cell an instance copies, or is a cell of the definition that waits on a variable of its scheme (an operand bound by a `let` first): the verdict is the same written in place or bound | `Solving::generalize` |
| a variable a definition writes is rigid in its body, and its bound is exactly what is written (`Any` if nothing) | `node/lambda.rs` (`DefGate`), `Solving::relate_in` |
| every variable a definition writes as its own, in its header or bare in its signature, is its scheme's once its check closes, so a call of a lambda where it is written copies it as a call by name does; one of a definition around it stays that definition's | `node/lambda.rs` (`DefGate::open`, `DefGate::close`), `Solving::own` |
| one type is asked of a declared variable only where it declares `Singleton`, and never beside another type | `Solving::one_at_every_call` |
| an instance's variable that nothing meets is bottom | `Known::copied` |
| `Function` is judged by shape where the value arrives | `Solving::relate_in` |
| a let a block writes is one value, which every instance of what the block defines shares | `node/bind.rs` (`binds_definition`, `outside_parts`) |
| a part of a definition's value that is computed once (a call among data that holds a function, a statement of a function-giving block) is compiled and checked at the level around the outermost definition it is a part of: its cells are no scheme's, so a read nothing asks a type of is refused there as anywhere | `node/bind.rs` (`Defining::around`, `Bind::outside`), `Node::typecheck0` |
| an open cell that flows into a cell fixed before it holds that cell, not its type: a type is asked of it only where one is asked of the cell it flows into, never by what else flowed there | `Solving::fix_cell`, `Solving::asked_of_above` |
| an argument that is no value says nothing of its parameter: the instance keeps the definition's own type for it (a callback's, which a collection is built from) | `CallSite::instance_signature` |
| a function no binding names is checked at the level it was compiled at: one written in a statement that a function-giving block runs once belongs to the level around the block | `node/lambda.rs` (`Lambda::around`, `DefGate::open`) |
| a function held as a function type is called as that type lists its arguments; a variadic one is called as one wherever its own type reaches the call | `Solving::fn_below`, `Use::Call` |
| `?` and `$` take the errors off their operand, or with none the null, and it holds one of the two; over an operand no value reaches there is nothing to take, and a `?` raises nothing | `bounds::strip` |
| a handler nothing raises to takes its type from what it is used as, which need hold no error; a seq's own handler in a function called under one has nothing to rethrow, and its instance judges no `?` of it | `Qop::typecheck0_instance` |
| a constructor trait's receiver that is a union of one constructor's applications is that constructor applied to what its members hold, at the check and at the build alike | `Solving::relate_in`, `node/traits.rs` (`receiver_constructor`) |
| an impl in a labeled argument's default is refused where the function is defined: the default compiles at every call that leaves the label out | `node/traits.rs` (`impl_outside_defaults`) |
| what a `?` or a `$` leaves a variable of a closing scheme, and what a `?` raises to one, is the scheme's too: a function returning a typed read through one is as generic in it as one returning the read's result whole, and each call decides it. What it leaves any other cell is the definition's own, whatever that cell reaches | `Solving::stripped` |
| a let of a block that ends in a generic name, or of a sample of one, names that name's instance, closed as the let's own scheme: the cell the read was compiled as is the instance | `Solving::name_as` |
| a variable's twin at a shallower level asks nothing of it: a `Concrete` variable is "asked" only by a type, or by a cell that is not its own twin | `Solving::type_asked` |
| what a function reads out of a binding outside it (a field, an element, a call's result, a pattern's name, what a held `?` or `$` leaves) is that binding's at every instance, and takes what is written to the binding after an instance is made | `Solving::quantify` (`produced_from_outside`, `given_late`), `copy_in` |
| what a held `?` or `$` gives late spreads: a cell it reaches through a type it is held under (a dereference's referent), through a use of it (a field, an element, a call's argument and result) or through a cell the scheme does not keep is given as late and is the outer binding's too, since no copy of the scheme replays a bound a cell outside it keeps. A variable of the scheme it flows into whole stays the scheme's: each copy keeps that edge | `Solving::given_late` |
| an order asked of a function is refused where the definition asking it closes, called or not | `Solving::generalize` (`hold_shape`) |
| a cell of a definition that a variable of its scheme flows into (a call's cell in a function inside it) is the scheme's too, so each instance has its own | `Solving::quantify_fed` |
| a cell a definition leaves undecided that must be one type is left to its instances only where it reaches a variable of its scheme; one that waits on a cell of the code around (arithmetic over an outer function's parameter) is left by the definition around whose variable it reaches | `Solving::generalize` |
| a cell nothing flows into takes the type of the cells it flows into and of what is written above it; where those cells, fixed apart, agree on no type it is no type, and only two types written for it are refused (a bound that is no cell, and what a function's instance requires of its argument) | `Solving::fixed`, `Solving::meets_demands` |
| a variable that no value reaches, a use of which read nothing (an element read, a select's arms), is bottom, not what the use demands: an instance's, and any cell no operation makes values at | `Solving::fixed` |
| a cell nothing flows into is no type where the type demanded of it holds a cell it flows into: fixed at that type it would hold itself | `Solving::fixed`, `bounds::takes` |
| the types demanded of a cell nothing flows into are met through their names: a named union and a set of primitives share what the union's members and the set share | `Solving::meet` |
| a type no value has (a tuple, struct or variant with a part nothing reaches) fits anywhere, as bottom does: a select over one checks whether it is written in place or bound first | `Solving::relate`, `Solving::empty` |
| a cell fixed by what is asked of it holds that type for the cells fixed after it, and a cell nothing flowed into and nothing is asked of holds what the cells below it became: a typed read asked its type through a block is that type in a call that only holds it | `Solving::settled`, `Solving::held_below` |
| the last fixing round is repeated while it fixes something: a cell it fixes may give a waiting select its value, and the select a cell its first bound | `Solving::fix_scope` |
| at an input's end every scheme has closed: a cell that reaches a scheme's variable (a value a named function reads) waits for nothing, and is fixed in order with the rest, so a use waiting on it is paid | `bounds::later`, `Solving::fixable` |
| a waiting use (a held choice, a select, a `?` or `$`, a chain) is paid after any other whose result flows into what it reads; at the input's end, uses that reach each other by some path are paid in that order alone, and only uses that feed each other in whatever order is left | `Solving::fix_round`, `fed_by` |
| a labeled default is compiled under its definition's own share, at every call that omits it: what the compile rewrites in it (a seq, a trait method named as a value) is the rewrite the check typed | `LambdaDef::in_signature`, `InstanceTypes::of_default` |

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
ok let h = { array::map([0], |x| |y| true); |n| n }; (h(1), h("s"))
ok let t = |x| [queuefn, x]; array::flat_map([1], t)
ok let f = |x| { let z = [x]; let g = |y: i64| array::len(z); g(0) }; (f(3), f("s"))
ok let second = |xs| xs[1]$; let a: Array<string> = array::map(never(), second); a
ok let f = |#k: i64 = seq 0 { 5 }, y| (k, y); f("s")
ok let v = {a: 1 + 1, b: 2}; let r = select v { {b: q, ..} => q }; let f = |x: i64| v.a; (r, f(0))
ok let g = { let f = (array::map(["a", "bb"], |s| s), |z| z); f }; (g.0, g.1(1), g.1("s"))
no "must be known here" let a = [|x| x, json::read("1")]; [a, -1, str::len]
no "must be known here" let x = json::read("true")$; let f = |q| { let arr = [false, x]; 0 }; f(0)
ok let x = json::read("true")$; let f = |q| { let arr: Array<bool> = [false, x]; arr[1]$ }; f(0)
no "neither type holds the other" let pick = |a| &a == |x: i64| -> i64 1; 0
no "cannot compare Option<'a> with i64" let f = 'a: Discernible |o: Option<'a>| o == 0; 0
ok let f = 'a: Discernible |o: ['a, null], v: 'a| select o { null => false, x => x == v }; f(1, 1)
ok let c = ["a", "b"]; let r = Collection::fold(c, 0, never()); 0
ok type T0 = Any; let n: T0 = 1; let m = {n => 1, |k| k * 2 => 2}; map::len(m)
ok let x = never(); let a = ([x, [x, 1]], 2 - { let t = x; t }); 0
no "used as i64 and as string, and no value is both" let x = never(); let y: i64 = x; let z: string = x; 0
no "used as string and as Number, and no value is both" let f = 'b: Number, 'c: string |x0: 'c, x1: 'b| 0; let v = never(); f(v, v)
ok (|v| -> [i64, Error<`Oops>] select v { 1 => error(`Oops), _ => v }); 0
ok let a = []; let b: Array<i64> = a; array::len(array::push(a, "s"))
no "used as i64 and as string, and no value is both" let a = []; let b: Array<i64> = a; let c: Array<string> = a; 0
ok let x = never(); let f = |i| array::map([0, x], |j| x); let r = array::map([2], f); 1
ok let s = json::read("1")$; let u = { let t = s; t } + 1; (u, once(s))
ok let k = |p: {f: fn(x: i64) -> i64}| 0; let s = {f: |x| x}; let t = {a: s, b: seqq 1 { k(s) }}; t.a.f("x")
ok let t = |k: i64| str::parse("[k]")$; let a: i64 = t(1); let b: f64 = t(2); (a, b)
no "must be known here" let t = |k: i64| str::parse("[k]")$; t(0)
ok let f = |x| x; let g = { 0; f }; let a = g(1); let b = g("s"); a + 1
ok let f = |x| x; let c = 1; let g = c ~ f; let a = g(1); let b = g("s"); a + 1
no "must be known here" let r = { let f = (1, |k: i64| str::parse("[k]")); 0; f }; r
no "must be known here" let r = { 0; (1, |k: i64| str::parse("[k]")) }; r
ok let f = 'a: Concrete |g: fn(v: 'a) -> i64| 0; let h = [list::nil, &[f]]; 0
no "must be known here" let f = 'a: Concrete |g: fn(v: 'a) -> i64| 0; let h = (list::nil, &[f]); (*(h.1))[0]$(|v| 0)
ok let f = |v| { let r = cast<{ x: i64 }>({ x: 1 }); r$.x }; f(0)
ok let got = never(); catch(e) got <- select e.0.error { `E(n) => n }; (|x| error(`E(x))?)(7); got
no "i64 does not contain f64" let f = |a| { let g = |x| { let rec s = |n| select n { 0 => 0, _ => s(n - a) }; s(3) }; g(0) }; f(1.0)
ok let f = |a| { let g = |x| { let rec s = |n| select n { 0 => 0, _ => s(n - a) }; s(3) }; g(0) }; f(1)
ok |x| [x, cast<string>(x), |acc, (k, v)| map::insert(acc, k, x)]; 100
ok let v0 = (cast<i64>(1)$, 1); let v1 = select v0 { (2, 1) => 1.0, (_, v3) => 2.5 }; let tm = |v2| v0.0; tm(0)
ok let err0: Error<ErrChain<`A(string)>> = never(); catch(e) err0 <- e; catch(e) e?; 10
ok let ka = |s| { let r = (|x: [i64, 'a]| -> [i64, 'a] x)(s); "[r]" }; (ka("abc"), ka(1.5))
ok let v1 = [&"a"]; let v4 = |v5: i64| { let t = v1[-1]; *(t$) }; let s: string = v4(1); s
no "i64 does not contain string" let v1 = [&"a"]; let v4 = |v5: i64| { let t = v1[-1]; *(t$) }; let s: i64 = v4(1); s
no "i64 does not contain string" let v1 = [{x: "a"}]; let v4 = |v5: i64| { let t = v1[-1]; (t$).x }; let s: i64 = v4(1); s
no "i64 does not contain string" let fs = [|x: i64| x + 1]; let v4 = |a| { let t = fs[0]; (t$)(a) }; v4("s")
ok let f = |x| [<1>] == { let t = [<x>]; t }; (f(1), f(2))
no "cannot compare List<i64> with List<string>" let f = |x| [<1>] == { let t = [<x>]; t }; f("a")
ok let f = |x, y| cast<[i64, duration]>("3.0")$ == { let t = [<0, x>]; t }; 1
ok let x = never(); type T = [i64, null]; let p: T = x; let r = [10, 20][-x]$; 3
no "used as T and as" let x = never(); type T = [string, null]; let p: T = x; let r = [10, 20][-x]$; 3
ok let s = select { let t = `Done({ 1; 0; }); t } { error as e => 1, null as _ => 2 }; 5
no "nothing matches `Done(i64)" select { let t = `Done(0); t } { error as e => 1, null as _ => 2 }
ok let inner = |x: i64| -> i64 seq let a = x { a + 1 }; let relay = |x: i64| { let r = 0; catch(e) r <- e; inner(x) }; relay(1)
ok let y: [Array<i64>, Array<Array<i64>>] = [[1], [2, 3]]; Collection::fold(y, 0, |acc, x| acc + 1)
no "is in a labeled argument's default" let f = |#foo: i64 = { type Tag = Abstract<string>; impl Cast for Tag; 1 }, x| foo; (f(1), f(2))
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
