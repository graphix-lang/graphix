# Generics: type variables and what stays generic

What the check enforces of a function's signature and of a function held as a
value. Every rule here refuses and names the fix.

A declared type variable must appear in the signature: on a parameter,
the return type, the throws, or in the bound of another declared
variable that does (never the bound of an enclosing definition's
variable). `'a: Number |x: i64| -> i64 { let v: 'a = 1; x }`
is refused (no call could choose `'a`), and so is a written
`fn<'a: Number>(x: i64) -> i64`. A bound may not name its own variable,
directly or through another's bound (`'a: [i64, Array<'a>]`, `'a: 'b,
'b: 'a`): bound it by a recursive type, `type T = [i64, Array<T>]` and
`'a: T`. It may name another declared variable, inside a union too
(`'x: Array<['a, null]>`). Each call picks a declared variable anew, so a value of it
may not reach a binding outside the function: `'a: Concrete |x: 'a| -> 'a
{ y <- x; x }` is refused. Give `y` a type that holds every `'a` (the
bound, or `Any`), or declare no `'a`: `|x: 'a| -> 'a { y <- x; x }` is
then one type, the first call's.

```graphix,cases
no "declared but the signature does not hold" 'a: Number |x: i64| -> i64 { let v: 'a = 1; x }
no "declared but the signature does not hold" let f: fn<'a: Number>(x: i64) -> i64 = |x| x; f(1)
no "declared but the signature does not hold" let f = 'a: Number, 'z: Array<'a> |w: 'z| { let g = 'a: Any |x: 'z| -> i64 0; g(w) }; f([1])
no "bound names" 'a: [i64, Array<'a>] |x: 'a| x
no "bound reaches" 'a: 'b, 'b: 'a |x: 'a, y: 'b| x
ok let f = 'x: Array<['a, null]> |y: 'x| y; f([1])
ok let g = 'b: ['c, null] |s: 'c, x: 'b| -> 'b x; let f = |v: 'a, w: ['a, null]| g(v, w); f(1, 2)
no "escapes the definition" let y = never(); let f = 'a: Concrete |x: 'a| -> 'a { y <- x; x }; f(1)
ok let y = never(); let f = |x: 'a| -> 'a { y <- x; x }; f(1)
no "write its return type, `-> 'a`" let t = 'a: Number |x: 'a| x; let r = &mut t; t(1)
ok let t = 'a: Number |x: 'a| -> 'a x; let r = &mut t; t(1)
```

A function written inside another names the outer one's variables, in its
parameters, return type and bounds alike, unless it declares the name
itself. One it declares is its own, whatever an outer bound names: inside
`'a: Number, 'z: Array<'a> |w: 'z|`, a `'a: Any |x: 'z, y: 'a| -> 'a`
returns its own `'a`, never an element of `x`. The outer one's variables
are those its signature writes: in a parameter, the return type, or a
bound of one of those, however many bounds deep.

```graphix,cases
ok let g = 'a0: Array<'a1>, 'a1: Array<'a2> |x: 'a0| { let f = |y: 'a2| y; f(x[0]$[0]$) }; g([["s"]])
no "does not contain i64" let g = 'a0: Array<'a1>, 'a1: Array<'a2> |x: 'a0| { let f = |y: 'a2| y; f(1) }; g([["s"]])
no "does not contain Array<string>" let f = 'z: Number |w: 'z| -> 'z { let g = 'b: Array<'z> |y: 'b| -> 'b y; let r = g(["s"]); w }; f(1)
ok let f = 'z: Number |w: 'z| { let g = 'b: Array<'z> |y: 'b| -> 'b y; g([w]) }; f(1)
ok let f = 'a: Number, 'z: Array<'a> |w: 'z| { let g = 'a: Any |x: 'z, y: 'a| -> 'a y; g(w, "s") }; f([1])
no "a call picks 'a" let f = 'a: Number, 'z: Array<'a> |w: 'z| { let g = 'a: Any |x: 'z, y: 'a| -> 'a x[0]$; g(w, "s") }; f([1])
```

A return type that writes a declared variable takes a value of that
variable, and a value of its bound is not one, since a call may pick a
narrower type: `'a: Number |x: 'a| -> 'a 1` is refused. So is `'a: {n:
i64} |s: 'a| -> 'a {s with n: s.n + 1}`: an update's type is a struct,
as a literal's is. Drop the return type, or write the body's (`-> {n:
i64}`). Another declared variable is not that variable either: `|x: 'a,
y: 'b| -> 'a y`. A trait method's return is its trait's, so there the
body must change and the refusal names no return type.

```graphix,cases
no "drop `-> 'a`, or write the body's type, `-> i64`" 'a: Number |x: 'a| -> 'a 1
no "drop `-> 'a`, or write the body's type, `-> { n: i64 }`" 'a: {n: i64} |s: 'a| -> 'a {s with n: s.n + 1}
ok 'a: {n: i64} |s: 'a| {s with n: s.n + 1}
ok 'a: {n: i64} |s: 'a| -> {n: i64} {s with n: s.n + 1}
no "drop `-> 'a`, or write the body's type, `-> 'b`" |x: 'a, y: 'b| -> 'a y
ok |x: 'a, y: 'b| -> 'b y
```

A callback whose result writes the variable returns a value of it, each
call's own, in a definition and a trait method alike; held in a binding
of the bound's type it is a value of the bound.

```graphix,cases
ok let m = 'b: Number |s: i64, f: fn(x: i64) -> 'b| -> 'b f(s); let r: f64 = m(3, |x| 1.5); r
ok trait T { val m: fn<'b: [i64, null]>(self, f: fn(x: self) -> 'b) -> 'b }; impl T for i64 { let m = |s, f| f(s) }; let r: i64 = T::m(3, |x| x + 1); r
no "drop `-> 'b`, or write the body's type, `-> [i64, null]`" 'b: [i64, null] |s: i64, f: fn(x: i64) -> 'b| -> 'b { let r: [i64, null] = f(s); r }
```

An unannotated parameter the declared result takes is of that variable,
from one arm or from several, and a call must give it the type it gives
the variable.

```graphix,cases
ok let f = |x: 'e, c: bool, a, b| -> 'e select c { true => a, false => b }; f(1, true, 2, 3)
no "i64 does not contain string" let f = |x: 'e, c: bool, a, b| -> 'e select c { true => a, false => b }; f(1, true, 2, "s")
no "drop `-> 'e`, or write the body's type" |x: 'e, c: bool, a| -> 'e select c { true => a, false => 1 }
```

A signature may not write a union of two of its type variables, `|x:
['a, 'b]|`, `Array<['a, 'b]>`, `-> ['a, 'b, null]`, a `.gxi` val's or a
trait method's alike, nor a function type a `let` annotation writes: no
value's type says which variable it takes. Keep one variable there
(`['a, null]` and a trait method's `[self, 'a]` are fine) or write the
types (`[i64, string]`). In an impl, `self` is the impl's target, so
`[self, 'a]` in `impl T for i64` is `[i64, 'a]`, one variable beside a
written type.

```graphix,cases
no "union of the type variables" |x: ['a, 'b]| 1
no "union of the type variables" |x: Array<['a, 'b]>| 1
ok |x: ['a, null]| 1
ok trait T { val m: fn(self, x: [self, 'a]) -> i64 }; impl T for i64 { let m = |s, x| 0 }; T::m(1, 2)
ok trait T { val m: fn(self, x: [i64, string]) -> i64 }; impl T for i64 { let m = |s, x: [self, 'c]| 0 }; T::m(1, "q")
```

Two members of one constructor holding two of its variables ask the
same question: `[Array<'c>, Array<'d>]` (a `[1]` fits either), `[{v:
'c}, {v: 'd}]`, `[('c, i64), (i64, 'd)]`, `[fn(x: 'c) -> i64, fn(x: 'd)
-> i64]`. Write one variable in both, or a written type in place of one.
Members of two constructors stand, and a member written twice is one.

The same holds for a union nothing wrote: a value that two undecided
members of a union could take goes to neither, and the check names what
to annotate. It shows up two ways. A `let` over `never()` that took a
select of two undecided values (`u <- select c { true => x, false => y
}`) and is then written a value: declare `u` (`let u: [i64, string] =
never()`), or `x` and `y`. And a typedef that unions its parameters
(`type E<'p, 'q> = ['p, 'q]`) applied to two variables in a signature: a
call's arguments are met in the order of the parameters, so an argument
either variable could take is refused unless the parameters before it
decided them. Declare the deciding parameters first (`|y: 'a, x: E<'a,
'b>|`), or write one variable (`E<'a, 'a>`).
Members that are one type are one member and need nothing: an
unannotated parameter a select takes apart with slice patterns (`[h,
t..]`, two arms that bind elements, a recursive `len(t)`) is typed as one
array an arm, all over one element type, and takes an array as `|x:
Array<'e>|` does. Another part the arms bind (`([a, c, ..], b)`, `([d],
b)`, `([], b)`; `(1, b)`, `(2, b)`, `(_, b)`) is one type in every arm,
whichever arm a value takes: arms that use it as two (`b + 1` in one,
`str::len(b)` in another) are refused. An impl's target may
not union two of its head's variables either (`impl<'a, 'b> T for
Array<['a, 'b]>`). An argument still undecided (`[]`) goes to no member
either, unless the arguments before it decided the members. The parts of
a single argument are related together, so within one argument the
deciding parts may stand anywhere (`|x: (P<'a, 'b>, 'a, 'b)|` takes
`([1], 1, "s")`). A variable standing in such a
union does not widen: the first argument that meets it decides it, and
an argument that needs it wider (`[n]` over `n: [i64, null]` after `1`)
is refused, so give the earlier argument the wide type (`let one: [i64,
null] = 1`). A callback is given to the member its result fits.

```graphix,cases
no "declare u's type where it is bound" let h = |c, a, b| select c { true => a, false => b }; let x = never(); let y = never(); let u = never(); u <- h(true, x, y); u <- 1; u
ok let h = |c, a, b| select c { true => a, false => b }; let x = never(); let y = never(); let u: [i64, string] = never(); u <- h(true, x, y); u <- 1; u
ok let h = |c, a, b| select c { true => a, false => b }; h(true, 1, "s")
ok let f = |c, a, b| { let u = select c { true => a, false => b }; u + 1 }; f(true, 1, 2)
no "picks none by order" type E<'p, 'q> = ['p, 'q]; let f = |x: E<'a, 'b>| 0; f(1)
ok type E<'p, 'q> = ['p, 'q]; let f = |x: E<'a, 'a>| 0; f(1)
ok type E<'p, 'q> = ['p, 'q]; let f = |y: 'a, x: E<'a, 'b>| 0; f(1, 2)
no "declare the parameters that decide them before this one" type E<'p, 'q> = ['p, 'q]; let f = |x: E<'a, 'b>, y: 'a| 0; f(1, 2)
ok let f = |x| select x { [h, t..] => h, [] => 0 }; (f([4, 5]), f(["a"]))
ok let f = |x: Array<'e>| select x { [h, t..] => h, [] => 0 }; f([4, 5])
ok let f = |x| select x { [a, b, ..] => a, [c] => c, [] => 0 }; f([4, 5])
ok let rec len = |x| select x { [h, t..] => 1 + len(t), [] => 0 }; len([1, 2])
ok let f = |x| select x { [h, t..] => array::push(t, h), [] => [] }; f([1, 2])
ok let rec len = |x| select x { [<h, t..>] => 1 + len(t), [<>] => 0 }; len([<1, 2>])
ok let k = |x| select x { [h, t..] => h, [] => 0 }; let f = |g: fn(x: Array<i64>) -> i64, y| g(y); f(k, [1, 2])
ok let f = |p| select p { (x, [h, t..]) => h, (x, []) => x }; f((1, [2]))
ok let f = |s| select s { ([a, c, ..], b) => (a, b), ([d], b) => (b, d), ([], b) => (b, b) }; f(([1, 2], "q"))
ok let f = |s| select s { (1, b) => (b, 1), (2, b) => (b, 2), (3, b) => (b, 3), (_, b) => (b, 0) }; f((1, "q"))
no "string does not contain i64" let f = |s| select s { (1, b) => 0, (2, b) => b + 1, (3, b) => str::len(b), _ => 0 }; f((2, 5))
ok let f = |s: (Array<'e>, 'b)| select s { ([a, c, ..], b) => (a, b), ([d], b) => (b, d), ([], b) => (b, b) }; f(([1, 2], "q"))
no "picks none by order" type P<'p, 'q> = [Array<'p>, Array<'q>]; let f = |x: P<'a, 'b>| 0; f([1])
no "union of the type variables" trait T { val m: fn(self) -> i64 }; impl<'a, 'b> T for Array<['a, 'b]> { let m = |s| 0 }; T::m([1])
no "picks none by order" type P<'p, 'q> = [Array<'p>, Array<'q>]; let f = |x: P<'a, 'b>| 0; f([])
no "picks none by order" type P<'p, 'q> = [Array<'p>, Array<'q>]; let f = |x: P<'a, 'b>, y: 'a, z: 'b| x; f([1], 1, "s")
ok type P<'p, 'q> = [Array<'p>, Array<'q>]; let f = |y: 'a, z: 'b, x: P<'a, 'b>| x; f(1, "s", [1])
no "picks none by order" type P<'p, 'q> = [Array<'p>, Array<'q>]; let f = |y: 'a, z: 'b, x: P<'a, 'b>| x; f(never(), never(), [1])
ok type P<'p, 'q> = [Array<'p>, Array<'q>]; let f = |y: 'a, z: 'b, x: P<'a, 'b>| x; let h = |v: 'a, w: 'b, u: P<'a, 'b>| f(v, w, u); h(1, "s", [1])
ok type P<'p, 'q> = [Array<'p>, Array<'q>]; let f = |x: (P<'a, 'b>, 'a, 'b)| x; f(([1], 1, "s"))
no "picks none by order" type P<'p, 'q> = [Array<'p>, Array<'q>]; let f = |x: (P<'a, 'b>, P<'b, 'a>)| x; f(([1], ["s"]))
ok type P<'p, 'q> = [Array<'p>, Array<'q>]; let f = |x: ('a, 'b, P<'a, 'b>)| x; f((1, "s", [1]))
ok type P<'p, 'q> = [Array<'p>, Array<'q>]; let f = |x: {p: P<'a, 'b>, a: 'a, b: 'b}| 0; f({p: [1], a: 1, b: "s"})
ok type P<'p, 'q> = [Array<'p>, Array<'q>]; let f = |x: {y: 'a, z: 'b, a: P<'a, 'b>}| 0; f({y: 1, z: "s", a: [1]})
ok type E<'p, 'q> = ['p, 'q]; let f = |y: 'a, x: Map<string, E<'a, 'b>>| 0; f("s", {"k" => 1.5})
no "picks none by order" type E<'p, 'q> = ['p, 'q]; let f = |x: Map<string, E<'a, 'b>>, y: 'a| 0; f({"k" => 1.5}, "s")
ok type E<'p, 'q> = ['p, 'q]; let id = |q| q; let f = |y: 'a, x: fn(q: i64) -> E<'a, 'b>| 0; f("s", id(|q: i64| 1.5))
no "does not contain Array<[i64, null]>" type P<'p, 'q> = [Array<'p>, Array<'q>]; let n: [i64, null] = null; let f = |y: 'a, z: 'b, x: P<'a, 'b>| 0; f(1, "s", [n])
ok type P<'p, 'q> = [Array<'p>, Array<'q>]; let n: [i64, null] = null; let one: [i64, null] = 1; let f = |y: 'a, z: 'b, x: P<'a, 'b>| 0; f(one, "s", [n])
no "picks none by order" type P<'p, 'q> = [Array<'p>, Array<'q>]; let f = |x: P<'a, 'b>, y: 'a, z: 'b| 0; f([], 1, "s")
ok type P<'p, 'q> = [Array<'p>, Array<'q>]; let f = |y: 'a, z: 'b, x: P<'a, 'b>| 0; f(1, "s", [])
ok type P<'p, 'q> = [Array<'p>, Array<'q>]; let f = |x: P<i64, string>| 0; f([])
ok type P<'p, 'q> = [Array<'p>, Array<'q>]; let f = |y: 'a, z: 'b, x: P<'a, 'b>| 0; let g = |v, w, u| f(v, w, u); g(1, "s", [])
ok let f = |x: ('a, i64), z: 'a| x.1; let g = |y| f(y, y.0); g((1, 2))
no "picks none by order" type P<'p, 'q> = [Array<'p>, Array<'q>]; let f = 'a: P<'b, 'c> |x: 'a| x; let g = |y| f(y); g([null])
ok type E<'x, 'y> = [fn(v: 'x) -> i64, fn(v: 'y) -> string]; let f = 'a: E<'b, 'b>, 'b: Number |x: 'a, y: 'b| x; let h = f(|v| "s", 1); h(2)
ok type A = [Array<i64>, Array<string>]; let f = |x: [A, null]| 0; f(["a"])
```

```graphix,cases
no "held by its members" |x: [Array<'c>, Array<'d>], y: 'c| y
no "held by its members" |x: [('c, i64), (i64, 'd)], y: 'c| y
no "held by its members" |k: [fn(x: 'c) -> i64, fn(x: 'd) -> i64], y: 'c| y
ok |k: [fn(x: 'c) -> i64, fn(x: 'c) -> i64], y: 'c| y
ok |x: [('c, 'd), ('c, 'd)], y: 'c| y
ok |x: [Array<'c>, Array<string>], y: 'c| y
ok |x: [Array<'c>, Map<string, 'd>], y: 'c| y
```

Two constructions of one nominal type over payloads nothing has decided
(`B([never()])` and `B([])` over `type B<'p> = Abstract<'p>`) that meet
in one value are one payload type, not a union of two: what later
decides one decides the other. Two undecided bindings that meet that way
(`B([e])` written where `B([g])` stands) become one type at the first
call that returns the payload, and a writer of one that the other's does
not fit is refused: declare the binding it names, or a function's
parameters (`|a: 'a, b: 'b|`).

```graphix,cases
ok type B<'p> = Abstract<'p>; let f = |a| { a <- B([never()]); a.0 }; let r = f(B([])); r <- [1]; r
no "declare g: [i64, string] where it is bound" type B<'p> = Abstract<'p>; let e = never(); let g = never(); let x = B([e]); x <- B([g]); let h = |u| u ~ x.0; let r = h(1); e <- 1; g <- "s"; r
ok type B<'p> = Abstract<'p>; let e = never(); let g: [i64, string] = never(); let x = B([e]); x <- B([g]); let h = |u| u ~ x.0; let r = h(1); e <- 1; g <- "s"; r
no "i64 does not contain string" type B<'p> = Abstract<'p>; let f = |a, b| { let x = B([a]); x <- B([b]); let h = |u| u ~ x.0; h(1) }; f(1, "s")
ok type B<'p> = Abstract<'p>; let f = |a: 'a, b: 'b| { let x = B([a]); x <- B([b]); let h = |u| u ~ x.0; h(1) }; f(1, "s")
```

A function that builds a nominal value over one parameter into another
(`|a, z| a <- B([z])`) needs the payload its first argument decided to
hold what it builds. A payload still open at the call (`f(B([1]), "s")`)
takes both; one already declared takes no argument wider than it holds:
declare it as wide as the widest argument.

```graphix,cases
ok type B<'p> = Abstract<'p>; let f = |a, z| { a <- B([z]); a.0 }; f(B([1]), "s")
no "another argument of this call decided as Array<i64>" type B<'p> = Abstract<'p>; let n: [i64, null] = null; let f = |a, z| { a <- B([z]); a.0 }; let x: B<Array<i64>> = B([1]); f(x, n)
ok type B<'p> = Abstract<'p>; let n: [i64, null] = null; let f = |a, z| { a <- B([z]); a.0 }; let x: B<Array<[i64, null]>> = B([1]); f(x, n)
```

A function type with variables of its own (`fn<'a: Number>(x: 'a) ->
'a`, or a trait as a parameter's type inside a written function type,
`fn(c: Collection) -> i64`) stands only as a definition's signature: a
`.gxi` `val`'s type, a trait method's, a lambda's own declared
variables. As a parameter's type, a field, an element, a `let`
annotation, a typedef, a return type or anywhere else inside a type it
is refused: declare the variable on the definition, `'a: Number |f:
fn(x: 'a) -> 'a, x: 'a| f(x)`, write the parameter as a trait, `|c:
Collection| ..`, or bind the generic function with `let` and call it by
name.

```graphix,cases
no "type variables of its own" |f: fn<'a: Number>(x: 'a) -> 'a| 1
no "type variables of its own" let g: fn<'a: Number>(x: 'a) -> 'a = |x| x; g(1)
no "type variables of its own" let f = |g: fn(c: Collection) -> i64| 1; 1
ok let g = 'a: Number |f: fn(x: 'a) -> 'a, x: 'a| f(x); g(|x| x, 1)
ok |c: Collection| 1
```

A function stays generic only where a binding names it: a `let` of a
lambda (or of a data literal holding lambdas, below), a `val`, a trait
method, and a `let` of a generic name or of a field or tuple read of
one (`let j = s.g`, `let j = t.0`), or of a block whose value is one of
these (`let f = { let n = 1; |x| x }`). Anywhere else it is one
instance, which its uses decide: `let fs = array::map([1], |r| |x| x);
let g = fs[0]$` takes `g(1)` and then refuses `g("s")`, and so do a
function a call returns (`let h = mk(0)` over `let mk = |z| |x| x`), a
field of a call's result, `(id, 1).0`, a reference (`let r = &f`, then
`(*r)(1)`) and a binding something writes (`let j = s.g; j <- ..`).
Each call of `mk` is an instance of its own: `let a = mk(0); let b =
mk(0)` takes `a(1)` and `b("s")`. Whatever holds such an instance beside
a generic value is that instance too: with `let j = s.f; j <- ..`, a
`{s with f: j}`, a `[s, {f: j}]` (an array, list or map of composites),
a `[(s.f, 0), (j, 1)]` or a `select b { true => s, false => {f: j} }`
takes `j`'s one type in every element and arm. A select arm's name over
members that hold lambdas is those lambdas, not a copy: `select t { (r, n)
=> r.f(v) }` over `({f: |x| x}, 1)` and `({f: |y| y}, "s")` calls either
one, and `v`'s type is then the type both take. A `j` that starts as
`never()` stays open in `{s with f: j}`, whose field is `j`'s own type, so a
call through the update before the write is refused until `j`'s type is
written on its `let` (`let j: fn(x: i64) -> i64 = never()`). A write the
compiler meets only after
the binding was made generic (a function body's `m::j <- ..` in the
module's parent, a later REPL input) must be as general as the binding,
and a `&mut` of it is refused: to write it at one type, write that type
on its `let` (`let j: fn(x: i64) -> i64 = s.g`). Arms holding lambdas
over two declared variables (`|p: 'a| p` beside `|q: 'b| q`) stay two
members, and a call through them is refused, naming both: give both one
variable, or one written type.

```graphix,cases
no "i64 does not contain string" let fs = array::map([1], |r| |x| x); let g = fs[0]$; (g(1), g("s"))
ok let fs = array::map([1], |r| |x| x); let g = fs[0]$; g(1)
no "i64 does not contain string" let id = |x| x; let g = (id, 1).0; (g(1), g("s"))
no "i64 does not contain string" let f = |x| x; let r = &f; ((*r)(1), (*r)("s"))
no "i64 does not contain string" let s = {f: |x| x}; let j = s.f; let p = |b| select b { true => s, false => {f: j} }; j <- |x: i64| x; (p(true).f("s"), p(false).f(1))
ok let s = {f: |x| x}; let j = s.f; let p = |b| select b { true => s, false => {f: j} }; (p(true).f("s"), p(false).f(1))
no "i64 does not contain string" let mk = |z| |x| x; let s = {f: |x| x, g: |y| y}; let t = {s with g: mk(0)}; (t.g(1), t.g("s"))
ok let mk = |z| |x| x; let s = {f: |x| x, g: |y| y}; let t = {s with g: mk(0)}; (t.f(1), t.f("s"), t.g(2))
no "needs its type known here" let s = {f: |x| x}; let j = never(); let t = {s with f: j}; let r = t.f("s"); j <- |x: i64| x + 1; r
ok let s = {f: |x| x}; let j: fn(x: i64) -> i64 = never(); let t = {s with f: j}; let r = t.f(2); j <- |x: i64| x + 1; r
ok let k = |v| { let t = select v > 0 { true => ({f: |x| x}, 1), false => ({f: |y| y}, "s") }; select t { (r, n) => r.f(v) } }; (k(2), k(-2))
no "i64 does not contain string" let k = |v| { let t = select v > 0 { true => ({f: |x| x}, 1), false => ({f: |y| y}, "s") }; select t { (r, n) => (r.f(v), r.f("s")) } }; k(2)
no "one of fn(p: 'a: i64) -> 'a: i64 and fn(q: 'b: string) -> 'b: string; write one variable in both, or one written type in both" let f = |c: bool, x: 'a, y: 'b| select c { true => {g: |p: 'a| p}, false => {g: |q: 'b| q} }; f(false, 1, "s").g(2)
ok let f = |c: bool, x: 'a, y: 'b| select c { true => {g: |p: 'a| p}, false => {g: |q: 'a| q} }; f(false, 1, "s").g(2)
ok let f = |c: bool, x: 'a, y: 'b| select c { true => {g: |p: i64| p}, false => {g: |q: i64| q} }; f(false, 1, "s").g(2)
```

A message and the shell's `-:` line print a function type's `fn<'a: ..>`
header only while its declared variables are generic, each call still
picking them. One instance prints without it, each variable where it
occurs with its state: the next use that decides `'a` decides it
wherever that instance holds it. Two instances are two variables, and
in one message the second prints numbered apart (`'a1`), under that
name in every type the message prints; a name numbered apart is none
that type, or one the message printed before it, declares. An inferred
variable numbers apart too: a second instance's copy of `'_7` prints
`'_7_1`.

```graphix,cases
no "y is Array<fn(x: 'a: unbound within Number) -> 'a: unbound within Number>" let fs = array::map([1], |r| 'a: Number |x: 'a| -> 'a x); let y = never(); y <- fs; y <- 1
no "y is fn(x: 'a: unbound within Number) -> 'a: unbound within Number and" let mk = |z| 'a: Number |x: 'a| -> 'a x; let h = mk(0); let y = never(); y <- h; y <- 1
no "y is (fn(x: 'a: i64) -> 'a: i64, fn(x: 'a1: f64) -> 'a1: f64) and" let mk = |z: i64| 'a: Number |x: 'a| -> 'a x; let h = mk(0); let k = mk(1); let y = never(); y <- (h, k); let r = y.0(1); let s = y.1(1.5); y <- 1
no "y is (fn(x: 'a: unbound within Number) -> 'a: unbound within Number, fn(x: 'a1: f64) -> 'a1: f64) and cannot hold fn(x: 'a1: f64) -> 'a1: f64" let mk = |z: i64| 'a: Number |x: 'a| -> 'a x; let h = mk(0); let k = mk(1); let r = k(1.5); let y = never(); y <- (h, k); y <- k
no "(fn(x: 'a: unbound within Number) -> 'a: unbound within Number, fn(x: 'a2: unbound within Number) -> 'a2: unbound within Number, fn(x: 'a1: unbound within Number) -> 'a1: unbound within Number) and" let mk = |z: i64| 'a: Number |x: 'a| -> 'a x; let h = mk(0); let k = mk(1); let mk1 = |z: i64| 'a1: Number |x: 'a1| -> 'a1 x; let m = mk1(0); let y = never(); y <- (h, k, m); y <- 1
no "_1: unbound) -> (string, '_" let mk = |v| |p| (v, p); let c = true; let s = select c { true => mk(1), false => mk("s") }; let n: i64 = s; n
```

A `let` nothing writes that holds lambdas in a struct, tuple, array or
other data literal is as polymorphic as a `let` of a lambda: `let s = {f: |x| x}`
serves `s.f(1)` and `s.f("a")`, and an annotation of one use (`let t:
{f: fn(x: i64) -> i64} = s`) decides nothing of the others. Two lambdas
in one array or list are one function type (`[|x| x, |y| y]`), and a
call through it, or a function it is passed to, judges each one's labeled
defaults against that one label (`[|#d = 1, x| x, |#d = "q", x| x]` is
refused there, even by `array::len`: annotate the binding, `let fs:
Array<fn(?#d: [i64, string], x: i64) -> i64> = ..`; a direct call of a
named member judges its own). The members of a union meet the same way:
optional lambdas (`[fn(..), null]`) that one select or array joins are one
function, and a lambda beside an optional callback (`select b { true =>
cb, false => |x| x + 1 }`) is the callback's type. A name an or-pattern's
alternatives share, and a field read over two struct shapes, are one
function the same way where the signatures agree, and the open lambda is
that function from there on (`|x| x` beside `|y| y + 1` takes `i64`),
called or not; a call through the name judges each one's labeled
defaults as a call through an array's element does, and where the
signatures do not agree it is refused, naming both. A
`seq` step or a `seqq` request reads `s` as generically as a `let` of
it: `seqq t { (s.f(1), s.f("s")) }` checks, unless the seq writes `s`.
A `let` nothing writes or annotates reads `s` once in its value,
wherever it reads it: in `let t = {a: s, b: k(s)}`, a `k` taking `{f:
fn(x: i64) -> i64}` makes `t.a.f` take `i64` too, and so does `b: seqq
1 { k(s) }`. A lambda read through a field or a tuple position has the
type a read of its binding has: a union of function types its signature
holds over two of its variables (`x: F2<'a, 'b>`) is that union in the
call's result, and a call through the result is refused, naming the
members.

```graphix,cases
ok let s = {f: |x| x}; let t: {f: fn(x: i64) -> i64} = s; (t.f(1), s.f("a"))
no "i64 does not contain string" let fs = [|#d = 1, x: i64| x, |#d = "q", x: i64| x]; array::len(fs)
no "the functions met here share one #d" let j = 0; let o1 = select j { 0 => |#d = 1, x: i64| x, _ => null }; let o2 = select j { 0 => |#d = "q", x: i64| x, _ => null }; let a = [o1, o2]; select a[1]$ { null as _ => 0, f => f(7) }
ok let j = 0; let o1 = select j { 0 => |#d = 1, x: i64| x, _ => null }; let o2 = select j { 0 => |#d = "q", x: i64| x, _ => null }; let a: Array<[fn(?#d: [i64, string], x: i64) -> i64, null]> = [o1, o2]; select a[1]$ { null as _ => 0, f => f(7) }
ok let g = |cb: [fn(x: i64) -> i64, null], b: bool| { let h = select b { true => cb, false => |x| x + 1 }; select h { null as _ => 0, f => f(2) } }; g(null, false)
ok let b = true; let t = select b { true => `A(|x| x, 1), false => `B(|y| y + 1, "s") }; select t { `A(r, _) | `B(r, _) => r(2) }
no "i64 does not contain string" let b = true; let t = select b { true => `A(|x| x, 1), false => `B(|y| y + 1, "s") }; select t { `A(r, _) | `B(r, _) => r("s") }
no "a call takes one function type" let b = true; let t = select b { true => `A(|x: string| x, 1), false => `B(|y| y + 1, "s") }; select t { `A(r, _) | `B(r, _) => r(2) }
no "the functions met here share one #d" let b = true; let t = select b { true => `A(|#d = 1, x: i64| x), false => `B(|#d = "q", x: i64| x) }; select t { `A(r) | `B(r) => r(7) }
no "the functions met here share one #d" let b = true; let t = `W(select b { true => `A(|#d = 1, x: i64| x), false => `B(|#d = "q", x: i64| x) }); select t { `W(`A(r) | `B(r)) => r(7) }
ok let b = true; let t: [`A(fn(?#d: [i64, string], x: i64) -> i64), `B(fn(?#d: [i64, string], x: i64) -> i64)] = select b { true => `A(|#d = 1, x: i64| x), false => `B(|#d = "q", x: i64| x) }; select t { `A(r) | `B(r) => r(7) }
ok let t = 1; let s = {f: |x| x}; seq t { (s.f(1), s.f("s")) }
ok let t = 1; let s = {f: |x| x}; seqq t { (s.f(1), s.f("s")) }
ok let s = {f: |x| x}; let h = {g: |k| seqq k { (s.f(1), s.f("s")) }}; h.g(1)
no "i64 does not contain string" let k = |p: {f: fn(x: i64) -> i64}| 0; let s = {f: |x| x}; let t = {a: s, b: seqq 1 { k(s) }}; t.a.f("x")
ok type F2<'p, 'q> = [fn(q: 'p) -> i64, fn(q: 'q) -> i64]; let s = {fld: |y: 'a, z: 'b, x: F2<'a, 'b>| (x, y, z)}; s.fld(1, "s", |q: i64| 0)
no "a call takes one function type" type F2<'p, 'q> = [fn(q: 'p) -> i64, fn(q: 'q) -> i64]; let s = {fld: |y: 'a, z: 'b, x: F2<'a, 'b>| x}; let g = s.fld(1, "s", |q: i64| q + 1); g("s")
```

A sample is no binding: under a binding made generic before the write,
`m::c <- t ~ {f: |x| x}` holds one instance and is refused, where the direct
`m::c <- {f: |x| x}` names its lambda. Write it on a trigger by giving the
`let` or `val` one type.

A labeled default returning an enclosing function's open type requires a
closed defaulted parameter type. Annotate the factory's parameter
(`|z: i64|`) or the defaulted one (`|#d: i64 = z, x|`); `|z| { let g =
|#d = z, x| (d, x); g }` and `#d: Array<'q> = [z]` are refused. A
variable the factory declares is closed: `|z: 'a| { let g = |#d: 'a = z,
y: i64| d; g(1) }` checks, and `#d = z` there takes `'a`; a type written
inside the default (`= |q: i64| -> 'a z`) names it too.

```graphix,cases
no "enclosing open type" |z| { let g = |#d = z, x| (d, x); g }
ok |z: i64| { let g = |#d = z, x| (d, x); g }
ok |z: 'a| { let g = |#d: 'a = z, y: i64| d; g(1) }
```

A lambda with a labeled default that a declaration holds through a
typedef whose body writes the function type (`type W<'z> = {cb: fn(?#d:
'z, x: i64) -> string}`) must fit the declared type as written: a reader's
`w.cb` is a function nothing is known of, so no call can judge the
default. A return type `-> W<'z>` over `#d = 1` is refused: write `->
W<i64>`, or give the default the variable's type (`|z: 'z| .. #d = z`). A
`let w: W<'p> = ..` annotation takes `W<i64>` from the default. The
typedef's body written out in the declaration leaves each call to judge
its own default.

```graphix,cases
no "write the default's type in the declared type" type W<'z> = {cb: fn(?#d: 'z, x: i64) -> string}; let mk = |j: i64| -> W<'z> {cb: |#d = 1, x: i64| cast<string>(d)$}; mk(0).cb(7)
ok type W<'z> = {cb: fn(?#d: 'z, x: i64) -> string}; let mk = |j: i64| -> W<i64> {cb: |#d = 1, x: i64| cast<string>(d)$}; mk(0).cb(7)
ok type W<'z> = {cb: fn(?#d: 'z, x: i64) -> string}; let mk = |z: 'z| -> W<'z> {cb: |#d = z, x: i64| cast<string>(d)$}; mk("s").cb(7)
ok let mk = |j: i64| -> {cb: fn(?#d: 'z, x: i64) -> string} {cb: |#d = 1, x: i64| cast<string>(d)$}; mk(0).cb(#d: "s", 7)
no "i64 does not contain string" type W<'z> = {cb: fn(?#d: 'z, x: i64) -> string}; let w: W<'p> = {cb: |#d = 1, x: i64| cast<string>(d)$}; w.cb(#d: "s", 7)
```

A binding something writes holds one function type for every reader, so
a default written to it decides its label's variable: after `let v:
fn(?#d: 'q, x: i64) -> string = never(); v <- |#d = "q", x: i64| ..`,
`'q` is `string`, and a later write of a `#d = 1` or a call `v(#d: 1.5,
7)` is refused. Write the label's type wide enough for every default
(`?#d: [i64, string]`). The function the binding starts with counts, and
so does one held beside a null: `let o = select j { 0 => |#d = 1, ..| ..,
_ => null }; o <- |#d = "q", ..| ..` is refused at the write, whatever
the calls pass. A write through a parameter's `&mut fn(?#d: 'a,
..)` may not decide the function's own `'a`: write the label's type there
(`?#d: i64`).

```graphix,cases
ok let v: fn(?#d: 'q, x: i64) -> string = never(); v <- |#d = "q", x: i64| cast<string>(d)$; v(7)
no "string does not contain i64" let v: fn(?#d: 'q, x: i64) -> string = |#d = "a", x: i64| cast<string>(d)$; let t = 1; v <- t ~ |#d = 1, x: i64| cast<string>(d)$; v(7)
ok let v: fn(?#d: [i64, string], x: i64) -> string = |#d = "a", x: i64| cast<string>(d)$; let t = 1; v <- t ~ |#d = 1, x: i64| cast<string>(d)$; v(7)
no "the functions met here share one #d" let j = 0; let o = select j { 0 => |#d = 1, x: i64| cast<string>(d)$, _ => null }; let t = 1; o <- t ~ |#d = "q", x: i64| cast<string>(d)$; select o { null as _ => "n", f => f(#d: "z", 7) }
ok let j = 0; let o: [fn(?#d: [i64, string], x: i64) -> string, null] = select j { 0 => |#d = 1, x: i64| cast<string>(d)$, _ => null }; let t = 1; o <- t ~ |#d = "q", x: i64| cast<string>(d)$; select o { null as _ => "n", f => f(7) }
no "write the default's type in the binding's type" |r: &mut fn(?#d: 'a, x: i64) -> string, t: Any| { *r <- t ~ |#d = 1, x: i64| cast<string>(d)$; null }
ok |r: &mut fn(?#d: i64, x: i64) -> string, t: Any| { *r <- t ~ |#d = 1, x: i64| cast<string>(d)$; null }
```

What a function returns is judged the same way where the function
meets a fn type: a callback that returns a lambda with a default decides
the variable the formal's return gives the label, and a default behind an
abstract type's constructor is a lower bound of the type's parameter (the
declaration that takes the value may widen it).

```graphix,cases
ok let app = |f: fn(j: i64) -> fn(?#d: 'z, x: i64) -> string| f(0)(7); app(|j: i64| |#d = 1, x: i64| cast<string>(d)$)
no "string does not contain i64" let app = |f: fn(j: i64) -> fn(?#d: 'z, x: i64) -> string, v: 'z| f(0)(#d: v, 7); app(|j: i64| |#d = 1, x: i64| cast<string>(d)$, "s")
no "write the default's type in the declared type" type W<'z> = {cb: fn(?#d: 'z, x: i64) -> string}; let mk = |j: i64| -> fn(k: i64) -> W<'z> |k: i64| {cb: |#d = 1, x: i64| cast<string>(d)$}; mk(0)(0).cb(7)
ok type C<'z> = Abstract<fn(?#d: 'z, x: i64) -> string>; let c = C(|#d = 1, x: i64| cast<string>(d)$); c.0(7)
no "i64 does not contain string" type C<'z> = Abstract<fn(?#d: 'z, x: i64) -> string>; let c = C(|#d = 1, x: i64| cast<string>(d)$); c.0(#d: "s", 7)
ok type C<'z> = Abstract<fn(?#d: 'z, x: i64) -> string>; let c: C<[i64, string]> = C(|#d = 1, x: i64| cast<string>(d)$); c.0(#d: "s", 7)
no "write the body's type, `-> C<i64>`" type C<'z> = Abstract<fn(?#d: 'z, x: i64) -> string>; let mk = |j: i64| -> C<'z> C(|#d = 1, x: i64| cast<string>(d)$); mk(0).0(7)
```

A call's arguments are met in the order of the parameters, then the
labeled defaults it omits, and nothing waits for a later one. A generic
function's variable that only data holds widens to a decided argument
wider than the ones before it, so decided arguments settle it alike in
any order; one neither wider nor narrower than what stands is refused
(`f(1, "s")` over `|x: 'a, y: 'a|`), whatever follows: write the widest
first, or annotate. An argument or omitted default whose type is still
open (a `str::parse`, a `let` over `never()`, `[]`) takes the variable as
the ones before it left it, and the variable widens no more:
`opt::or_default(str::parse(s)$, 0)` reads an `i64`, `f(str::parse(s),
1)` over `|x: 'a, y: 'a|` runs (the read is the Result that holds the
`1`), and `f(1, str::parse(s))` is refused, "annotate it" (`'a` is
`i64`, which holds no parse error): annotate the read, or pass it first. What stands
must also fit what the open one's other uses ask of it, an operator that
read it or the values written where it is read from: `|p, q| { let s = p
+ q; array::push(["s"], p) }` is refused, since a string is no number.

```graphix,cases
ok let f = |x: 'a, y: 'a| -> 'a y; let n: [i64, null] = null; (f(1, n), f(n, 1))
no "'a: i64 does not contain string" let f = |x: 'a, y: 'a| -> 'a y; f(1, "s")
no "an undecided argument before this one met 'a" let f = |x: 'a, y: 'a, z: 'a| -> 'a z; let v = never(); let n: [i64, null] = null; f(v, 1, n)
ok let f = |x: 'a, y: 'a, z: 'a| -> 'a z; let v = never(); let n: [i64, null] = null; f(n, v, 1)
no "undecided at this" |p, q| { let s = p + q; array::push(["s"], p) }
ok |p, q| { let s = p + q; array::push([1], p) }
no "undecided at this" { type B<'p> = Abstract<'p>; let x = B(1); x <- B("a"); let v = x.0; array::push([10], v) }
```

A variable bounded by a union two members of which could hold the
argument (`'a: [Array<i64>, Array<string>]`, a `[&T, &mut U]`, two
function types) takes an argument whose type is decided. One still open
(`[z]` over a `let z = never()`, an untyped parameter, `[]`, an untyped
lambda) is refused where both members could hold it, "nothing has decided
which", naming the binding to annotate: which member holds it would
follow a later use. Where only one member can hold it that member decides
it (`(z, 1)` at `[(i64, i64), (string, string)]` makes `z` an `i64`). A
member written as a name, or as a variable another argument has decided,
counts as what it stands for.

```graphix,cases
no "nothing has decided which: `z` is undecided at this call; annotate `z`" let f = 'a: [Array<i64>, Array<string>] |v: 'a| v; let z = never(); let r = f([z]); z <- 1; r
ok let f = 'a: [Array<i64>, Array<string>] |v: 'a| v; let z: i64 = never(); let r = f([z]); z <- 1; r
no "nothing has decided which: this argument is undecided at this call; annotate it" let f = 'a: [Array<i64>, Array<string>] |v: 'a| v; f([])
ok let f = 'a: [Array<i64>, Array<string>] |v: 'a| v; let e: Array<string> = []; f(e)
no "cannot hold f64" let f = 'a: [(i64, i64), (string, string)] |v: 'a| v; let z = never(); let r = f((z, 1)); z <- 2.5; r
no "nothing has decided which: `z` is undecided at this call; annotate `z`" type A = Array<i64>; type B = Array<string>; let f = 'a: [A, B] |v: 'a| v; let z = never(); let r = f([z]); z <- 1; r
no "nothing has decided which" let f = 'a: [Array<i64>, 'b] |w: 'b, v: 'a| v; let z = never(); let r = f(["s"], [z]); z <- 1; r
ok let f = 'a: [Array<i64>, 'b] |w: 'b, v: 'a| v; let z = never(); let r = f("s", [z]); z <- 1; r
```

A bound holds through a chain of bounds. Under `'a0: ['a1, null], 'a1:
['a2, null]` the argument that decides `'a0` is a value `'a1` must hold,
and so one `'a2` must hold: a later `x2: 'a2` fits it or is refused. A
variable that widens hands its bound's variables the wider type the same
way.

```graphix,cases
ok let f = 'a0: ['a1, null], 'a1: ['a2, null] |x0: 'a0, x1: 'a1, x2: 'a2| x2; let v: ([i64, null], i64) = (1, 1); f(v, (1, 1), (null, 1))
no "'a2: ([i64, null], i64) does not contain string" let f = 'a0: ['a1, null], 'a1: ['a2, null] |x0: 'a0, x1: 'a1, x2: 'a2| x2; let v: ([i64, null], i64) = (1, 1); f(v, (1, 1), "s")
no "'a3: duration does not contain string" let f = 'a2: [('a3, bool), bool] |x1: Array<'a2>, x2: 'a2, x3: 'a3| x3; let n: [(duration, bool), bool] = (duration:1.s, true); f([true], n, "s")
```

A wrapper's declared variable takes no bound from a callee's. Inside the
value a callee's bound takes, the bound's own variable takes it, as where
the bound's type is written in the parameter; where the bound has no
variable there, the wrapper declares the bound itself.

```graphix,cases
ok let g = 'b: Array<['c, null]> |x: 'b, s: 'c| -> 'b x; let f = |v: 'a, w: Array<'a>| g(w, v); (f(1, [2]), f("a", ["b"]))
no "'c: unbound does not contain null" let g = 'b: Array<['c, null]> |x: 'b, s: 'c| -> 'b x; let f = |w: Array<'a>| g(w, null); f([null])
no "does not contain Array<'a: unbound>" let g = 'b: Array<[i64, null]> |x: 'b| -> 'b x; let f = |w: Array<'a>| g(w); f([2])
ok let g = 'b: Array<[i64, null]> |x: 'b| -> 'b x; let f = 'a: [i64, null] |w: Array<'a>| g(w); f([2])
```

A variable a callback also holds does not widen: the first argument decides
it, an open one included, so order decides between a value and a refusal.
`f(str::parse(s), 1, |v| 0)` over `|x: 'a, y: 'a, k: fn(v: 'a) -> i64|`
runs (`'a` is the parse's Result, which holds the `1`), and `f(1,
str::parse(s), |v| 0)` is refused, "annotate that argument as wide as this
one": annotating the parse widens nothing; annotate the argument that
decided `'a` (here the `1`), or put the open one first.

```graphix,cases
ok let f = |x: 'a, y: 'a, k: fn(v: 'a) -> i64| -> 'a y; f(str::parse("5"), 1, |v| 0)
no "annotate that argument as wide as this one" let f = |x: 'a, y: 'a, k: fn(v: 'a) -> i64| -> 'a y; f(1, str::parse("5"), |v| 0)
ok let f = |x: 'a, y: 'a, k: fn(v: 'a) -> i64| -> 'a y; let one: [i64, Error<`ParseError(string)>] = 1; f(one, str::parse("5"), |v| 0)
```

A generic function held at a written function type (an annotated `let`,
a callback parameter, a field, a write) meets the type's positions in
the order written, and its variable takes the first position's type:
the later ones must fit it, and nothing widens. Write the function
type's earlier position as wide as the later ones need, or wrap the
function in a lambda with its parameters written (`|b: i64, k: Any| f(b,
k)`), whose call widens. A trait
default's label typed by a bound over `self` stands before `self`: write
it as the declared variable (`|#k: 'b, s|`), or leave it unwritten.

```graphix,cases
no "does not contain fn(b: 'y: i64, k: 'y: i64)" let f = |b: 'y, k: 'y| 0; let g: fn(b: i64, k: Any) -> i64 = f; g(1, 2)
ok let f = |b: 'y, k: 'y| 0; let g: fn(b: i64, k: Any) -> i64 = |b: i64, k: Any| f(b, k); g(1, 2)
ok let f = |k: 'y, b: 'y| 0; let g: fn(k: Any, b: i64) -> i64 = f; g(2, 1)
ok let f = |k: 'y, b: 'y| 0; let app = |h: fn(k: Any, b: i64) -> i64| h(2, 1); app(f)
no "does not contain fn(b: 'y: i64, k: 'y: i64)" let f = |b: 'y, k: 'y| 0; let g: fn(b: i64, k: string) -> i64 = f; g(1, "s")
no "-> i64 does not contain" let f = |b: 'y, k: 'y| -> 'y k; let g: fn(b: i64, k: [i64, null]) -> i64 = f; g(1, null)
no "does not contain fn(b: 'y: i64" let f = |b: 'y, k: 'y, c: fn(v: 'y) -> i64| c(k); let g: fn(b: i64, k: Any, c: fn(v: Any) -> i64) -> i64 = f; g(1, 2, |v| 7)
ok let f = |b: 'y, k: 'y, c: fn(v: 'y) -> i64| c(k); let g: fn(b: Any, k: i64, c: fn(v: Any) -> i64) -> i64 = f; g(1, 2, |v| 7)
ok type L<'a> = [`Nil, `Cons('a, L<'a>)]; let f = |k: 'y, l: L<'y>| -> 'y k; let g: fn(k: [i64, null], l: L<i64>) -> [i64, null] = f; g(null, `Nil)
no "does not contain fn(l: L<'y: i64>" type L<'a> = [`Nil, `Cons('a, L<'a>)]; let f = |l: L<'y>, k: 'y| -> 'y k; let g: fn(l: L<i64>, k: [i64, null]) -> [i64, null] = f; g(`Nil, null)
ok type T<'a> = [`Leaf('a), `Node(T<i64>)]; let f = |k: 'y, t: T<'y>| -> 'y k; let g: fn(k: [i64, null], t: T<i64>) -> [i64, null] = f; g(null, `Leaf(1))
ok let g = 'b: [i64, null] |x: 'b| { let h: fn(s: i64, v: 'b) -> i64 = |s: 'a, v: ['a, null]| 0; h(1, x) }; g(null)
no "does not contain fn(v: [null, 'b" let g = 'b: [i64, null] |x: 'b| { let h: fn(v: 'b, s: i64) -> i64 = |v: ['a, null], s: 'a| 0; h(x, 1) }; g(null)
```

## Abstract comparisons

An abstract's `Eq` implementation bypasses representation checks for equality
only; `Ord` bypasses them for ordering only. Without the relevant implementation,
its representation must satisfy `Discernible` or `Ordered`, respectively.

## Recursive typedefs

A recursive typedef recurses through a constructor (`type T = [i64, T]`
is refused) and passes its parameters on as they are: one that applies
itself to a parameter wrapped in a constructor, a nested datatype
(``type N<'a> = [`Nil, `Cons('a, N<Array<'a>>)]``, `P<('a, 'a)>`), is
refused at its definition, naming the parameter, directly or through
other typedefs. Passing a parameter to another position, replacing it
with a closed type, or adding union members to it is fine, written out
or through an alias (`U<Opt<'a>>` over `type Opt<'a> = ['a, null]`, an
alias of an alias, `type I<'b> = 'b`). The alias is defined above the
recursive type and bounds no parameter, or the refusal names it (a name
the module declares below is that type, never an imported one); one
with a constructor around its parameter (`type R<'b> = Array<'b>`) grows
the type as the constructor does. A function over such a type names the
tail's type to recurse into it (a `let rec` is one instance), in either
spelling: `|v: U<Opt<'a>>|` is `|v: U<['a, null]>|`.

```graphix,cases
no "refers back to itself" type T = [i64, T]; 1
no "grows its parameter 'a" type N<'a> = [`Nil, `Cons('a, N<Array<'a>>)]; 1
no "grows its parameter 'b" type A<'a> = [`N, `C(B<'a>)]; type B<'b> = A<Array<'b>>; 1
ok type S<'a, 'b> = [`Nil, `C('a, S<'b, 'a>)]; 1
ok type K<'a> = [`Nil, `C('a, K<Array<i64>>)]; 1
ok type U<'a> = [`Nil, `C('a, U<['a, null]>)]; 1
ok type I<'b> = 'b; type U<'a> = [`Nil, `C('a, U<I<'a>>)]; 1
ok type Opt<'b> = ['b, null]; type Two<'b> = Opt<Opt<'b>>; type U<'a> = [`Nil, `C('a, U<Two<'a>>)]; let u: U<i64> = `C(1, `C(null, `Nil)); u
no "grows its parameter 'a" type R<'b> = Array<'b>; type U<'a> = [`Nil, `C('a, U<R<'a>>)]; 1
no "through Opt, which is not defined above" type U<'a> = [`Nil, `C('a, U<Opt<'a>>)]; type Opt<'b> = ['b, null]; 1
no "through Result, which is not defined above" type U<'a> = [`Nil, `C('a, U<Result<'a, `E>>)]; type Result<'b, 'c> = ['b, 'c, null]; 1
no "grows its parameter 'a" type N<'a> = [`Nil, `Cons('a, Result<'a, `E>)]; type Result<'a, 'b> = [N<Array<'a>>, 'b]; 1
no "through Num, which bounds a parameter" type Num<'b: Number> = ['b, null]; type U<'a: Number> = [`Nil, `C('a, U<Num<'a>>)]; 1
ok type Opt<'b> = ['b, null]; type U<'a> = [`Nil, `C('a, U<Opt<'a>>)]; let rec go = |v: U<Opt<'a>>, n: i64| -> i64 select v { `Nil => n, `C(_, t) => go(t, n + 1) }; let u: U<Opt<i64>> = `C(1, `C(null, `Nil)); go(u, 0)
ok type Opt<'b> = ['b, null]; let f = |x: [Opt<'a>, null]| -> Opt<'a> x; f(1)
ok type Opt<'b> = ['b, null]; let f = |x: [Opt<'a>, i64, null]| -> [Opt<'a>, i64] x; f(1)
ok type Opt<'b> = ['b, null]; let f = 'a: Number |x: [Opt<'a>, string]| x; let v: [Opt<i64>, string] = 1; f(v)
```
