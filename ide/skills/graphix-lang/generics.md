# Generics: type variables and what stays generic

What the check enforces of a function's signature and of a function held as a
value. Every rule here refuses and names the fix.

A declared type variable must appear in the signature: on a parameter,
the return type, the throws, or in the bound of another declared
variable that does. `'a: Number |x: i64| -> i64 { let v: 'a = 1; x }`
is refused (no call could choose `'a`), and so is a written
`fn<'a: Number>(x: i64) -> i64`. A bound may not name its own variable,
directly or through another's bound (`'a: [i64, Array<'a>]`, `'a: 'b,
'b: 'a`): bound it by a recursive type, `type T = [i64, Array<T>]` and
`'a: T`. Each call picks a declared variable anew, so a value of it
may not reach a binding outside the function: `'a: Concrete |x: 'a| -> 'a
{ y <- x; x }` is refused. Give `y` a type that holds every `'a` (the
bound, or `Any`), or declare no `'a`: `|x: 'a| -> 'a { y <- x; x }` is
then one type, the first call's.

```graphix,cases
no "declared but the signature does not hold" 'a: Number |x: i64| -> i64 { let v: 'a = 1; x }
no "declared but the signature does not hold" let f: fn<'a: Number>(x: i64) -> i64 = |x| x; f(1)
no "bound names" 'a: [i64, Array<'a>] |x: 'a| x
no "bound reaches" 'a: 'b, 'b: 'a |x: 'a, y: 'b| x
no "escapes the definition" let y = never(); let f = 'a: Concrete |x: 'a| -> 'a { y <- x; x }; f(1)
ok let y = never(); let f = |x: 'a| -> 'a { y <- x; x }; f(1)
no "write its return type, `-> 'a`" let t = 'a: Number |x: 'a| x; let r = &mut t; t(1)
ok let t = 'a: Number |x: 'a| -> 'a x; let r = &mut t; t(1)
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
to annotate. It shows up three ways. A `let` over `never()` that took a
select of two undecided values (`u <- select c { true => x, false => y
}`) and is then written a value: declare `u` (`let u: [i64, string] =
never()`), or `x` and `y`. A typedef that unions its parameters (`type
E<'p, 'q> = ['p, 'q]`) applied to two variables in a signature: an
argument either variable could take is refused, unless an earlier
parameter decided one of them, so write one variable (`E<'a, 'a>`). And
an unannotated parameter a select takes apart with slice patterns, where
an arm binds the rest (`[h, t..]`) or two arms bind elements: each such
arm types the parameter as an array of its own, so write the parameter
(`|x: Array<'e>|`). One arm that binds elements beside `[]` or `_` needs
nothing. An impl's target may not union two of its head's variables
either (`impl<'a, 'b> T for Array<['a, 'b]>`). An argument still undecided
(`[]`) goes to no member either: it waits for the other parameters to
decide the members, and is refused where none does. A callback is given
to the member its result fits.

```graphix,cases
no "declare u's type where it is bound" let h = |c, a, b| select c { true => a, false => b }; let x = never(); let y = never(); let u = never(); u <- h(true, x, y); u <- 1; u
ok let h = |c, a, b| select c { true => a, false => b }; let x = never(); let y = never(); let u: [i64, string] = never(); u <- h(true, x, y); u <- 1; u
ok let h = |c, a, b| select c { true => a, false => b }; h(true, 1, "s")
ok let f = |c, a, b| { let u = select c { true => a, false => b }; u + 1 }; f(true, 1, 2)
no "picks none by order" type E<'p, 'q> = ['p, 'q]; let f = |x: E<'a, 'b>| 0; f(1)
ok type E<'p, 'q> = ['p, 'q]; let f = |x: E<'a, 'a>| 0; f(1)
ok type E<'p, 'q> = ['p, 'q]; let f = |y: 'a, x: E<'a, 'b>| 0; f(1, 2)
no "picks none by order" type E<'p, 'q> = ['p, 'q]; let f = |x: E<'a, 'b>, y: 'a| 0; f(1, 2)
no "write the type of the parameter" let f = |x| select x { [h, t..] => h, [] => 0 }; f([4, 5])
ok let f = |x: Array<'e>| select x { [h, t..] => h, [] => 0 }; f([4, 5])
ok let f = |x| select x { [h, ..] => h, [] => 0 }; f([4, 5])
no "write the type of the parameter" let f = |x| select x { [a, b, ..] => a, [c] => c, [] => 0 }; f([4, 5])
ok let f = |x: Array<'e>| select x { [a, b, ..] => a, [c] => c, [] => 0 }; f([4, 5])
no "union of the type variables" trait T { val m: fn(self) -> i64 }; impl<'a, 'b> T for Array<['a, 'b]> { let m = |s| 0 }; T::m([1])
no "picks none by order" type P<'p, 'q> = [Array<'p>, Array<'q>]; let f = |x: P<'a, 'b>| 0; f([])
ok type P<'p, 'q> = [Array<'p>, Array<'q>]; let f = |x: P<'a, 'b>, y: 'a, z: 'b| 0; f([], 1, "s")
ok type P<'p, 'q> = [Array<'p>, Array<'q>]; let f = |x: P<i64, string>| 0; f([])
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
occurs with its state: the next use that decides `'a` decides it for
every other.

```graphix,cases
no "y is Array<fn(x: 'a: unbound within Number) -> 'a: unbound within Number>" let fs = array::map([1], |r| 'a: Number |x: 'a| -> 'a x); let y = never(); y <- fs; y <- 1
no "y is fn(x: 'a: unbound within Number) -> 'a: unbound within Number and" let mk = |z| 'a: Number |x: 'a| -> 'a x; let h = mk(0); let y = never(); y <- h; y <- 1
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
cb, false => |x| x + 1 }`) is the callback's type. A
`seq` step or a `seqq` request reads `s` as generically as a `let` of
it: `seqq t { (s.f(1), s.f("s")) }` checks, unless the seq writes `s`.
A `let` nothing writes or annotates reads `s` once in its value,
wherever it reads it: in `let t = {a: s, b: k(s)}`, a `k` taking `{f:
fn(x: i64) -> i64}` makes `t.a.f` take `i64` too, and so does `b: seqq
1 { k(s) }`.

```graphix,cases
ok let s = {f: |x| x}; let t: {f: fn(x: i64) -> i64} = s; (t.f(1), s.f("a"))
no "i64 does not contain string" let fs = [|#d = 1, x: i64| x, |#d = "q", x: i64| x]; array::len(fs)
no "the functions met here share one #d" let j = 0; let o1 = select j { 0 => |#d = 1, x: i64| x, _ => null }; let o2 = select j { 0 => |#d = "q", x: i64| x, _ => null }; let a = [o1, o2]; select a[1]$ { null as _ => 0, f => f(7) }
ok let j = 0; let o1 = select j { 0 => |#d = 1, x: i64| x, _ => null }; let o2 = select j { 0 => |#d = "q", x: i64| x, _ => null }; let a: Array<[fn(?#d: [i64, string], x: i64) -> i64, null]> = [o1, o2]; select a[1]$ { null as _ => 0, f => f(7) }
ok let g = |cb: [fn(x: i64) -> i64, null], b: bool| { let h = select b { true => cb, false => |x| x + 1 }; select h { null as _ => 0, f => f(2) } }; g(null, false)
ok let t = 1; let s = {f: |x| x}; seq t { (s.f(1), s.f("s")) }
ok let t = 1; let s = {f: |x| x}; seqq t { (s.f(1), s.f("s")) }
ok let s = {f: |x| x}; let h = {g: |k| seqq k { (s.f(1), s.f("s")) }}; h.g(1)
no "i64 does not contain string" let k = |p: {f: fn(x: i64) -> i64}| 0; let s = {f: |x| x}; let t = {a: s, b: seqq 1 { k(s) }}; t.a.f("x")
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

A generic function's variable that only data holds takes the widest
argument, in either order, an omitted labeled default included (one
that is neither wider nor narrower waits for the others to widen it). An
argument or omitted default whose type is still open (a `str::parse`, a
`let` over `never()`, `[]`) takes what the others decide:
`opt::or_default(str::parse(s)$, 0)` reads an `i64`, and
`f(str::parse(s), 1)` over `|x: 'a, y: 'a|` is refused, "annotate it"
(`'a` is `i64`, which holds no parse error): annotate the read.

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
a callback parameter, a field, a write) decides its variables the same
way: one that only data holds takes the widest of the written positions,
in either order, and with none that holds the others it is refused, as a
call is. One a callback also holds takes the first position's type: write
the function type's earlier position as wide as the later ones need. So
does one a recursive typedef passes on to itself (`l: L<'y>` over `type
L<'a> = [`Nil, `Cons('a, L<'a>)]`).

```graphix,cases
ok let f = |b: 'y, k: 'y| 0; let g: fn(b: i64, k: Any) -> i64 = f; g(1, 2)
ok let f = |k: 'y, b: 'y| 0; let g: fn(k: Any, b: i64) -> i64 = f; g(2, 1)
ok let f = |b: 'y, k: 'y| 0; let app = |h: fn(b: i64, k: Any) -> i64| h(1, 2); app(f)
no "does not contain fn(b: 'y: i64, k: 'y: i64)" let f = |b: 'y, k: 'y| 0; let g: fn(b: i64, k: string) -> i64 = f; g(1, "s")
no "-> i64 does not contain" let f = |b: 'y, k: 'y| -> 'y k; let g: fn(b: i64, k: [i64, null]) -> i64 = f; g(1, null)
no "does not contain fn(b: 'y: i64" let f = |b: 'y, k: 'y, c: fn(v: 'y) -> i64| c(k); let g: fn(b: i64, k: Any, c: fn(v: Any) -> i64) -> i64 = f; g(1, 2, |v| 7)
ok let f = |b: 'y, k: 'y, c: fn(v: 'y) -> i64| c(k); let g: fn(b: Any, k: i64, c: fn(v: Any) -> i64) -> i64 = f; g(1, 2, |v| 7)
ok type L<'a> = [`Nil, `Cons('a, L<'a>)]; let f = |k: 'y, l: L<'y>| -> 'y k; let g: fn(k: [i64, null], l: L<i64>) -> [i64, null] = f; g(null, `Nil)
no "does not contain fn(l: L<'y: i64>" type L<'a> = [`Nil, `Cons('a, L<'a>)]; let f = |l: L<'y>, k: 'y| -> 'y k; let g: fn(l: L<i64>, k: [i64, null]) -> [i64, null] = f; g(`Nil, null)
ok type T<'a> = [`Leaf('a), `Node(T<i64>)]; let f = |t: T<'y>, k: 'y| -> 'y k; let g: fn(t: T<i64>, k: [i64, null]) -> [i64, null] = f; g(`Leaf(1), null)
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
recursive type and bounds no parameter, or the refusal names it; one
with a constructor around its parameter (`type R<'b> = Array<'b>`) grows
the type as the constructor does.

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
no "through Num, which bounds a parameter" type Num<'b: Number> = ['b, null]; type U<'a: Number> = [`Nil, `C('a, U<Num<'a>>)]; 1
```
