# Generics: type variables and what stays generic

What the check enforces of a function's signature, of a call's arguments
and of a function held as a value.

## Declared variables

A declared type variable must appear in the signature: on a parameter,
the return type, the throws, or in the bound of another declared
variable that does (never the bound of an enclosing definition's
variable). `'a: Number |x: i64| -> i64 { let v: 'a = 1; x }`
is refused (no call could choose `'a`), and so is a written
`fn<'a: Number>(x: i64) -> i64`. A bound may not name its own variable,
directly or through another's bound (`'a: [i64, Array<'a>]`, `'a: 'b,
'b: 'a`): bound it by a recursive type, `type T = [i64, Array<T>]` and
`'a: T`. It may name another declared variable, inside a union too
(`'x: Array<['a, null]>`). Each call picks a declared variable anew. A
value of it written to a binding outside the function (`y <- x`) flows
into that binding as any value does: an unannotated `y` is the union of
what the calls pass, and an annotated one must hold every `'a` (the
bound's type, or `Any`).

```graphix,cases
no "declared but the signature does not hold" 'a: Number |x: i64| -> i64 { let v: 'a = 1; x }
no "declared but the signature does not hold" let f: fn<'a: Number>(x: i64) -> i64 = |x| x; f(1)
no "declared but the signature does not hold" let f = 'a: Number, 'z: Array<'a> |w: 'z| { let g = 'a: Any |x: 'z| -> i64 0; g(w) }; f([1])
no "bound names" 'a: [i64, Array<'a>] |x: 'a| x
no "bound reaches" 'a: 'b, 'b: 'a |x: 'a, y: 'b| x
ok let f = 'x: Array<['a, null]> |y: 'x| y; f([1])
ok let g = 'b: ['c, null] |s: 'c, x: 'b| -> 'b x; let f = |v: 'a, w: ['a, null]| g(v, w); f(1, 2)
ok let y = never(); let f = 'a: Number |x: 'a| -> 'a { y <- x; x }; (f(1), f(1.5))
no "i64 does not contain f64" let y = never(); let f = 'a: Number |x: 'a| -> 'a { y <- x; x }; let r = (f(1), f(1.5)); let z: i64 = y; z
no "y is i64 and cannot hold 'a" let y: i64 = never(); let f = 'a: Number |x: 'a| -> 'a { y <- x; x }; f(1)
ok let y: Number = never(); let f = 'a: Number |x: 'a| -> 'a { y <- x; x }; f(1)
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
no "does not contain string" let f = 'z: Number |w: 'z| -> 'z { let g = 'b: Array<'z> |y: 'b| -> 'b y; let r = g(["s"]); w }; f(1)
ok let f = 'z: Number |w: 'z| { let g = 'b: Array<'z> |y: 'b| -> 'b y; g([w]) }; f(1)
ok let f = 'a: Number, 'z: Array<'a> |w: 'z| { let g = 'a: Any |x: 'z, y: 'a| -> 'a y; g(w, "s") }; f([1])
no "a call picks 'a" let f = 'a: Number, 'z: Array<'a> |w: 'z| { let g = 'a: Any |x: 'z, y: 'a| -> 'a x[0]$; g(w, "s") }; f([1])
```

## Return types

A return type that writes a type variable, declared or bare, takes a
value of that variable, and a value of its bound is not one, since a
call may pick a narrower type: `'a: Number |x: 'a| -> 'a 1` is refused.
So is `'a: {n: i64} |s: 'a| -> 'a {s with n: s.n + 1}`: an update's type
is a struct, as a literal's is. Drop the return type, or write the
body's (`-> {n: i64}`). Another variable is not that variable either:
`|x: 'a, y: 'b| -> 'a y`. A trait method's return is its trait's, so
there the body must change and the refusal names no return type.

```graphix,cases
no "drop `-> 'a`, or write the body's type, `-> i64`" 'a: Number |x: 'a| -> 'a 1
no "drop `-> 'a`, or write the body's type, `-> { n: i64 }`" 'a: {n: i64} |s: 'a| -> 'a {s with n: s.n + 1}
ok 'a: {n: i64} |s: 'a| {s with n: s.n + 1}
ok 'a: {n: i64} |s: 'a| -> {n: i64} {s with n: s.n + 1}
no "drop `-> 'a`, or write the body's type, `-> i64`" |x: 'a| -> 'a 1
no "drop `-> 'a`, or write the body's type, `-> 'b`" |x: 'a, y: 'b| -> 'a y
ok |x: 'a, y: 'b| -> 'b y
no "does not contain i64" trait T { val m: fn<'b: Number>(self, x: 'b) -> 'b }; impl T for i64 { let m = |s, x| 1 }; T::m(1, 2)
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
from one arm or from several, and what a call passes there widens the
variable as an argument of `x: 'e` does.

```graphix,cases
ok let f = |x: 'e, c: bool, a, b| -> 'e select c { true => a, false => b }; f(1, true, 2, 3)
ok let f = |x: 'e, c: bool, a, b| -> 'e select c { true => a, false => b }; f(1, true, 2, "s")
no "i64 does not contain string" let f = |x: 'e, c: bool, a, b| -> 'e select c { true => a, false => b }; let r: i64 = f(1, true, 2, "s"); r
no "drop `-> 'e`, or write the body's type" |x: 'e, c: bool, a| -> 'e select c { true => a, false => 1 }
```

## What a call decides

A call's arguments of one variable widen it: `f(1, "s")` over `|x: 'a,
y: 'a| -> 'a y` makes `'a` an `[i64, string]`, in any order, and the
result is that union. Two things refuse. A bound: `'a: Number` takes no
string. And a body that takes one type: arithmetic over the `[i64, f64]`
a call made is refused at the call, "cannot compute with [i64, f64]"
(cast one argument). A callback's parameter of the variable is the
widened type: an untyped lambda is given it, a written one must hold it
(`|v: [i64, string]|`). A variable only callbacks take, with no value
passed for it, is bottom, so callbacks over different types both fit.

```graphix,cases
ok let f = |x: 'a, y: 'a| -> 'a y; let n: [i64, null] = null; (f(1, n), f(n, 1))
ok let f = |x: 'a, y: 'a| -> 'a y; let r: [i64, string] = f(1, "s"); r
no "i64 does not contain string" let f = |x: 'a, y: 'a| -> 'a y; let r: i64 = f(1, "s"); r
no "Number does not contain string" let f = 'a: Number |x: 'a, y: 'a| -> 'a y; f(1, "s")
ok let f = 'a: Number |x: 'a, y: 'a| -> 'a y; f(1, 1.5)
no "cannot compute with [i64, f64]" let f = 'a: Number |x: 'a, y: 'a| -> 'a x + y; f(1, 1.5)
ok let f = 'a: Number |x: 'a, y: 'a| -> 'a x + y; f(1.0, cast<f64>(1)$)
ok let f = |x: 'a, y: 'a, k: fn(v: 'a) -> i64| -> 'a y; f(1, "s", |v| 0)
no "i64 does not contain string" let f = |x: 'a, y: 'a, k: fn(v: 'a) -> i64| -> 'a y; f(1, "s", |v: i64| 0)
ok let f = |x: 'a, y: 'a, k: fn(v: 'a) -> i64| -> 'a y; f(1, "s", |v: [i64, string]| 0)
ok let f = |g: fn(x: 'a) -> i64, h: fn(x: 'a) -> i64| 0; f(|x: i64| 0, |x: null| 0)
```

A value a type-directed operation makes (`str::parse`, `json::read`, a
cast) takes its type from what is asked of it: a parameter's written
type, an annotation, an operator, the annotation its call's result goes
to. It takes nothing from a sibling argument, so passed for a bare
variable it is refused, "the type 'b must be known here, an operation
reads it to know what to make: annotate where its result goes": annotate
the read, or the call's result.

```graphix,cases
ok opt::or_default(str::parse("5")$, 0)
no "annotate where its result goes" let f = |x: 'a, y: 'a| -> 'a y; f(1, str::parse("1"))
no "annotate where its result goes" let f = |x: 'a, y: 'a, k: fn(v: 'a) -> i64| -> 'a y; f(str::parse("5"), 1, |v| 0)
ok let f = |x: 'a, y: 'a| -> 'a y; let p: [i64, Error<`ParseError(string)>] = str::parse("1"); f(p, 1)
ok let f = |x: 'a, y: 'a| -> 'a y; let r: [i64, Error<`ParseError(string)>] = f(str::parse("1"), 1); r
```

## Unions in a signature

A signature may write a union of its type variables (`|x: ['a, 'b], y:
'a, z: 'b|`, `[Array<'a>, Array<'b>]`, two function types). A value
there waits for what decides the members, the call's other arguments
wherever they stand or what is asked of the result, and goes to the
member that holds it. A decided member takes no other type, so a
variable in such a union does not widen at it: `f(true, 1, "s")` is
refused, "goes to no member". A value two members could still take when
the check ends is refused, "could go to more than one member of the
union .., and nothing has decided which": write one variable in both
members. In an impl, `self` is the impl's target, so `[self, 'a]` in
`impl T for i64` is `[i64, 'a]`.

```graphix,cases
ok let f = |x: ['a, 'b], y: 'a, z: 'b| -> (Array<'a>, Array<'b>) ([y], [z]); f("t", 1, "s")
no "a decided member takes no other type" let f = |x: ['a, 'b], y: 'a, z: 'b| -> (Array<'a>, Array<'b>) ([y], [z]); f(true, 1, "s")
ok type P<'p, 'q> = [Array<'p>, Array<'q>]; let f = |x: P<'a, 'b>, y: 'a, z: 'b| x; f([1], 1, "s")
ok type P<'p, 'q> = [Array<'p>, Array<'q>]; let f = |y: 'a, z: 'b, x: P<'a, 'b>| x; f(1, "s", [1])
no "does not contain Array<bool>" type P<'p, 'q> = [Array<'p>, Array<'q>]; let f = |x: P<'a, 'b>, y: 'a, z: 'b| x; f([true], 1, "s")
ok type P<'p, 'q> = [Array<'p>, Array<'q>]; let f = |x: P<'a, 'b>| -> (Array<'a>, Array<'b>) ([], []); let r: (Array<string>, Array<i64>) = f([1]); r
no "nothing has decided which" type F2<'p, 'q> = [fn(q: 'p) -> i64, fn(q: 'q) -> i64]; let mk = |z| {fld: |x: F2<'a, 'b>| x}; let s = mk(0); let r = s.fld(|q: string| 1); 0
ok type F2<'p, 'q> = [fn(q: 'p) -> i64, fn(q: 'q) -> i64]; let mk = |z| {fld: |x: F2<'a, 'a>| x}; let s = mk(0); let r = s.fld(|q: string| 1); 0
ok |x: ['a, null]| 1
ok trait T { val m: fn(self, x: [self, 'a]) -> i64 }; impl T for i64 { let m = |s, x| 0 }; T::m(1, 2)
ok trait T { val m: fn(self, x: [i64, string]) -> i64 }; impl T for i64 { let m = |s, x: [self, 'c]| 0 }; T::m(1, "q")
```

A variable beside written members takes of an argument's primitive types
only those the written members do not: `x: ['a, null]` over a `[i64, null,
Array<i64>]` makes `'a` `[i64, Array<i64>]`, so a select over a `-> 'a`
result writes no null arm.

```graphix,cases
ok let f = |x: ['a, null]| -> 'a x$; let v: [Array<i64>, i64, null] = 1; select f(v) { i64 as i => i, Array<i64> as a => 2 }
no "pattern null will never match" let f = |x: ['a, null]| -> 'a x$; let v: [Array<i64>, i64, null] = 1; select f(v) { null as _ => 0, i64 as i => i, Array<i64> as a => 2 }
ok let f = 'a: Number |x: [['a, null], Array<i64>]| x; let v: [i64, null, Array<i64>] = 1; f(v)
```

A variable bounded by a union (`'a: [Array<i64>, Array<string>]`, `'a:
[Array<i64>, 'b]`), or a parameter or a `let` written as one, takes a
value still undecided (`[z]` over a `let z = never()`, `[]`): the value
waits, goes to the member that holds what it turns out to be, and is
refused if none does. An untyped lambda at a union
of function types is refused where no one member takes it as written
(`|x| x` at `[fn(x: i64) -> i64, fn(x: string) -> string]`): write its
parameter's type.

```graphix,cases
ok let f = 'a: [Array<i64>, Array<string>] |v: 'a| v; let z = never(); let r = f([z]); z <- 1; r
no "does not contain Array<bool>" let f = 'a: [Array<i64>, Array<string>] |v: 'a| v; let z = never(); let r = f([z]); z <- true; r
ok let f = 'a: [Array<i64>, Array<string>] |v: 'a| v; f([])
ok let f = 'a: [Array<i64>, Array<string>] |v: 'a| v; let e: Array<string> = []; f(e)
no "i64 does not contain f64" let f = 'a: [(i64, i64), (string, string)] |v: 'a| v; let z = never(); let r = f((z, 1)); z <- 2.5; r
ok let f = 'a: [Array<i64>, 'b] |v: 'a, w: 'b| v; let z = never(); let r = f([z], ["s"]); z <- "s"; r
ok let f = 'a: [Array<i64>, 'b] |w: 'b, v: 'a| v; f(["s"], ["x"])
ok let f = |x: [Array<i64>, Array<string>]| x; let z = never(); let r = f([z]); z <- "t"; r
ok let x: [Array<i64>, Array<string>] = []; x
ok let f = |k: [fn(x: i64) -> i64, fn(x: string) -> i64]| 0; f(|x| 0)
no "does not contain fn(x: _) -> _" let f = |k: [fn(x: i64) -> i64, fn(x: string) -> string]| k; f(|x| x)
ok let f = |k: [fn(x: i64) -> i64, fn(x: string) -> string]| k; f(|x: string| x)
```

## Unannotated parameters a select takes apart

An unannotated parameter a select takes apart is typed by its arms
(`select.md`), and the function is generic over what they leave open.
Slice patterns (`[h, t..]`, two arms that bind elements, a recursive
`len(t)`) make it one array over one element type, taken as `|x:
Array<'e>|` takes it. A part several arms bind (`([a, c, ..], b)`,
`([d], b)`, `([], b)`; `(1, b)`, `(2, b)`, `(_, b)`) is one type in
every arm, whichever arm a value takes: where arms use it as two (`b +
1` in one, `str::len(b)` in another) no argument fits. An arm that binds
a part whole beside arms that take it apart (`(1, b)` beside `` (2,
`A(x)) `` and `` (3, `B(y)) ``) leaves that part the union of their
shapes, which a wrapper's untyped parameter passes on.

```graphix,cases
ok let f = |x| select x { [h, t..] => h, [] => 0 }; (f([4, 5]), f(["a"]))
ok let f = |x: Array<'e>| select x { [h, t..] => h, [] => 0 }; f([4, 5])
ok let rec len = |x| select x { [h, t..] => 1 + len(t), [] => 0 }; len([1, 2])
ok let rec len = |x| select x { [<h, t..>] => 1 + len(t), [<>] => 0 }; len([<1, 2>])
ok let k = |x| select x { [h, t..] => h, [] => 0 }; let f = |g: fn(x: Array<i64>) -> i64, y| g(y); f(k, [1, 2])
ok let f = |s| select s { ([a, c, ..], b) => (a, b), ([d], b) => (b, d), ([], b) => (b, b) }; f(([1, 2], "q"))
ok let f = |s| select s { (1, b) => (b, 1), (2, b) => (b, 2), (3, b) => (b, 3), (_, b) => (b, 0) }; f((1, "q"))
no "string does not contain i64" let f = |s| select s { (1, b) => 0, (2, b) => b + 1, (3, b) => str::len(b), _ => 0 }; f((2, 5))
no "i64 does not contain string" let f = |s| select s { (1, b) => 0, (2, b) => b + 1, (3, b) => str::len(b), _ => 0 }; f((3, "x"))
ok let f = |s| select s { (1, b) => 0, (2, `A(x)) => x, (3, `B(y)) => str::len(y), (_, b) => 0 }; (f((1, `A(5))), f((3, `B("x"))))
ok let f = |s| select s { (1, b) => 0, (2, `A(x)) => x, (3, `B(y)) => str::len(y), (_, b) => 0 }; let h = |a| f((2, `A(a))); h(5)
ok let f = |s| select s { (1, b) => 0, (2, (x, y)) => x, (3, (x, y, z)) => x, (_, b) => 0 }; let g = |u| f((2, u)); g((5, 6))
```

## Bounds

A declared variable is rigid in its body: the body asks no more of it
than its bound says. A comparison, an ordering, a sort or a map key over
a variable bounded by less than `Discernible` or `Ordered` is refused,
naming the bound to add ("add Discernible to the bound of 'a"). A call
or arithmetic that needs `Number` is refused "Number does not contain
'c": write `'c: Number`. Nothing is added to the signature quietly. A
bound that implies what the body needs is enough (`'c: Int` for
`Number`, `Number` for an ordering), and arithmetic over a `Number`
variable needs no `Singleton` written. A variable written bare (`|y:
'c|`, no header) takes the bound its body needs, a class or a union
(`Number`, `[i64, null]`), and a comparison in such a body, or in an
untyped one, is judged at each call. No variable is one written type: a
body that passes `y: 'c` where an `i64` is taken is refused, "i64 does
not contain 'c": write `y: i64`, or leave `y` untyped.

```graphix,cases
no "Number does not contain 'c" let f = 'a: Number |x: 'a| -> 'a x; 'c: Any |y: 'c| -> 'c f(y)
no "Number does not contain 'c" let f = 'a: Number |x: 'a| -> 'a x; 'c: [i64, string] |y: 'c| -> 'c f(y)
ok let f = 'a: Number |x: 'a| -> 'a x; 'c: Number |y: 'c| -> 'c f(y)
ok let f = 'a: Number |x: 'a| -> 'a x; 'c: Int |y: 'c| -> 'c f(y)
ok let f = 'a: Number |x: 'a| -> 'a x; |y: 'c| -> 'c f(y)
ok let g = |x: [i64, null]| x; let f = |y: 'c| g(y); f(1)
no "i64 does not contain 'c" let g = |x: i64| x; let f = |y: 'c| g(y); f(1)
ok let g = |x: i64| x; let f = |y| g(y); f(1)
no "Number does not contain 'a" 'a: Any |x: 'a, y: 'a| -> 'a x + y
ok 'a: Number |x: 'a, y: 'a| -> 'a x + y
ok 'a: Number |x: 'a, y: 'a| x < y
no "add Ordered to the bound of 'a" 'a: Any |x: 'a, y: 'a| x < y
no "add Discernible to the bound of 'a" 'a: Any |x: 'a, y: 'a| x == y
no "add Ordered to the bound of 'a" 'a: Any |x: 'a| array::sort([x])
ok let f = |x: 'c, y: 'c| x == y; f(1, 2)
no "'c must be Ordered" let f = |x: 'c, y: 'c| x < y; let g = |q: i64| q; f(g, g)
ok let f = |x, y| x == y; (f(1, 2), f("a", "b"))
no "cannot compare i64 with string" let f = |x, y| x == y; f(1, "s")
no "must be Ordered here" let f = |x, y| x < y; let g = |q: i64| q; f(g, g)
```

A wrapper's bare variable passed for a callee's bounded one takes the
callee's bound, and a call of the wrapper is held to it; a declared one
declares the bound itself.

```graphix,cases
ok let g = 'b: Array<['c, null]> |x: 'b, s: 'c| -> 'b x; let f = |v: 'a, w: Array<'a>| g(w, v); (f(1, [2]), f("a", ["b"]))
ok let g = 'b: [i64, null] |x: 'b| -> 'b x; let f = |w: 'a| g(w); f(1)
no "[i64, null] does not contain string" let g = 'b: [i64, null] |x: 'b| -> 'b x; let f = |w: 'a| g(w); f("s")
no "[i64, null] does not contain 'a" let g = 'b: Array<[i64, null]> |x: 'b| -> 'b x; let f = 'a: Any |w: Array<'a>| g(w); f([2])
ok let g = 'b: Array<[i64, null]> |x: 'b| -> 'b x; let f = 'a: [i64, null] |w: Array<'a>| g(w); f([2])
ok let g = 'b: (['c, null], ['c, i64]) |s: 'c, x: 'b| -> 'b x; let f = |u, w: (['a, null], ['a, null])| g(u, w); f(1, (2, 2))
ok let f = 'a1: ['a3, Array<'b>], 'a3: ['a4, Array<'a4>], 'a4: ['a5, Array<'b>], 'a5: Array<i64>, 'b: Number |x: ('a1, 'a3, 'a4, 'a5)| x; f(([1], [], [], [1]))
```

A bound holds through a chain of bounds. Under `'a0: ['a1, null], 'a1:
['a2, null]` the argument that decides `'a0` is a value `'a1` must hold,
and so one `'a2` must hold: `'a2` is the union of it and what `x2: 'a2`
is passed. A bound on the inner variable refuses what reaches it through
the chain.

```graphix,cases
ok let f = 'a0: ['a1, null], 'a1: ['a2, null] |x0: 'a0, x1: 'a1, x2: 'a2| x2; let v: ([i64, null], i64) = (1, 1); f(v, (1, 1), (null, 1))
no "string does not contain ([i64, null], i64)" let f = 'a0: ['a1, null], 'a1: ['a2, null] |x0: 'a0, x1: 'a1, x2: 'a2| -> 'a2 x2; let v: ([i64, null], i64) = (1, 1); let r: string = f(v, (1, 1), "s"); r
ok let f = 'a1: ['a2, duration] |x0: 'a1, x2: Array<'a2>| x2; let w0: [null, string] = null; f(w0, ["s"])
no "i64 does not contain [null, string]" let f = 'a1: ['a2, duration] |x0: 'a1, x2: Array<'a2>| x2; let w0: [null, string] = null; let r: Array<i64> = f(w0, [1]); r
ok let f = 'x: Array<['a, null]> |y: 'x| y; let g = |u| f([u, [u]]); g(1)
no "Number does not contain string" let f = 'a: Array<'b>, 'b: Number |x: 'a| x; f(["s"])
```

A variable a written callback takes is held to that callback: with `|q:
string| 0` passed for `fn(q: 'c) -> i64`, in the parameter or in a bound
(`'a: fn(q: 'c) -> i64`), a wider `c: 'c` is refused. Annotate the
callback's parameter as wide as `c`, or leave it untyped.

```graphix,cases
no "string does not contain [i64, string]" let f = 'a: fn(q: 'c) -> i64 |v: 'a, c: 'c| c; let n: [i64, string] = 1; f(|q: string| 0, n)
ok let f = 'a: fn(q: 'c) -> i64 |v: 'a, c: 'c| c; let n: [i64, string] = 1; f(|q: [i64, string]| 0, n)
ok let f = |v: fn(q: 'c) -> i64, c: 'c| c; let n: [i64, string] = 1; f(|q| 0, n)
ok let f = 'a: Array<'c> |v: 'a, c: 'c| c; let n: [i64, string] = 1; f(["s"], n)
```

## Quantified function types

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
A trait is no type inside a typedef either: bound a parameter by it.

```graphix,cases
no "type variables of its own" |f: fn<'a: Number>(x: 'a) -> 'a| 1
no "type variables of its own" let g: fn<'a: Number>(x: 'a) -> 'a = |x| x; g(1)
no "type variables of its own" let f = |g: fn(c: Collection) -> i64| 1; 1
ok let g = 'a: Number |f: fn(x: 'a) -> 'a, x: 'a| f(x); g(|x| x, 1)
ok |c: Collection| 1
no "trait Sh used as a type" type T = Array<Sh>; trait Sh { val m: fn(self) -> i64 }; 0
no "trait Sh used as a type" trait Sh { val m: fn(self) -> i64 }; type T = Array<Sh>; 0
ok type T<'a: Sh> = Array<'a>; trait Sh { val m: fn(self) -> i64 }; impl Sh for i64 { let m = |s| 1 }; let v: T<i64> = [1]; v
```

## What stays generic

A function stays generic only where a binding names it: a `let` of a
lambda (or of a data literal holding lambdas, below), a `val`, a trait
method, and a `let` of a generic name or of a field or tuple read of
one (`let j = s.g`, `let j = t.0`), or of a block whose value is one of
these (`let f = { let n = 1; |x| x }`). Each read of it is an instance
of its own. Anything else is one instance, whose variables are what all
its uses pass between them: a function a call returns (`let h = mk(0)`
over `let mk = |z| |x| x`), an element bound by a `let` (`let g =
fs[0]$`), a field of a call's result, a reference (`let r = &f`, then
`(*r)(1)`) and a binding something writes (`j <- ..`, `&mut j`). A
second use at another type widens the variable: `(h(1), h("s"))` checks,
and `h`'s parameter is an `[i64, string]`. Only a body that takes one
type refuses it: arithmetic, "cannot compute with [i64, f64]: both
operands must be one numeric type". Each call of `mk` is an instance of
its own: `let a = mk(0); let b = mk(0)` takes `a(1)` and `b(1.5)`.
Whatever holds such an instance beside a generic value is that instance
too: with `let j = s.f; j <- ..`, a `{s with f: j}`, a `[s, {f: j}]` or
a `select b { true => s, false => {f: j} }` is `j`'s one type in every
element and arm, while `s` read by its name stays generic. A select
arm's name over members that hold lambdas is those lambdas: `select t {
(r, n) => r.f(v) }` over `({f: |x| x}, 1)` and `({f: |y| y}, "s")` calls
either one. The result of `|c, a, b| select c { true => a, false => b }`
over two functions is one function to call, and the argument must fit
both.

```graphix,cases
ok let s = {g: |x| x + x}; let j = s.g; (j(1), j(1.5))
ok let t = (|x| x + x, 0); let j = t.0; (j(1), j(1.5))
ok let f = { let n = 1; |x| x + x }; (f(1), f(1.5))
ok let fs = array::map([1], |r| |x| x); let g = fs[0]$; (g(1), g("s"))
no "cannot compute with [i64, f64]" let fs = array::map([1], |r| |x| x + x); let g = fs[0]$; (g(1), g(1.5))
no "cannot compute with [i64, f64]" let mk = |z| |x| x + x; let h = mk(0); (h(1), h(1.5))
ok let mk = |z| |x| x + x; let a = mk(0); let b = mk(0); (a(1), b(1.5))
no "cannot compute with [i64, f64]" let mk = |z| {f: |x| x + x}; let j = mk(0).f; (j(1), j(1.5))
ok let f = |x| x; let r = &f; ((*r)(1), (*r)("s"))
no "cannot compute with [i64, f64]" let f = |x| x + x; let r = &f; ((*r)(1), (*r)(1.5))
no "cannot compute with [i64, f64]" let f = |x| x + x; let r = &mut f; (f(1), f(1.5))
no "cannot compute with [i64, f64]" let s = {g: |x| x + x}; let j = s.g; j <- s.g; (j(1), j(1.5))
no "i64 does not contain string" let f = |x| x; f <- |y: i64| y; f("s")
ok let f = { let t = sys::net::publish; t }; f(never(), 10)
ok let f = sys::net::publish; f(never(), 10)
ok let s = {f: |x| x + x}; let j = s.f; j <- s.f; let t = {s with f: j}; (t.f(1), s.f(1.5), s.f(2))
no "cannot compute with [i64, f64]" let s = {f: |x| x + x}; let j = s.f; j <- s.f; let t = {s with f: j}; (t.f(1), t.f(1.5))
no "cannot compute with [i64, f64]" let s = {f: |x| x + x}; let j = s.f; j <- s.f; let a = [s, {f: j}]; (a[0]$.f(1), a[1]$.f(1.5))
no "cannot compute with [i64, f64]" let s = {f: |x| x + x}; let j = s.f; j <- s.f; let p = |b| select b { true => s, false => {f: j} }; (p(true).f(1.5), p(false).f(1))
ok let s = {f: |x| x}; let j = s.f; let p = |b| select b { true => s, false => {f: j} }; (p(true).f("s"), p(false).f(1))
ok let k = |v| { let t = select v > 0 { true => ({f: |x| x}, 1), false => ({f: |y| y}, "s") }; select t { (r, n) => r.f(v) } }; (k(2), k(-2))
ok let pick = |c, a, b| select c { true => a, false => b }; let f = |x: i64| x; let g = |x: i64| x + 1; pick(false, f, g)(1)
no "string does not contain i64" let pick = |c, a, b| select c { true => a, false => b }; pick(false, |x: i64| x, |x: string| x)(1)
ok let pick = |c, a, b| select c { true => a, false => b }; pick(false, |x: i64| x, |x: [i64, string]| 0)(1)
```

A read of a declared variable's value (a call, `.x`, `.0`) takes its
declaration's bound: the variable is rigid in the body, so annotating a
binding does not help. Declare `'a: fn(x: i64) -> i64`, `'a: {x: i64}`
or `'a: (i64, i64)`, or write the type in its place.

```graphix,cases
no "does not contain 'a" let f = |a: 'a| a(1); f(|x| x + 1)
no "does not contain 'a" let f = 'a: Number |a: 'a| a(1); f(1)
ok let f = 'a: fn(x: i64) -> i64 |a: 'a| a(1); f(|x| x + 1)
no "Any has no field x" let f = 'a: Any |a: 'a| a.x; f({x: 1})
ok let f = 'a: {x: i64} |a: 'a| a.x; f({x: 1})
ok let f = 'a: (i64, i64) |a: 'a| a.0; f((1, 2))
ok let f = 'a: {x: i64}, 'b: {x: i64} |c: bool, a: 'a, b: 'b| (select c { true => a, false => b }).x; f(true, {x: 1}, {x: 2})
no "has no field x" 'a: {x: i64}, 'b: {y: i64} |c: bool, a: 'a, b: 'b| (select c { true => a, false => b }).x
```

A message prints one instance's function type without the `fn<'a: ..>`
header: a declared variable where it occurs with its state (`'a: unbound
within Number` before a use), an inferred one as the type its uses gave
it. Two instances are two variables, and in one message the second
prints numbered apart (`'a1`). The shell's `-:` line prints the header
for a generic binding alone; one instance nothing has called stands at
its variable's bound (`fn(x: Number) -> Number`), `_` with none.

```graphix,cases
no "does not contain fn(x: 'a: unbound within Number) -> 'a: unbound within Number" let mk = |z| 'a: Number |x: 'a| -> 'a x; let h = mk(0); let n: i64 = h; n
no "does not contain fn(x: string) -> string" let mk = |z| |x| x; let h = mk(0); let r = h("s"); let n: i64 = h; n
no "does not contain fn(x: [i64, string]) -> [i64, string]" let mk = |z| |x| x; let h = mk(0); let r = (h(1), h("s")); let n: i64 = h; n
no "-> 'a: unbound within Number, fn(x: 'a1: unbound within Number) -> 'a1: unbound within Number)" let mk = |z: i64| 'a: Number |x: 'a| -> 'a x; let h = mk(0); let k = mk(1); let n: i64 = (h, k); n
```

## Functions in data

A `let` nothing writes that holds lambdas in a struct, tuple, array or
other data literal is as generic as a `let` of a lambda: `let s = {f:
|x| x}` serves `s.f(1)` and `s.f("a")`, and an annotation of one use
(`let t: {f: fn(x: i64) -> i64} = s`) decides nothing of the others. A
`seq` step or a `seqq` request reads `s` as generically as a `let` of
it, unless the seq writes `s`. A `let` nothing writes or annotates reads
`s` once in its value, wherever it reads it: in `let t = {a: s, b:
k(s)}`, a `k` taking `{f: fn(x: i64) -> i64}` makes `t.a.f` take `i64`
too, and so does `b: seqq 1 { k(s) }`.

```graphix,cases
ok let s = {f: |x| x}; let t: {f: fn(x: i64) -> i64} = s; (t.f(1), s.f("a"))
ok let t = 1; let s = {f: |x| x}; seq t { (s.f(1), s.f("s")) }
ok let t = 1; let s = {f: |x| x}; seqq t { (s.f(1), s.f("s")) }
ok let s = {f: |x| x}; let h = {g: |k| seqq k { (s.f(1), s.f("s")) }}; h.g(1)
no "cannot compute with [i64, f64]" let t = 1; let s = {f: |x| x + x}; seq t { s <- {f: |x| x + x}; (s.f(1), s.f(1.5)) }
no "i64 does not contain string" let k = |p: {f: fn(x: i64) -> i64}| 0; let s = {f: |x| x}; let t = {a: s, b: k(s)}; t.a.f("x")
no "is used as i64 and as string, and no value is both" let k = |p: {f: fn(x: i64) -> i64}| 0; let s = {f: |x| x}; let t = {a: s, b: seqq 1 { k(s) }}; t.a.f("x")
```

Lambdas that meet in one value (an array's elements, a select's arms, a
name an or-pattern's alternatives share, a field read over two struct
shapes, an optional callback beside a lambda) are one function to call:
the argument must fit every one's parameter, so `|x| x` beside `|y| y +
1` takes `i64`. They meet uncalled whatever their types. Each keeps its
own labeled default: a call that leaves `#d` out passes each its own,
and one that passes `#d` must suit every body. A function beside itself,
or beside the same lambda written again, is that function.

```graphix,cases
ok let g = |cb: [fn(x: i64) -> i64, null], b: bool| { let h = select b { true => cb, false => |x| x + 1 }; select h { null as _ => 0, f => f(2) } }; g(null, false)
ok let b = true; let t = select b { true => `A(|x| x, 1), false => `B(|y| y + 1, "s") }; select t { `A(r, _) | `B(r, _) => r(2) }
no "i64 does not contain string" let b = true; let t = select b { true => `A(|x| x, 1), false => `B(|y| y + 1, "s") }; select t { `A(r, _) | `B(r, _) => r("s") }
no "string does not contain i64" let b = true; let t = select b { true => `A(|x: string| x, 1), false => `B(|y| y + 1, "s") }; select t { `A(r, _) | `B(r, _) => r(2) }
ok let b = 0; let t = select b { 0 => {f: |x| x, a: 1}, 1 => {f: |y| y + 1, b: 1}, _ => {f: |z| "[z]!", c: 1} }; t.f(1)
no "i64 does not contain string" let b = 0; let t = select b { 0 => {f: |x| x, a: 1}, 1 => {f: |y| y + 1, b: 1}, _ => {f: |z| "[z]!", c: 1} }; t.f("s")
ok let t = [|x| x, |w| w, |y: i64| y + 1, |z: string| "[z]!"]; array::len(t)
no "string does not contain i64" let t = [|x| x, |w| w, |y: i64| y + 1, |z: string| "[z]!"]; t[0]$(1)
ok let fs = [|#d = 1, x: i64| d + x, |#d = "q", x: i64| x]; fs[0]$(7)
no "i64 does not contain string" let fs = [|#d = 1, x: i64| d + x, |#d = "q", x: i64| x]; fs[0]$(#d: "s", 7)
ok type P<'p, 'q> = [Array<'p>, Array<'q>]; let f = |y: 'a, x: P<'a, 'b>| x; let b = true; let g = select b { true => f, false => f }; g(1, ["s"])
ok type P<'p, 'q> = [Array<'p>, Array<'q>]; let f = |y: 'a, x: P<'a, 'b>| x; let b = true; let g = select b { true => f, false => |y: 'a, x: P<'a, 'b>| x }; g(1, ["s"])
```

## Writes to a generic binding

A write the compiler meets only after the binding was made generic (a
module's `val` written from outside it, a REPL `let` written by a later
input) must be as general as the binding, and a `&mut` of it is refused:
to write it at one type, write that type on its `let` or `val`. A sample
is no binding: `m::c <- t ~ {f: |x| x}` holds one instance and is
refused, where the direct `m::c <- {f: |x| x}` names its lambda.

```graphix,cases
no "a generic binding, so what is written to it must be as general" let st = mod m dynamic { sig { val j: fn(x: 'a) -> 'a }; source "let j = |x| x" }; m::j <- |x: i64| x; st
ok let st = mod m dynamic { sig { val j: fn(x: 'a) -> 'a }; source "let j = |x| x" }; m::j <- |x| x; st
no "a writable reference to it would let a write narrow it" let st = mod m dynamic { sig { val j: fn(x: 'a) -> 'a }; source "let j = |x| x" }; let r = &mut m::j; st
ok let st = mod m dynamic { sig { val j: fn(x: i64) -> i64 }; source "let j = |x| x" }; m::j <- |x: i64| x + 1; st
no "it holds a type variable of the code around the write" let st = mod m dynamic { sig { val c: {f: fn(x: 'a) -> 'a} }; source "let c = {f: |x| x}" }; let t = 1; m::c <- t ~ {f: |x| x}; st
ok let st = mod m dynamic { sig { val c: {f: fn(x: 'a) -> 'a} }; source "let c = {f: |x| x}" }; m::c <- {f: |x| x}; st
```

## A generic function at a written function type

A generic function held at a written function type (an annotated `let`,
a callback parameter, a field, a write) is one instance there: its
variable is the union of what the type's positions write for it, and a
bound refuses a position outside it. A result of the variable is then
declared as wide as its parameters (`fn(b: i64, k: string) -> [i64,
string]` for `|b: 'y, k: 'y| -> 'y k`), and a callback of it as wide as
the values.

```graphix,cases
ok let f = |b: 'y, k: 'y| 0; let g: fn(b: i64, k: Any) -> i64 = f; g(1, 2)
ok let f = |k: 'y, b: 'y| 0; let app = |h: fn(k: Any, b: i64) -> i64| h(2, 1); app(f)
no "i64 does not contain [i64, null]" let f = |b: 'y, k: 'y| -> 'y k; let g: fn(b: i64, k: [i64, null]) -> i64 = f; g(1, null)
ok let f = |b: 'y, k: 'y| -> 'y k; let g: fn(b: i64, k: string) -> [i64, string] = f; g(1, "s")
ok let f = |b: 'y, k: 'y| -> 'y k; let s: {g: fn(b: i64, k: string) -> [i64, string]} = {g: f}; s.g(1, "s")
no "string does not contain i64" let f = |b: 'y, k: 'y| -> 'y k; let g: fn(b: i64, k: string) -> string = never(); g <- f; g(1, "s")
no "i64 does not contain string" let f = |b: 'y, k: 'y, c: fn(v: 'y) -> i64| c(k); let g: fn(b: i64, k: string, c: fn(v: i64) -> i64) -> i64 = f; g(1, "s", |v| 7)
ok let f = |b: 'y, k: 'y, c: fn(v: 'y) -> i64| c(k); let g: fn(b: Any, k: i64, c: fn(v: Any) -> i64) -> i64 = f; g(1, 2, |v| 7)
ok type L<'a> = [`Nil, `Cons('a, L<'a>)]; let f = |k: 'y, l: L<'y>| -> 'y k; let g: fn(k: [i64, null], l: L<i64>) -> [i64, null] = f; g(null, `Nil)
ok let g = 'b: [i64, null] |x: 'b| { let h: fn(s: i64, v: 'b) -> i64 = |s: 'a, v: ['a, null]| 0; h(1, x) }; g(null)
no "Number does not contain string" let f = 'a: Number |x: 'a| -> 'a x; let g: fn(x: string) -> string = f; g("s")
```

## Labeled defaults

A lambda's labeled default is its own: a call that leaves the label out
passes it, and one that passes the label must suit the body. A default
may read an enclosing function's parameter (`|z| { let g = |#d = z, x|
(d, x); g }`).

```graphix,cases
ok let f = |#d = 1, x: i64| cast<string>(d)$; (f(7), f(#d: "s", 7))
no "i64 does not contain string" let f = |#d = 1, x: i64| d + x; f(#d: "s", 7)
ok let mk = |z| { let g = |#d = z, x| (d, x); g }; let h = mk(1); (h(2), h(#d: "s", 3))
ok |z: 'a| { let g = |#d: 'a = z, y: i64| d; g(1) }
```

A lambda with a default, returned at a declared type whose label is the
function's own variable (`-> {cb: fn(?#d: 'z, x: i64) -> string}` over
`#d = 1`, written out, through a typedef, one function type deeper or
behind an abstract type's constructor), is refused: a call picks `'z`,
and the default need not be of it. Write the label's type (`?#d: i64`,
`W<i64>`, `C<i64>`), give the default the variable's type (`|z: 'z| ..
#d = z`), or drop the return type. A `let w: W<'p> = ..` annotation
takes the default as it is, and so does a call's own variable: a
callback returning such a lambda fits `fn(j: i64) -> fn(?#d: 'z, x: i64)
-> string`.

```graphix,cases
no "write the default's type in the declared type" type W<'z> = {cb: fn(?#d: 'z, x: i64) -> string}; let mk = |j: i64| -> W<'z> {cb: |#d = 1, x: i64| cast<string>(d)$}; mk(0).cb(7)
ok type W<'z> = {cb: fn(?#d: 'z, x: i64) -> string}; let mk = |j: i64| -> W<i64> {cb: |#d = 1, x: i64| cast<string>(d)$}; mk(0).cb(7)
ok type W<'z> = {cb: fn(?#d: 'z, x: i64) -> string}; let mk = |z: 'z| -> W<'z> {cb: |#d = z, x: i64| cast<string>(d)$}; mk("s").cb(7)
no "write the default's type in the declared type" let mk = |j: i64| -> {cb: fn(?#d: 'z, x: i64) -> string} {cb: |#d = 1, x: i64| cast<string>(d)$}; mk(0).cb(7)
ok let mk = |j: i64| {cb: |#d = 1, x: i64| cast<string>(d)$}; mk(0).cb(#d: "s", 7)
ok type W<'z> = {cb: fn(?#d: 'z, x: i64) -> string}; let w: W<'p> = {cb: |#d = 1, x: i64| cast<string>(d)$}; w.cb(#d: "s", 7)
no "write the default's type in the declared type" type W<'z> = {cb: fn(?#d: 'z, x: i64) -> string}; let mk = |j: i64| -> fn(k: i64) -> W<'z> |k: i64| {cb: |#d = 1, x: i64| cast<string>(d)$}; mk(0)(0).cb(7)
no "C<'z> does not contain" type C<'z> = Abstract<fn(?#d: 'z, x: i64) -> string>; let mk = |j: i64| -> C<'z> C(|#d = 1, x: i64| cast<string>(d)$); mk(0).0(7)
ok type C<'z> = Abstract<fn(?#d: 'z, x: i64) -> string>; let mk = |j: i64| -> C<i64> C(|#d = 1, x: i64| cast<string>(d)$); mk(0).0(7)
ok type C<'z> = Abstract<fn(?#d: 'z, x: i64) -> string>; let c = C(|#d = 1, x: i64| cast<string>(d)$); c.0(7)
ok let app = |f: fn(j: i64) -> fn(?#d: 'z, x: i64) -> string| f(0)(7); app(|j: i64| |#d = 1, x: i64| cast<string>(d)$)
```

A binding's written function type holds every default written to it:
`let v: fn(?#d: string, x: i64) -> string` refuses a write of `#d = 1`.
Write the label wide enough for every default (`?#d: [i64, string]`), or
as a variable (`?#d: 'q`), which is the union of them. A write through a
parameter's `&mut fn(?#d: 'a, ..)` may
not decide the function's own `'a`: write the label's type (`?#d: i64`),
or the default's beside it (`?#d: 'a = i64`).

```graphix,cases
ok let v: fn(?#d: 'q, x: i64) -> string = never(); v <- |#d = "q", x: i64| cast<string>(d)$; v(7)
ok let v: fn(?#d: 'q, x: i64) -> string = |#d = "a", x: i64| cast<string>(d)$; let t = 1; v <- t ~ |#d = 1, x: i64| cast<string>(d)$; v(7)
no "write the default's type in the declared type" let v: fn(?#d: string, x: i64) -> string = |#d = "a", x: i64| cast<string>(d)$; let t = 1; v <- t ~ |#d = 1, x: i64| cast<string>(d)$; v(7)
ok let v: fn(?#d: [i64, string], x: i64) -> string = |#d = "a", x: i64| cast<string>(d)$; let t = 1; v <- t ~ |#d = 1, x: i64| cast<string>(d)$; v(7)
ok let j = 0; let o: [fn(?#d: [i64, string], x: i64) -> string, null] = select j { 0 => |#d = 1, x: i64| cast<string>(d)$, _ => null }; let t = 1; o <- t ~ |#d = "q", x: i64| cast<string>(d)$; select o { null as _ => "n", f => f(7) }
no "write the default's type in the declared type" |r: &mut fn(?#d: 'a, x: i64) -> string, t: Any| { *r <- t ~ |#d = 1, x: i64| cast<string>(d)$; null }
ok |r: &mut fn(?#d: i64, x: i64) -> string, t: Any| { *r <- t ~ |#d = 1, x: i64| cast<string>(d)$; null }
ok |r: &mut fn(?#d: 'a = i64, x: i64) -> string, t: Any| { *r <- t ~ |#d = 1, x: i64| cast<string>(d)$; null }
```

A labeled default that raises (`#d = e?`) raises at each call that omits
it, so a written function type that hides the label or makes it optional
must declare what it raises: a typedef or written return type over `|#d
= error(`E)?, x: i64|` is refused until it writes `throws
Error<ErrChain<`E>>`. With no return type written nothing is needed. A
default that raises through the factory's variables (`k? + j?` over
`Result<i64, 'e>` and `Result<i64, 'f>`) needs a written `throws` that
holds every one, `Error<ErrChain<['e, 'f]>>`.

```graphix,cases
no "W does not contain" type W = fn(?#d: i64, x: i64) -> i64; let mk = |k: i64| -> W |#d = error(`E)?, x: i64| d + x; mk(0)(7)
no "does not contain Error<ErrChain<`E>>" let mk = |k: i64| -> fn(x: i64) -> i64 |#d = error(`E)?, x: i64| d + x; mk(0)(7)
ok type W = fn(?#d: i64, x: i64) -> i64 throws Error<ErrChain<`E>>; let mk = |k: i64| -> W |#d = error(`E)?, x: i64| d + x; { catch(e) 0; mk(0)(7) }
ok let mk = |k: i64| |#d = error(`E)?, x: i64| d + x; { catch(e) 0; mk(0)(7) }
ok let mk = |k: Result<i64, 'e>| |#d = k?, x: i64| d + x; { catch(e) 0; mk(error(`E))(7) }
ok let mk = |k: Result<i64, 'e>| -> fn(?#d: i64, x: i64) -> i64 throws Error<ErrChain<'e>> |#d = k?, x: i64| d + x; { catch(e) 0; mk(error(`E))(7) }
no "does not contain 'f" let mk = |k: Result<i64, 'e>, j: Result<i64, 'f>| -> fn(?#d: i64, x: i64) -> i64 throws Error<ErrChain<'e>> |#d = k? + j?, x: i64| d + x; { catch(e) 0; mk(error(`E), 1)(7) }
ok let mk = |k: Result<i64, 'e>, j: Result<i64, 'f>| -> fn(?#d: i64, x: i64) -> i64 throws Error<ErrChain<['e, 'f]>> |#d = k? + j?, x: i64| d + x; { catch(e) 0; mk(error(`E), 1)(7) }
```

A labeled default that holds a function is judged as that function
written at the call is: where the call leaves the outer label out, the
inner function must fit the label's type as the call's arguments left
it. Pass the outer label where it does not.

```graphix,cases
ok let f = |#w: fn(?#d: 'z, x: i64) -> string = |#d = 1, x: i64| cast<string>(d)$, x: i64| w(x); f(7)
no "the default of #w is passed here, where the call leaves #w out" let f = |#w: fn(?#d: 'z, x: i64) -> string = |#d = 1, x: i64| "[d + 1]", y: 'z, x: i64| w(#d: y, x); f("q", 7)
ok let f = |#w: fn(?#d: 'z, x: i64) -> string = |#d = 1, x: i64| "[d + 1]", y: 'z, x: i64| w(#d: y, x); f(#w: |#d = "s", x: i64| cast<string>(d)$, "q", 7)
```

## Abstract comparisons

An abstract's `Eq` implementation bypasses representation checks for equality
only; `Ord` bypasses them for ordering only. Without the relevant implementation,
its representation must satisfy `Discernible` or `Ordered`, respectively.

```graphix,cases
no "implement Ord for it" type T = Abstract<fn(x: i64) -> i64>; let a = T(|x| x); a < a
ok type T = Abstract<fn(x: i64) -> i64>; impl Ord for T { let cmp = |a, b| `Equal }; let a = T(|x| x); a < a
no "implement Ord for it" type T = Abstract<fn(x: i64) -> i64>; impl Eq for T { let eq = |a, b| true }; let a = T(|x| x); a < a
no "implement Eq for it" type T = Abstract<[fn(x: i64) -> i64, fn(s: string) -> string]>; impl Ord for T { let cmp = |a, b| `Equal }; let a = T(|x: i64| x); a == a
ok type T = Abstract<[fn(x: i64) -> i64, fn(s: string) -> string]>; impl Eq for T { let eq = |a, b| true }; let a = T(|x: i64| x); a == a
```

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
