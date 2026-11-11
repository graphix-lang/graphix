# Modules, interfaces and traits

Beyond the import rules in `SKILL.md`.

```graphix,fragment
use package::{AdminError, Target, ceremony::{questions, result}};
use super::{Toast, admin_text, panels::{self, CloseReason}};
use tui::{
  line, span, style,
  block::block,
  input_handler::{Event, input_handler, on_press},
  layout::{child, layout},
  overlay::{self, Layer, layer},
  paragraph::paragraph
};
```

Types first, then values, then one line per module. An import nothing
uses goes: it tells the reader the file does something it does not. A
group names a widget module (`self`) or its function, not both:
`overlay::{self, overlay}` is refused (both are `overlay`), so import
what you would have qualified, `overlay::{Layer, layer, overlay}`. Two
explicit imports of one name are an error where a glob silently lost to
the explicit one. `use Collection::*` (a trait's methods) is the one
glob the book teaches.

Interfaces: `foo.gxi` beside `foo.gx` (directories: `mod.gx`/`mod.gxi`);
`val`, `type`, `mod`, `trait`, `impl T for X;` declarations with `///`
docs; types and mods from the gxi are in scope in the impl. A hidden
type must be `Abstract<..>` or Rust-backed.

A gxi `mod` or `use` the body does not repeat is spliced into the body
after the body's last declaration of every gxi item listed before it,
in whatever order the body declares them (at the top when the body
declares none of them, at the end when one of them is a `val` the body
lacks; a gxi-only type, or a trait with no default body, always goes to
the top). So a gxi-only submodule sees every
binding the gxi lists before it, and the body can use the submodule
only once those are all declared: `let c = l::z` above the body's
`let a` of a `val a` the gxi lists before `mod l` is `l::z not defined`.

Traits: `trait Show { val show: fn(self) -> string; val twice: fn(self)
-> string = |s| "[show(s)] [show(s)]" }`, `impl Show for Counter { let
show = |c| .. }`, `impl<'a: Show> Show for Array<'a> { .. }`, bounds
`'a: Show + Eq`, `|x: Show|` sugar. A trait's methods live under its
name: outside its default bodies and impls, call `Show::show(x)` (or
`use Show::{show}`); a bare `show(x)` is "show not defined". Dispatch is
static on the self
argument's type; a union self compiles to a select, unless a method of
the trait takes `self` beyond its receiver (another parameter, or the
bound of a variable): then select on the value and call per member.
Core `Eq`/`Ord`/`Display` ride the value everywhere (map keys, sort,
operators, printing); only abstract types may implement them outside
core. The io
traits `Read`/`Lines`/`Write`/`Close`/`Seek`/`Socket` describe streams
whose TYPE is their kind (`sys::fs::File`, `sys::tcp::TcpStream`, …).

In an impl, `self` is the impl's target in every type the impl writes (a
method's `let` annotation, its lambda's parameters and result, anything
inside it), as in a default it is the trait's: `|s: self, x|` in `impl T
for i64` is `|s: i64, x|`, and `[self, 'c]` there is `[i64, 'c]`. A
written `self` the trait does not declare there is refused, and so is
a `self` in a `type` definition inside a generic impl's method (the
target holds the impl's variables, which a definition cannot name: give
it a parameter). A bound the trait declares over `self` is over the
target in an impl too: the method writes it with `self` or with the
target, or leaves it out.

```graphix,cases
ok trait T { val m: fn(self, x: i64) -> i64 }; impl T for i64 { let m = |s: self, x| s + x }; T::m(1, 2)
ok trait T { val m: fn(self, x: [i64, string]) -> i64 }; impl T for i64 { let m = |s, x: [self, 'c]| 0 }; T::m(1, "q")
no "string does not contain i64" trait T { val m: fn(self, x: string) -> string }; impl T for i64 { let m = |s, x: self| x }; T::m(1, "q")
ok trait T { val m: fn<'b: [self, null]>(self, x: 'b) -> 'b }; impl T for i64 { let m = 'q: [self, null] |s, x: 'q| -> 'q x }; (T::m(1, 2), T::m(1, null))
ok trait T { val m: fn<'b: [self, null]>(self, x: 'b) -> 'b }; impl<'a> T for Array<'a> { let m = 'q: [Array<'a>, null] |s, x: 'q| -> 'q x }; (T::m([1], [2]), T::m([1], null))
ok trait T { val m: fn<'b: [self, null]>(self, x: 'b) -> i64 }; impl T for i64 { let m = |s, x| select x { null as _ => s, i64 as v => v + s } }; T::m(1, 2)
no "requires 'b: i64, which the signature does not declare" trait T { val m: fn<'b: [self, null]>(self, x: 'b) -> 'b }; impl T for i64 { let m = 'q: self |s, x: 'q| -> 'q x }; T::m(1, 2)
no "T::m takes `self` beyond it" trait T { val m: fn<'b: [self, null]>(self, x: 'b) -> 'b }; impl T for i64 { let m = |s, x| x }; impl T for string { let m = |s, x| x }; let v: [i64, string] = 1; T::m(v, null)
ok trait T { val m: fn<'b: [self, null]>(self, x: 'b) -> 'b }; impl T for i64 { let m = |s, x| x }; impl T for string { let m = |s, x| x }; let v: [i64, string] = 1; select v { i64 as i => T::m(i, null), string as s => T::m(s, null) }
no "type P writes `self`" trait T { val m: fn(self) -> i64 }; impl<'a> T for Array<'a> { let m = |s| { type P = self; let p: P = s; array::len(p) } }; T::m([1])
ok trait T { val m: fn(self) -> i64 }; impl<'a> T for Array<'a> { let m = |s| { type P<'b> = Array<'b>; let p: P<'a> = s; array::len(p) } }; T::m([1])
```

A method, an impl's or a default, is generic where a `let` of its value
is: a lambda, or a read of a generic binding or of a field or tuple of
one (`let put = s.g`), with no annotation on the method's `let`. Any
other value (a call's result, a literal's projection) is one instance,
refused under a variable each call picks. A method that reads a binding
has exactly that binding's type: under a wider declaration, write a
lambda or annotate the method's `let`.

```graphix,cases
ok type C = Abstract<i64>; let s = {g: |s, x| x}; trait Put { val put: fn(self, x: 'a) -> 'a }; impl Put for C { let put = s.g }; (Put::put(C(1), 1), Put::put(C(1), "s"))
ok let s = {g: |s, x| x}; trait Put { val put: fn(self, x: 'a) -> 'a = s.g }; impl Put for i64 {}; (Put::put(1, 1), Put::put(1, "s"))
no "so no call copies its types" let s = {g: |s, x| x}; trait Put { val put: fn(self, x: 'a) -> 'a }; impl Put for i64 { let put: fn(self, x: 'a) -> 'a = s.g }; (Put::put(1, 1), Put::put(1, "s"))
no "so no call copies its types" let mk = |z| |s, x| x; trait Put { val put: fn(self, x: 'a) -> 'a }; impl Put for i64 { let put = mk(0) }; (Put::put(1, 1), Put::put(1, "s"))
ok let mk = |z| |s, x| x + z; trait Put { val put: fn(self, x: i64) -> i64 }; impl Put for i64 { let put = mk(1) }; Put::put(1, 2)
no "has exactly that binding's type" let f = |s: i64| -> i64 s + 1; trait T { val m: fn(self) -> [i64, null] }; impl T for i64 { let m = f }; T::m(1)
ok let f = |s: i64| -> i64 s + 1; trait T { val m: fn(self) -> [i64, null] }; impl T for i64 { let m = |s| f(s) }; T::m(1)
ok let f = |s: i64| -> i64 s + 1; trait T { val m: fn(self) -> [i64, null] }; impl T for i64 { let m: fn(self) -> [i64, null] = f }; T::m(1)
ok let mk = |z| |s: i64| -> i64 s + z; trait T { val m: fn(self) -> [i64, null] }; impl T for i64 { let m = mk(1) }; T::m(1)
```

A trait method's `#[pure]` or `#[sync]` holds for every implementation, even
an uncalled one. Builds reject violating bodies; dynamic modules fail to load.
Callback parameters retain caller-dependent effects, as in ordinary definitions.
`--check` does not elaborate these body assertions.

## A `val`

A `val` in a `.gxi` declares exactly the type its implementation has:
`val x: [i64, null]` over `let x = 3` is refused (`declared [i64, null]
but implemented i64`). When a use needs the wider type, annotate the
binding (`let x: [i64, null] = 3`); every module then sees the declared
type, so the others take the null off with `a::x$`.
A function val declares every bound its implementation needs, of a
variable named only inside another's bound too: ``val f: fn<'a: [i64,
string], 'b: ['a, `X]>(x: 'b) -> 'b`` over ``let f = 'a: [i64, f64], 'b:
['a, `X] |x: 'b| -> 'b x`` is refused (`the implementation requires 'a:
[i64, f64], which the signature does not declare`); a wider bound in
the implementation (`'a: [i64, string, f64]`) is fine. A trait method,
an impl's or a default, is judged the same way:

```graphix,cases
no "requires 'a: [i64, f64], which the signature does not declare" trait T { val m: fn<'a: [i64, string], 'b: Array<'a>>(self, x: 'b) -> 'b }; impl T for i64 { let m = 'p: [i64, f64], 'q: Array<'p> |s, x: 'q| -> 'q x }; T::m(1, ["s"])
ok trait T { val m: fn<'a: [i64, string], 'b: Array<'a>>(self, x: 'b) -> 'b }; impl T for i64 { let m = 'p: [i64, string, f64], 'q: Array<'p> |s, x: 'q| -> 'q x }; T::m(1, ["s"])
```

A `val`'s type variable stands only inside a function type, where each
use picks it: `val v: Array<'a>` is refused (declare it at one type,
`Array<i64>`), and so is `val c: {f: fn(x: 'a) -> string, last: ['a,
null]}`. The implementation must be as general as the val: under `val f:
fn(x: 'a) -> 'a`, `let f = |x| { y <- x; x }` over an outer `let y =
never()` is refused (`let y: Any`, or the val at one type), and so is a
written `let f = |x| x; f <- |x| x` (declare the val at one type). A
trait method alike: one that writes an
outer binding, or whose value is neither a lambda nor a generic
binding's read and has a type its value fixed (`(store, 1).0` over a
writer of an outer binding, `mk(0)`), is refused under a variable each
call picks: write it as a lambda. A val that
something writes (`m::f <- ..`, `&mut m::f`) must fit its
implementation: `val f: fn(x: i64) -> i64` over a generic `let f = |x|
x` takes no write until the `let` is annotated `fn(x: i64) -> i64`. A
val generic in a variable (`val c: {f: fn(x: 'a) -> 'a}`, `val g:
fn<'a: Any>(..)` at its top, `val h: F<'a>` over `type F<'a> = fn(x: 'a)
-> 'a`)
is copied by each use, takes only a write as general as it, and no
`&mut` of it or of a place holding its variable (`&mut m::c.f`; `&mut
m::c.n` over `n: i64` is fine). `let g = m::c; g <- ..` makes `g` one
instance of the val, which the write decides for every use of `g`.
