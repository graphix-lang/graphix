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

A `use` is in force throughout its statement list, for a type name
written above it too: `type Op = {mode: Mode}` or `let m: Mode = ..`
above `use sys::fs::Mode` means the imported `Mode`, never an outer or
prelude type of the name. A statement above the `use` that needs the type
as it compiles (a cast, a type test, a `let` that takes the type apart,
an abstract constructor, a trait as a parameter's type) is refused, and
above a glob that holds for every type name the list neither declares
nor imports, a trait's glob (`use Collection::*`) included. The same
goes for a name a `use` imports from a module declared further down:
until the `mod`, the name is not read, and a function above the `mod`
is refused where its body or signature names it. Write `use` first, then `mod`.

```graphix,cases
no "Mode names what a later `use` of this module imports" let m = cast<Mode>(`Read); use sys::fs::Mode; m
ok use sys::fs::Mode; let m = cast<Mode>(`Read); m
ok type Op = {mode: Mode}; use sys::fs::Mode; let o: Op = {mode: `Read}; o.mode
ok let m: Mode = `Read; use sys::fs::Mode; m
no "Mode may name what a later glob `use` of this module imports" let m = cast<Mode>(`Read); use sys::fs::*; m
no "Show may name what a later glob `use` of this block imports" trait Show { val show: fn(self) -> i64 }; let r = { let f = |x: Show| Show::show(x); use array::*; 0 }; r
ok trait Show { val show: fn(self) -> i64 }; let r = { use array::*; let f = |x: Show| Show::show(x); 0 }; r
```

Interfaces: `foo.gxi` beside `foo.gx` (directories: `mod.gx`/`mod.gxi`);
`val`, `type`, `mod`, `trait`, `impl T for X;` declarations with `///`
docs; types and mods from the gxi are in scope in the impl. A hidden
type must be `Abstract<..>` or Rust-backed.

An interface names by a bare name only what it imports or declares
itself, never what its implementation imports or the code around the
module declares. A dynamic module's `sig` is one, checked with the
program that declares it: a type of the program it names is imported
there, `use super::Foo;`, or written by its path, `super::Foo`, and
declared above the module.

```graphix,cases
no "undefined type Foo in foo" type Foo = {a: i64}; let st = mod foo dynamic { sandbox unrestricted; sig { val f: fn(x: Foo) -> i64 }; source "let f = |x| 1" }; st
ok type Foo = {a: i64}; let st = mod foo dynamic { sandbox unrestricted; sig { use super::Foo; val f: fn(x: Foo) -> i64 }; source "let f = |x: Foo| x.a" }; st
no "Foo names the type a later statement of this module declares" let st = mod foo dynamic { sandbox unrestricted; sig { use super::Foo; val f: fn(x: Foo) -> i64 }; source "let f = |x| 1" }; type Foo = {a: i64}; st
ok type Foo = {a: i64}; let st = mod foo dynamic { sandbox unrestricted; sig { val f: fn(x: super::Foo) -> i64 }; source "let f = |x: super::Foo| x.a" }; st
no "super::Foo names the type a later statement of this module declares" let st = mod foo dynamic { sandbox unrestricted; sig { val f: fn(x: super::Foo) -> i64 }; source "let f = |x| 1" }; type Foo = {a: i64}; st
```

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
the trait takes `self` beyond its receiver (another parameter, the
bound of a variable, or a function's parameter or a writable
reference's referent in its result, `-> {cb: fn(d: self) -> string}`;
`self` as data or as what a returned function returns is each member's
own): then select on the value and call per member. A
union two of whose members may share a value where neither holds the
other is refused too (`Array<[i64, string]>` beside `Array<[i64,
null]>`; any two array types, lists or maps, which share the empty
one): give the value one member's type, or select on it yourself.
Core `Eq`/`Ord`/`Display` ride the value everywhere (map keys, sort,
operators, printing); only abstract types may implement them outside
core. The io
traits `Read`/`Lines`/`Write`/`Close`/`Seek`/`Socket` describe streams
whose TYPE is their kind (`sys::fs::File`, `sys::tcp::TcpStream`, …).

In an impl, `self` is the impl's target in every type the impl writes (a
method's `let` annotation, its lambda's parameters and result, anything
inside it, a module's signature there included), as in a default it is
the trait's: `|s: self, x|` in `impl T for i64` is `|s: i64, x|`, and
`[self, 'c]` there is `[i64, 'c]`. A written `self` the trait does not
declare there is refused, and so is a `self` in a `type` definition
inside a generic impl's method (the target holds the impl's variables,
which a definition cannot name: give it a parameter), and a `self` in a
module's signature inside a generic impl's method or a default (a
signature names only variables of its own: write one). A target the impl
names (`impl T for W`) is a name a signature's `self` writes, so the
signature imports it: `use super::W;`. A bound the trait declares over `self` is over the
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
no "T::mk takes `self` beyond it" trait T { val mk: fn(self) -> {cb: fn(d: self) -> string} }; impl T for i64 { let mk = |s| {cb: |d: i64| "i"} }; impl T for string { let mk = |s| {cb: |d: string| d} }; let v: [i64, string] = 1; T::mk(v)
ok trait T { val mk: fn(self) -> {cb: fn(d: i64) -> self} }; impl T for i64 { let mk = |s| {cb: |d: i64| s} }; impl T for string { let mk = |s| {cb: |d: i64| "x"} }; let v: [i64, string] = 1; T::mk(v)
ok trait T { val m: fn<'b: [self, null]>(self, x: 'b) -> 'b }; impl T for i64 { let m = |s, x| x }; impl T for string { let m = |s, x| x }; let v: [i64, string] = 1; select v { i64 as i => T::m(i, null), string as s => T::m(s, null) }
no "neither of which holds the other: give the value one member's type" trait T { val m: fn(self) -> i64 }; impl T for Array<i64> { let m = |s| 1 }; impl T for Array<string> { let m = |s| 2 }; let e: Array<i64> = []; let v: [Array<i64>, Array<string>] = e; T::m(v)
ok trait T { val m: fn(self) -> i64 }; impl T for Array<i64> { let m = |s| 1 }; impl T for Array<string> { let m = |s| 2 }; let e: Array<i64> = []; T::m(e)
ok trait T { val m: fn(self) -> i64 }; impl T for Array<i64> { let m = |s| 1 }; impl T for Array<string> { let m = |s| 2 }; let e: Array<i64> = []; let v: [Array<i64>, Array<string>] = e; select v { Array<i64> as a => T::m(a), Array<string> as b => T::m(b) }
ok trait T { val m: fn(self) -> i64 }; impl T for Array<i64> { let m = |s| 1 }; impl T for null { let m = |s| 2 }; let v: [Array<i64>, null] = [1]; T::m(v)
no "type P writes `self`" trait T { val m: fn(self) -> i64 }; impl<'a> T for Array<'a> { let m = |s| { type P = self; let p: P = s; array::len(p) } }; T::m([1])
ok trait T { val m: fn(self) -> i64 }; impl<'a> T for Array<'a> { let m = |s| { type P<'b> = Array<'b>; let p: P<'a> = s; array::len(p) } }; T::m([1])
no "i64 does not contain string" trait T { val m: fn(self) -> i64 }; impl T for i64 { let m = |s| { let st = mod foo dynamic { sandbox whitelist [core]; sig { val f: fn(x: self) -> self }; source "let f = |x| x" }; select st { error as e => 0, null as _ => str::len(foo::f("ab")) + s } } }; T::m(1)
no "[i64, null] does not contain string" trait T { val m: fn(self) -> i64 }; impl T for i64 { let m = |s| { let st = mod foo dynamic { sandbox whitelist [core]; sig { val f: fn<'b: [self, null]>(x: 'b) -> 'b }; source "let f = |x| x" }; select st { error as e => 0, null as _ => str::len(foo::f("ab")) + s } } }; T::m(1)
no "undefined type W" type W = Abstract<i64>; trait T { val m: fn(self) -> i64 }; impl T for W { let m = |s| { let st = mod foo dynamic { sandbox whitelist [core]; sig { val f: fn(x: self) -> self }; source "let f = |x| x" }; select st { error as e => 0, null as _ => foo::f(s).0 } } }; T::m(W(7))
ok type W = Abstract<i64>; trait T { val m: fn(self) -> i64 }; impl T for W { let m = |s| { let st = mod foo dynamic { sandbox whitelist [core]; sig { use super::W; val f: fn(x: self) -> self }; source "let f = |x| x" }; select st { error as e => 0, null as _ => foo::f(s).0 } } }; T::m(W(7))
no "the signature of module foo writes `self`" trait T { val m: fn(self) -> i64 }; impl<'a> T for Array<'a> { let m = |s| { let st = mod foo dynamic { sandbox whitelist [core]; sig { val f: fn(x: self) -> self }; source "let f = |x| x" }; select st { error as e => 0, null as _ => array::len(s) } } }; T::m([1])
no "which in a default is the trait's type variable" trait T { val m: fn(self) -> i64 = |s| { let st = mod foo dynamic { sandbox whitelist [core]; sig { val f: fn(x: self) -> self }; source "let f = |x| x" }; select st { error as e => 0, null as _ => 1 } } }; impl T for i64 {}; T::m(1)
ok trait T { val m: fn(self) -> i64 = |s| { let st = mod foo dynamic { sandbox whitelist [core]; sig { val f: fn(x: 'q) -> 'q }; source "let f = |x| x" }; select st { error as e => 0, null as _ => str::len(foo::f("ab")) } } }; impl T for i64 {}; T::m(1)
```

A method's value decides none of the variables its trait's signature
declares: a lambda narrower than one is refused at the impl, and a
variable beside a written type (`x: [Array<'a>, Array<string>]`) stays
the method's variable. Leave the method's `let` unannotated: an
annotation written there is decided by the value in the order of its
positions, as any `let`'s is. A union stood in the method's result or in
a callback's parameter takes the lambda's value as it is. A type the
lambda writes names variables of its own, even one named like the
declaration's, and the check cannot tell whose it is: write that place
as the member the lambda gives.

```graphix,cases
ok trait T { val m: fn(self, x: [Array<'a>, Array<string>], y: 'a) -> 'a }; impl T for i64 { let m = |s, x, y| y }; (T::m(1, [1], 2), T::m(1, ["a"], "b"))
ok trait T { val m: fn(self, x: [Array<'a>, Array<string>], y: 'a) -> 'a = |s, x, y| y }; impl T for i64 {}; (T::m(1, [1], 2), T::m(1, ["a"], "b"))
no "does not contain fn(s: i64, x: i64) -> i64" trait T { val m: fn(self, x: 'a) -> i64 }; impl T for i64 { let m = |s, x: i64| 0 }; T::m(1, 1)
no "signature has type variable 'a where implementation has string" trait T { val m: fn(self, x: [Array<'a>, Array<string>], y: 'a) -> 'a }; impl T for i64 { let m: fn(self, x: [Array<'c>, Array<string>], y: 'c) -> 'c = |s, x, y| y }; T::m(1, [1], 2)
ok trait T { val m: fn(self, y: 'a, x: [Array<'a>, Array<string>]) -> 'a }; impl T for i64 { let m: fn(self, y: 'c, x: [Array<'c>, Array<string>]) -> 'c = |s, y, x| y }; T::m(1, 2, [1])
ok trait T { val m: fn(self) -> [Array<'a>, Array<string>] }; impl T for i64 { let m = |s| ["x"] }; T::m(1)
no "could go to Array<string> or Array<'a1: unbound>" trait T { val m: fn(self) -> [Array<'a>, Array<string>] }; impl T for i64 { let m = |s| -> [Array<'a>, Array<string>] ["x"] }; T::m(1)
ok trait T { val m: fn(self) -> [Array<'a>, Array<string>] }; impl T for i64 { let m = |s| -> Array<string> ["x"] }; T::m(1)
ok trait T { val m: fn(self, k: fn(v: [{a: 'a}, {a: string}]) -> i64, y: 'a) -> i64 }; impl T for i64 { let m = |s, k: fn(v: {a: string}) -> i64, y| k({a: "x"}) }; T::m(1, |v: [{a: i64}, {a: string}]| 7, 2)
```

A method, an impl's or a default, is generic where a `let` of its value
is: a lambda, or a read of a generic binding or of a field or tuple of
one (`let put = s.g`), with no annotation on the method's `let`. Any
other value (a call's result, a literal's projection) is one instance,
refused under a variable each call picks. A method that reads a binding
has exactly that binding's type: under a wider declaration, write a
lambda or annotate the method's `let`. A lambda method that calls a call's
result a binding holds (`s0.h` over `let s0 = {h: mk()}`) shares that one
instance: the refusal names the binding and asks for a lambda in that
function's place.

```graphix,cases
no "in that function's place" let mk = || |s, x| x; let s0 = {g: |s, x| x, h: mk()}; trait Put { val put: fn(self, x: 'a) -> 'a }; impl Put for i64 { let put = |s, x| s0.h(s, x) }; (Put::put(1, 1), Put::put(1, "s"))
ok let s0 = {g: |s, x| x, h: |s, x| x}; trait Put { val put: fn(self, x: 'a) -> 'a }; impl Put for i64 { let put = |s, x| s0.h(s, x) }; (Put::put(1, 1), Put::put(1, "s"))
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

A call of a method reads the trait's signature, where a function the
method returns is not known, so no call judges that function's labeled
default: the method's value must fit the declared return as it is
written. Under a declared variable (`fn(self) -> {cb: fn(?#d: 'z, x:
i64) -> string}` over `#d = 1`) it is refused, whatever the calls pass:
write the default's type in the trait's signature (`?#d: i64`), or give
the default the variable's type (`fn(self, v: 'z) -> ..` over `#d = v`).
A default that reads a binding nothing has decided yet is refused there
too, naming the binding to annotate. A default of the method's own
label is left to the call that omits it.

```graphix,cases
no "write the default's type in the trait's signature" trait T { val mk: fn(self) -> {cb: fn(?#d: 'z, x: i64) -> string} }; impl T for i64 { let mk = |s| {cb: |#d = 1, x: i64| cast<string>(d)$} }; T::mk(1).cb(7)
ok trait T { val mk: fn(self) -> {cb: fn(?#d: i64, x: i64) -> string} }; impl T for i64 { let mk = |s| {cb: |#d = 1, x: i64| cast<string>(d)$} }; T::mk(1).cb(7)
ok trait T { val mk: fn(self, v: 'z) -> {cb: fn(?#d: 'z, x: i64) -> string} }; impl T for i64 { let mk = |s, v| {cb: |#d = v, x: i64| cast<string>(d)$} }; T::mk(1, "s").cb(7)
no "`k` is undecided here; annotate `k`" let k = never(); trait T { val mk: fn(self) -> {cb: fn(?#d: 'z, x: i64) -> string} }; impl T for i64 { let mk = |s| {cb: |#d = k, x: i64| cast<string>(d)$} }; k <- 1; T::mk(1).cb(7)
ok trait T { val cb: fn(?#d: 'z, self, x: i64) -> string }; impl T for i64 { let cb = |#d = 1, s, x: i64| cast<string>(d)$ }; T::cb(1, 7)
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
the implementation (`'a: [i64, string, f64]`) is fine. A bound of
another shape than the signature's (``'q: ['p, `X, `Y]`` under ``'b: ['a,
`X]``) is judged by what its own variables admit: `'p` stands for `'a`
only where `'p`'s bound holds every type `'a`'s does (`'p: Singleton`
only where `'a` declares `Singleton` or a bound that implies it). A
signature variable inside a union is any type its bounds admit there
too: ``x: ['a, `X]`` over `'q: OneNumber |x: 'q|` is refused under `'a:
Number` (`declare 'a: OneNumber`) and holds under `'a: Number +
OneNumber` (`Singleton` implies it); over `'q: NoError |x: 'q|` it holds
only where a bound of `'a` admits no error. A
variable only bounds name is one type wherever they name it. Named at two
places of the signature it is its own bound's type, which must hold both
(`'p: [i64, string]`, `'p: Any`); with no bound that is one type (none, a
predicate alone, a union under `Singleton`) it is refused: name it at a
parameter, as the signature does. Beside a variable that is its own
bound's type, every other variable only bounds name that has such a bound
is its own too, so one a function's argument names (`'s: fn(x: 'k) ->
i64`) is refused: name one of the two at a parameter, or give the second
no such bound. A method's variables carry its trait's bounds, so there the
second may be the trait's own (`'e` below): give each of the trait's
variables a variable of the impl's. A
trait method,
an impl's or a default, is judged the same way:

```graphix,cases
no "requires 'a: [i64, f64], which the signature does not declare" trait T { val m: fn<'a: [i64, string], 'b: Array<'a>>(self, x: 'b) -> 'b }; impl T for i64 { let m = 'p: [i64, f64], 'q: Array<'p> |s, x: 'q| -> 'q x }; T::m(1, ["s"])
ok trait T { val m: fn<'a: [i64, string], 'b: Array<'a>>(self, x: 'b) -> 'b }; impl T for i64 { let m = 'p: [i64, string, f64], 'q: Array<'p> |s, x: 'q| -> 'q x }; T::m(1, ["s"])
no "requires 'b: ['p, `X, `Y], which the signature does not declare" trait T { val m: fn<'a: [i64, string], 'b: ['a, `X]>(self, x: 'b) -> 'b }; impl T for i64 { let m = 'p: [i64, f64], 'q: ['p, `X, `Y] |s, x: 'q| -> 'q x }; T::m(1, "s")
ok trait T { val m: fn<'a: [i64, string], 'b: ['a, `X]>(self, x: 'b) -> 'b }; impl T for i64 { let m = 'p: [i64, string], 'q: ['p, `X, `Y] |s, x: 'q| -> 'q x }; T::m(1, "s")
no "requires 'b: ['p, `X, `Y], which the signature does not declare" trait T { val m: fn<'a: [i64, string], 'b: ['a, `X]>(self, x: 'b) -> 'b }; impl T for i64 { let m = 'p: Singleton, 'q: ['p, `X, `Y] |s, x: 'q| -> 'q x }; T::m(1, "s")
ok trait T { val m: fn<'a: [i64, string] + Singleton, 'b: ['a, `X]>(self, x: 'b) -> 'b }; impl T for i64 { let m = 'p: Singleton, 'q: ['p, `X, `Y] |s, x: 'q| -> 'q x }; T::m(1, "s")
no "'p is one type wherever a bound names it" trait T { val m: fn<'a: [i64, string] + Singleton, 'c: [i64, string] + Singleton, 'b: (i64, 'a), 'd: (i64, 'c)>(self, x: 'b, y: 'd) -> i64 }; impl T for i64 { let m = 'p: [i64, string] + Singleton, 'q: (Number, 'p), 'r: (Number, 'p) |s, x: 'q, y: 'r| -> i64 0 }; T::m(1, (1, "s"), (1, 1))
ok trait T { val m: fn<'a: [i64, string], 'c: [i64, string], 'b: (i64, 'a), 'd: (i64, 'c)>(self, x: 'b, y: 'd) -> i64 }; impl T for i64 { let m = 'p: [i64, string], 'q: (Number, 'p), 'r: (Number, 'p) |s, x: 'q, y: 'r| -> i64 0 }; T::m(1, (1, "s"), (1, 1))
no "takes 'e as its own too" trait T { val m: fn<'a: [i64, string], 'c: [i64, string], 'b: (i64, 'a), 'd: (i64, 'c), 'e: [i64, string], 'g: fn(x: 'e) -> i64>(self, x: 'b, y: 'd, z: 'g) -> i64 }; impl T for i64 { let m = 'p: [i64, string], 'q: (Number, 'p), 'r: (Number, 'p) |s, x: 'q, y: 'r, z| -> i64 0 }; T::m(1, (1, "s"), (1, 1), |x: i64| x)
ok trait T { val m: fn<'a: [i64, string], 'c: [i64, string], 'b: (i64, 'a), 'd: (i64, 'c), 'e: [i64, string], 'g: fn(x: 'e) -> i64>(self, x: 'b, y: 'd, z: 'g) -> i64 }; impl T for i64 { let m = 'p: [i64, string], 'o: [i64, string], 'q: (Number, 'p), 'r: (Number, 'o) |s, x: 'q, y: 'r, z| -> i64 0 }; T::m(1, (1, "s"), (1, 1), |x: i64| x)
```

A `val`'s type variable stands only inside a function type, where each
use picks it: `val v: Array<'a>` is refused (declare it at one type,
`Array<i64>`), and so is `val c: {f: fn(x: 'a) -> string, last: ['a,
null]}`. The implementation must be as general as the val: under `val f:
fn(x: 'a) -> 'a`, `let f = |x| { y <- x; x }` over an outer `let y =
never()` is refused (`let y: Any`, or the val at one type), and so is a
written `let f = |x| x; f <- |x| x` (declare the val at one type), and
`let f = |x| k(x)` over a written `let k = |x| x; k <- |x| x`, whose
type every read of `k` shares (the val at one type). A
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
