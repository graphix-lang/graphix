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

A `use` is in force throughout its statement list, above it as below,
as in Rust: `type Op = {mode: Mode}`, `let m: Mode = ..` or a cast above
`use sys::fs::Mode` means the imported `Mode`, never an outer or prelude
type of the name. A name is declared once in a list: a second `use` of
it, and a `use` of a name the list declares as a type, are refused; a
`.gx` reads what its `.gxi` imports. A `let` still rebinds from where it
stands, an imported name included, and an inner block may shadow what an
outer one declares or imports. One order remains: a name a `use` imports
from a module declared further down is not read by a cast, a type test
or a destructuring `let` until the `mod`.

```graphix,cases
ok let m = cast<Mode>(`Read); use sys::fs::Mode; m
ok use sys::fs::Mode; let m = cast<Mode>(`Read); m
ok type Op = {mode: Mode}; use sys::fs::Mode; let o: Op = {mode: `Read}; o.mode
ok let m: Mode = `Read; use sys::fs::Mode; m
ok let m = cast<Mode>(`Read); use sys::fs::*; m
no "`Mode` is already imported here: remove the repeated import" use sys::fs::Mode; use sys::fs::Mode; 0
no "`Mode` is already defined in this scope" use sys::fs::Mode; type Mode = i64; 0
ok { let len = 1; use array::len; len }
ok trait Show { val show: fn(self) -> i64 }; let r = { let f = |x: Show| Show::show(x); use array::*; 0 }; r
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
there, `use super::Foo;`, or written by its path, `super::Foo`.

```graphix,cases
no "undefined type Foo in foo" type Foo = {a: i64}; let st = mod foo dynamic { sig { val f: fn(x: Foo) -> i64 }; source "let f = |x| 1" }; st
ok type Foo = {a: i64}; let st = mod foo dynamic { sig { use super::Foo; val f: fn(x: Foo) -> i64 }; source "let f = |x: Foo| x.a" }; st
ok let st = mod foo dynamic { sig { use super::Foo; val f: fn(x: Foo) -> i64 }; source "let f = |x| 1" }; type Foo = {a: i64}; st
ok type Foo = {a: i64}; let st = mod foo dynamic { sig { val f: fn(x: super::Foo) -> i64 }; source "let f = |x| 1" }; st
ok let st = mod foo dynamic { sig { val f: fn(x: super::Foo) -> i64 }; source "let f = |x| 1" }; type Foo = {a: i64}; st
```

A dynamic module's body sees only what its `sig` imports: no package,
`core` included, and nothing of the program around it. Operators and
literals need no import. `use core;` brings a package whole, `use
array::len;` one name, `use super::limit;` a binding declared above the
module, `use super::*;` every name the enclosing scope declares and no
package. The body may still write a granted name by its path
(`array::len`, `super::limit`). An import that names nothing above the
module is refused at the check; a body that reaches past its sig's
imports fails at the load, so it shows in the module's status and never
at the check. There is no `sandbox` statement.

```graphix,cases
ok let k = 1; let st = mod m dynamic { sig { use core; use array::len; use super::k; val v: i64 }; source "let v = k" }; st
no "the sig imports `k`, which nothing above this module declares" let st = mod m dynamic { sig { use super::k; val v: i64 }; source "let v = k" }; let k = 1; st
no "a dynamic module has no `sandbox`" let st = mod m dynamic { sandbox whitelist [core]; sig { val v: i64 }; source "let v = 1" }; st
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
`use Show::{show}`); a bare `show(x)` is "show not defined". Core
`Eq`/`Ord`/`Display` ride the value everywhere (map keys, sort,
operators, printing); only abstract types may implement them outside
core. The io
traits `Read`/`Lines`/`Write`/`Close`/`Seek`/`Socket` describe streams
whose TYPE is their kind (`sys::fs::File`, `sys::tcp::TcpStream`, …).

Dispatch is static on the self argument's type; a union self compiles
to a select that tests each member. A call the dispatch cannot make is
refused as `T does not contain X`, the trait and the self argument's
type: the message names no cause and no remedy. The causes are three. A
type, or a member of the union, has no impl. The self is a union and a
method of the trait takes `self` beyond its receiver (another
parameter, the bound of a variable, or a function's parameter or a
writable reference's referent in its result, `-> {cb: fn(d: self) ->
string}`; `self` as data or as what a returned function returns is each
member's own): select on the value and call per member. Two members of
the union may share a value where neither holds the other (`Array<[i64,
string]>` beside `Array<[i64, null]>`; any two array types, lists or
maps, which share the empty one): give the value one member's type, or
select on it yourself.

```graphix,cases
ok trait T { val m: fn(self) -> i64 }; impl T for i64 { let m = |s| 1 }; impl T for string { let m = |s| 2 }; let v: [i64, string] = 1; T::m(v)
no "T does not contain [i64, null, string]" trait T { val m: fn(self) -> i64 }; impl T for i64 { let m = |s| 1 }; impl T for string { let m = |s| 2 }; let v: [i64, string, null] = 1; T::m(v)
no "T does not contain [i64, string]" trait T { val m: fn<'b: [self, null]>(self, x: 'b) -> 'b }; impl T for i64 { let m = |s, x| x }; impl T for string { let m = |s, x| x }; let v: [i64, string] = 1; T::m(v, null)
no "T does not contain [i64, string]" trait T { val mk: fn(self) -> {cb: fn(d: self) -> string} }; impl T for i64 { let mk = |s| {cb: |d: i64| "i"} }; impl T for string { let mk = |s| {cb: |d: string| d} }; let v: [i64, string] = 1; T::mk(v)
ok trait T { val mk: fn(self) -> {cb: fn(d: i64) -> self} }; impl T for i64 { let mk = |s| {cb: |d: i64| s} }; impl T for string { let mk = |s| {cb: |d: i64| "x"} }; let v: [i64, string] = 1; T::mk(v)
ok trait T { val m: fn<'b: [self, null]>(self, x: 'b) -> 'b }; impl T for i64 { let m = |s, x| x }; impl T for string { let m = |s, x| x }; let v: [i64, string] = 1; select v { i64 as i => T::m(i, null), string as s => T::m(s, null) }
no "T does not contain [Array<i64>, Array<string>]" trait T { val m: fn(self) -> i64 }; impl T for Array<i64> { let m = |s| 1 }; impl T for Array<string> { let m = |s| 2 }; let e: Array<i64> = []; let v: [Array<i64>, Array<string>] = e; T::m(v)
ok trait T { val m: fn(self) -> i64 }; impl T for Array<i64> { let m = |s| 1 }; impl T for Array<string> { let m = |s| 2 }; let e: Array<i64> = []; T::m(e)
ok trait T { val m: fn(self) -> i64 }; impl T for Array<i64> { let m = |s| 1 }; impl T for Array<string> { let m = |s| 2 }; let e: Array<i64> = []; let v: [Array<i64>, Array<string>] = e; select v { Array<i64> as a => T::m(a), Array<string> as b => T::m(b) }
ok trait T { val m: fn(self) -> i64 }; impl T for Array<i64> { let m = |s| 1 }; impl T for null { let m = |s| 2 }; let v: [Array<i64>, null] = [1]; T::m(v)
```

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
signature imports it: `use super::W;`. A bound the trait declares over
`self` is over the target in an impl too: the method writes it with
`self` or with the target, or leaves it out. A narrower one (`'q: self`
under `'b: [self, null]`) is refused as a mismatch of the two bounds.

```graphix,cases
ok trait T { val m: fn(self, x: i64) -> i64 }; impl T for i64 { let m = |s: self, x| s + x }; T::m(1, 2)
ok trait T { val m: fn(self, x: [i64, string]) -> i64 }; impl T for i64 { let m = |s, x: [self, 'c]| 0 }; T::m(1, "q")
no "string does not contain i64" trait T { val m: fn(self, x: string) -> string }; impl T for i64 { let m = |s, x: self| x }; T::m(1, "q")
ok trait T { val m: fn<'b: [self, null]>(self, x: 'b) -> 'b }; impl T for i64 { let m = 'q: [self, null] |s, x: 'q| -> 'q x }; (T::m(1, 2), T::m(1, null))
ok trait T { val m: fn<'b: [self, null]>(self, x: 'b) -> 'b }; impl<'a> T for Array<'a> { let m = 'q: [Array<'a>, null] |s, x: 'q| -> 'q x }; (T::m([1], [2]), T::m([1], null))
ok trait T { val m: fn<'b: [self, null]>(self, x: 'b) -> i64 }; impl T for i64 { let m = |s, x| select x { null as _ => s, i64 as v => v + s } }; T::m(1, 2)
no "i64 does not contain 'b: unbound within [i64, null]" trait T { val m: fn<'b: [self, null]>(self, x: 'b) -> 'b }; impl T for i64 { let m = 'q: self |s, x: 'q| -> 'q x }; T::m(1, 2)
no "type P writes `self`" trait T { val m: fn(self) -> i64 }; impl<'a> T for Array<'a> { let m = |s| { type P = self; let p: P = s; array::len(p) } }; T::m([1])
ok trait T { val m: fn(self) -> i64 }; impl<'a> T for Array<'a> { let m = |s| { type P<'b> = Array<'b>; let p: P<'a> = s; array::len(p) } }; T::m([1])
no "i64 does not contain string" trait T { val m: fn(self) -> i64 }; impl T for i64 { let m = |s| { let st = mod foo dynamic { sig { val f: fn(x: self) -> self }; source "let f = |x| x" }; select st { error as e => 0, null as _ => str::len(foo::f("ab")) + s } } }; T::m(1)
no "[i64, null] does not contain string" trait T { val m: fn(self) -> i64 }; impl T for i64 { let m = |s| { let st = mod foo dynamic { sig { val f: fn<'b: [self, null]>(x: 'b) -> 'b }; source "let f = |x| x" }; select st { error as e => 0, null as _ => str::len(foo::f("ab")) + s } } }; T::m(1)
no "undefined type W" type W = Abstract<i64>; trait T { val m: fn(self) -> i64 }; impl T for W { let m = |s| { let st = mod foo dynamic { sig { val f: fn(x: self) -> self }; source "let f = |x| x" }; select st { error as e => 0, null as _ => foo::f(s).0 } } }; T::m(W(7))
ok type W = Abstract<i64>; trait T { val m: fn(self) -> i64 }; impl T for W { let m = |s| { let st = mod foo dynamic { sig { use super::W; val f: fn(x: self) -> self }; source "let f = |x| x" }; select st { error as e => 0, null as _ => foo::f(s).0 } } }; T::m(W(7))
no "the signature of module foo writes `self`" trait T { val m: fn(self) -> i64 }; impl<'a> T for Array<'a> { let m = |s| { let st = mod foo dynamic { sig { val f: fn(x: self) -> self }; source "let f = |x| x" }; select st { error as e => 0, null as _ => array::len(s) } } }; T::m([1])
no "which in a default is the trait's type variable" trait T { val m: fn(self) -> i64 = |s| { let st = mod foo dynamic { sig { val f: fn(x: self) -> self }; source "let f = |x| x" }; select st { error as e => 0, null as _ => 1 } } }; impl T for i64 {}; T::m(1)
ok trait T { val m: fn(self) -> i64 = |s| { let st = mod foo dynamic { sig { val f: fn(x: 'q) -> 'q }; source "let f = |x| x" }; select st { error as e => 0, null as _ => str::len(foo::f("ab")) } } }; impl T for i64 {}; T::m(1)
```

A method, an impl's or a default, takes its trait's declaration: its
lambda has the declared type at every parameter and result it leaves
unwritten, a recursive method's too. What it writes may take more than
the declaration and return less (`x: [i64, null]` under `x: i64`, `->
Array<string>` under `-> [Array<'a>, Array<string>]`); the other way is
refused at the impl. The declaration's variables are rigid in the
method: a parameter narrower than one (`x: i64` under `x: 'a`) is
refused, `i64 does not contain 'a`, and so is a body that returns
another type under one. A variable the lambda writes is its own,
whatever its name: it takes what the declaration passes it, beside a
written member of a union too (`-> [Array<'a>, Array<string>]` over
`["x"]`). An annotation on the method's `let` is held to the
declaration as the value is.

```graphix,cases
ok trait T { val m: fn(self, x: [Array<'a>, Array<string>], y: 'a) -> 'a }; impl T for i64 { let m = |s, x, y| y }; (T::m(1, [1], 2), T::m(1, ["a"], "b"))
ok trait T { val m: fn(self, x: [Array<'a>, Array<string>], y: 'a) -> 'a = |s, x, y| y }; impl T for i64 {}; (T::m(1, [1], 2), T::m(1, ["a"], "b"))
ok trait T { val m: fn(self, x: 'a) -> 'a }; impl T for i64 { let rec m = |s, x| select s { 0 => x, n => m(n - 1, x) } }; (T::m(1, 5), T::m(1, "s"))
ok trait T { val m: fn(self, x: i64) -> [i64, null] }; impl T for i64 { let m = |s, x: [i64, null]| -> i64 0 }; T::m(1, 1)
no "does not contain fn(s: i64, x: i64) -> [i64, null]" trait T { val m: fn(self, x: i64) -> i64 }; impl T for i64 { let m = |s, x| -> [i64, null] x }; T::m(1, 1)
no "i64 does not contain 'a" trait T { val m: fn(self, x: 'a) -> i64 }; impl T for i64 { let m = |s, x: i64| 0 }; T::m(1, 1)
no "'a: unbound does not contain i64" trait T { val m: fn(self, x: 'a) -> 'a }; impl T for i64 { let m = |s, x| 0 }; T::m(1, 1)
ok trait T { val m: fn(self, x: 'a, y: 'b) -> 'a }; impl T for i64 { let m = |s, x: 'b, y: 'a| -> 'b x }; T::m(1, 1, "s")
ok trait T { val m: fn(self) -> [Array<'a>, Array<string>] }; impl T for i64 { let m = |s| -> [Array<'a>, Array<string>] ["x"] }; T::m(1)
ok trait T { val m: fn(self) -> [Array<'a>, Array<string>] }; impl T for i64 { let m = |s| -> Array<string> ["x"] }; T::m(1)
ok trait T { val m: fn(self, x: [Array<'a>, Array<string>], y: 'a) -> 'a }; impl T for i64 { let m: fn(self, x: [Array<'c>, Array<string>], y: 'c) -> 'c = |s, x, y| y }; T::m(1, [1], 2)
no "does not implement the declared fn(self: i64, x: 'a) -> 'a" trait T { val m: fn(self, x: 'a) -> 'a }; impl T for i64 { let m: fn(self, x: i64) -> i64 = |s, x| x }; T::m(1, 1)
```

A method whose value reads a binding (a name, or a field or tuple of
one: `let put = s.g`) is held to the declaration by one instance of the
binding fitting it, annotated or not: the binding may be more general,
take more or return less (`let m = f` over `f: fn(s: i64) -> i64` under
`-> [i64, null]`). A function a call returns (`mk(0)`) and a binding
something writes are one instance, whose types all uses share: one fits
a declaration at one type and is refused under a variable each call
picks, as the method's value or called by the method's lambda through a
binding (`s0.h` over `let s0 = {h: mk()}`). So is a method that writes
an outer binding. The refusal says only "a declared type variable meets
a value from outside the definition that declares it", or, for the
outer write and for `let put = mk(0)` alike, "the implementation writes
it to a binding outside its let, which has one type all uses share".
Write the call inside the lambda (`|s, x| mk(0)(s, x)`), a lambda in the
function's place, a type on the written binding that holds every `'a`
(`let y: Any`), or the declaration at one type.

```graphix,cases
ok let s = {g: |s, x| x}; trait Put { val put: fn(self, x: 'a) -> 'a = s.g }; impl Put for i64 {}; (Put::put(1, 1), Put::put(1, "s"))
ok let s = {g: |s, x| x}; trait Put { val put: fn(self, x: 'a) -> 'a }; impl Put for i64 { let put: fn(self, x: 'a) -> 'a = s.g }; (Put::put(1, 1), Put::put(1, "s"))
ok let g = |s, x| x; trait T { val m: fn(self, x: i64) -> i64 }; impl T for i64 { let m = g }; T::m(1, 2)
ok let f = |s: i64| -> i64 s + 1; trait T { val m: fn(self) -> [i64, null] }; impl T for i64 { let m = f }; T::m(1)
no "does not contain fn(s: i64) -> [i64, null]" let f = |s: i64| -> [i64, null] s; trait T { val m: fn(self) -> i64 }; impl T for i64 { let m = f }; T::m(1)
no "the implementation writes it to a binding outside its let, which has one type all uses share" let mk = |z| |s, x| x; trait Put { val put: fn(self, x: 'a) -> 'a }; impl Put for i64 { let put = mk(0) }; (Put::put(1, 1), Put::put(1, "s"))
ok let mk = |z| |s, x| x; trait Put { val put: fn(self, x: 'a) -> 'a }; impl Put for i64 { let put = |s, x| mk(0)(s, x) }; (Put::put(1, 1), Put::put(1, "s"))
ok let mk = |z| |s, x| x + z; trait Put { val put: fn(self, x: i64) -> i64 }; impl Put for i64 { let put = mk(1) }; Put::put(1, 2)
no "a declared type variable meets a value from outside the definition that declares it" let g = |s, x| x; g <- |s, x| x; trait Put { val put: fn(self, x: 'a) -> 'a }; impl Put for i64 { let put = g }; (Put::put(1, 1), Put::put(1, "s"))
no "a declared type variable meets a value from outside the definition that declares it" let mk = || |s, x| x; let s0 = {g: |s, x| x, h: mk()}; trait Put { val put: fn(self, x: 'a) -> 'a }; impl Put for i64 { let put = |s, x| s0.h(s, x) }; (Put::put(1, 1), Put::put(1, "s"))
ok let s0 = {g: |s, x| x, h: |s, x| x}; trait Put { val put: fn(self, x: 'a) -> 'a }; impl Put for i64 { let put = |s, x| s0.h(s, x) }; (Put::put(1, 1), Put::put(1, "s"))
no "give that binding a type that holds every 'a" let y = never(); trait T { val m: fn(self, x: 'a) -> 'a }; impl T for i64 { let m = |s, x| { y <- x; x } }; T::m(1, 1)
ok let y: Any = never(); trait T { val m: fn(self, x: 'a) -> 'a }; impl T for i64 { let m = |s, x| { y <- x; x } }; T::m(1, 1)
```

A call of a method reads the trait's signature, where a function the
method returns is not known, so no call judges that function's labeled
default: the method's value must fit the declared return as it is
written. Under a declared variable (`fn(self) -> {cb: fn(?#d: 'z, x:
i64) -> string}` over `#d = 1`) it is refused, whatever the calls pass,
"write the default's type in the declared type": write `?#d: i64` in
the trait's signature, or give the default the variable's type
(`fn(self, v: 'z) -> ..` over `#d = v`). A default of the method's own
label is left to the call that omits it.

```graphix,cases
no "write the default's type in the declared type" trait T { val mk: fn(self) -> {cb: fn(?#d: 'z, x: i64) -> string} }; impl T for i64 { let mk = |s| {cb: |#d = 1, x: i64| cast<string>(d)$} }; T::mk(1).cb(7)
ok trait T { val mk: fn(self) -> {cb: fn(?#d: i64, x: i64) -> string} }; impl T for i64 { let mk = |s| {cb: |#d = 1, x: i64| cast<string>(d)$} }; T::mk(1).cb(7)
ok trait T { val mk: fn(self, v: 'z) -> {cb: fn(?#d: 'z, x: i64) -> string} }; impl T for i64 { let mk = |s, v| {cb: |#d = v, x: i64| cast<string>(d)$} }; T::mk(1, "s").cb(7)
ok trait T { val cb: fn(?#d: 'z, self, x: i64) -> string }; impl T for i64 { let cb = |#d = 1, s, x: i64| cast<string>(d)$ }; T::cb(1, 7)
```

A trait method's `#[pure]` or `#[sync]` holds for every implementation, even
an uncalled one. Builds reject violating bodies; dynamic modules fail to load.
Callback parameters retain caller-dependent effects, as in ordinary definitions.
`--check` does not elaborate these body assertions.

## A `val`

A `val` in a `.gxi` holds an implementation one instance of which fits
it, the val's variables standing for any type their bounds admit. So
the val may declare more than the implementation gives: `val x: [i64,
null]` over `let x = 3`, a function val returning more (`-> [i64, null]`
over `|x: i64| x + 1`), taking less (`x: i64` over `|x: [i64, null]|
..`), throwing more, or standing at one type over a generic
implementation. Every module sees the declared type, so the others take
the null off with `a::x$`. A trait's declaration holds a method by the
same fit: the cases here are methods, which one line holds.

The val declares every bound its implementation needs. What the body
asks of a declared variable is named with its remedy: a method's `x + y`
under `'a: Number` is "the body needs 'a within Singleton, which the
signature does not declare: write 'a: Number + Singleton", and `x == y`
under a bare `'a` wants `Discernible`. A variable the implementation writes is its own and takes
what the val passes it, so its bound must hold the declared variable's,
in whatever shape the two stand (``'q: ['p, `X, `Y]`` under ``'b: ['a,
`X]``): `'p: [i64, f64]` under `'a: [i64, string]` is refused, as a
mismatch of the two bounds with no remedy, and the wider `'p: [i64,
string, f64]` is fine. `'p: Singleton` takes only a variable that
declares `Singleton`. One variable of the implementation that two of the
val's reach (`'q: (Number, 'p), 'r: (Number, 'p)` under `'b: (i64, 'a),
'd: (i64, 'c)`) holds both, which `Singleton` refuses.

```graphix,cases
ok trait T { val m: fn(self, x: i64) -> [i64, null] throws `E }; impl T for i64 { let m = |s, x: [i64, null]| -> i64 0 }; T::m(1, 1)
no "the body needs 'a within Singleton, which the signature does not declare: write 'a: Number + Singleton" trait T { val m: fn<'a: Number>(self, x: 'a, y: 'a) -> 'a }; impl T for i64 { let m = |s, x, y| x + y }; T::m(1, 1, 2)
ok trait T { val m: fn<'a: Number + Singleton>(self, x: 'a, y: 'a) -> 'a }; impl T for i64 { let m = |s, x, y| x + y }; T::m(1, 1, 2)
no "the body needs 'a within Discernible, which the signature does not declare: add Discernible to the bound of 'a" trait T { val m: fn(self, x: 'a, y: 'a) -> bool }; impl T for i64 { let m = |s, x, y| x == y }; T::m(1, 1, 2)
ok trait T { val m: fn<'a: Discernible>(self, x: 'a, y: 'a) -> bool }; impl T for i64 { let m = |s, x, y| x == y }; T::m(1, 1, 2)
no "[i64, f64] does not contain 'a: unbound within [i64, string]" trait T { val m: fn<'a: [i64, string]>(self, x: 'a) -> 'a }; impl T for i64 { let m = 'p: [i64, f64] |s, x: 'p| -> 'p x }; T::m(1, 1)
ok trait T { val m: fn<'a: [i64, string]>(self, x: 'a) -> 'a }; impl T for i64 { let m = 'p: [i64, string, f64] |s, x: 'p| -> 'p x }; T::m(1, 1)
no "[i64, f64] does not contain 'b" trait T { val m: fn<'a: [i64, string], 'b: ['a, `X]>(self, x: 'b) -> 'b }; impl T for i64 { let m = 'p: [i64, f64], 'q: ['p, `X, `Y] |s, x: 'q| -> 'q x }; T::m(1, "s")
ok trait T { val m: fn<'a: [i64, string], 'b: ['a, `X]>(self, x: 'b) -> 'b }; impl T for i64 { let m = 'p: [i64, string], 'q: ['p, `X, `Y] |s, x: 'q| -> 'q x }; T::m(1, "s")
no "the implementation requires 'a: Singleton, which the signature does not declare" trait T { val m: fn<'a: [i64, string]>(self, x: 'a) -> 'a }; impl T for i64 { let m = 'p: Singleton |s, x: 'p| -> 'p x }; T::m(1, "s")
ok trait T { val m: fn<'a: [i64, string] + Singleton, 'b: ['a, `X]>(self, x: 'b) -> 'b }; impl T for i64 { let m = 'p: Singleton, 'q: ['p, `X, `Y] |s, x: 'q| -> 'q x }; T::m(1, "s")
no "['a, 'c] is more than one type" trait T { val m: fn<'a: [i64, string] + Singleton, 'c: [i64, string] + Singleton, 'b: (i64, 'a), 'd: (i64, 'c)>(self, x: 'b, y: 'd) -> i64 }; impl T for i64 { let m = 'p: [i64, string] + Singleton, 'q: (Number, 'p), 'r: (Number, 'p) |s, x: 'q, y: 'r| -> i64 0 }; T::m(1, (1, "s"), (1, 1))
ok trait T { val m: fn<'a: [i64, string], 'c: [i64, string], 'b: (i64, 'a), 'd: (i64, 'c)>(self, x: 'b, y: 'd) -> i64 }; impl T for i64 { let m = 'p: [i64, string], 'q: (Number, 'p), 'r: (Number, 'p) |s, x: 'q, y: 'r| -> i64 0 }; T::m(1, (1, "s"), (1, 1))
```

The implementation must be as general as the val, as a method must
(above): under `val f: fn(x: 'a) -> 'a`, `let f = |x| { y <- x; x }` over
an outer `let y = never()` is refused (`let y: Any`, or the val at one
type), and so is a `let f` something writes (`f <- |x| x`), a call's
result (`let f = mk(0)`), and `let f = |x| k(x)` over a `k` something
writes, whose type every read of `k` shares (the val at one type).

A `val`'s type variable stands only inside a function type, where each
use picks it: `val v: Array<'a>` is refused (declare it at one type,
`Array<i64>`), and so is `val c: {f: fn(x: 'a) -> string, last: ['a,
null]}`. A val generic in a variable (`val c: {f: fn(x: 'a) -> 'a}`,
`val g: fn<'a: Any>(..)` at its top, `val h: F<'a>` over `type F<'a> =
fn(x: 'a) -> 'a`) is copied by each use, takes only a write as general
as it, and no `&mut` of it or of a place holding its variable (`&mut
m::c.f`; `&mut m::c.n` over `n: i64` is fine): to write it at one type,
declare the val at that type. `let g = m::c; g <- ..` makes `g` one
instance of the val, which the write decides for every use of `g`. A
dynamic module's `sig` is an interface, so one line shows these:

```graphix,cases
ok let st = mod m dynamic { sig { val x: [i64, null] }; source "let x = 3" }; m::x$ + 1
no "declare the val at one type" let st = mod m dynamic { sig { val c: {f: fn(x: 'a) -> string, last: ['a, null]} }; source "let c = 1" }; st
ok let st = mod m dynamic { sig { val c: {f: fn(x: 'a) -> 'a} }; source "let c = {f: |x| x}" }; (m::c.f(1), m::c.f("s"))
ok let st = mod m dynamic { sig { val f: fn(x: 'a) -> 'a }; source "let f = |x| x" }; m::f <- |x| x; st
no "what is written to it must be as general" let st = mod m dynamic { sig { val f: fn(x: 'a) -> 'a }; source "let f = |x| x" }; m::f <- |x: i64| x; st
no "a writable reference to it would let a write narrow it" let st = mod m dynamic { sig { val c: {f: fn(x: 'a) -> 'a, n: i64} }; source "let c = {f: |x| x, n: 1}" }; let r = &mut m::c.f; st
ok let st = mod m dynamic { sig { val c: {f: fn(x: 'a) -> 'a, n: i64} }; source "let c = {f: |x| x, n: 1}" }; let r = &mut m::c.n; st
no "i64 does not contain string" let st = mod m dynamic { sig { val c: {f: fn(x: 'a) -> 'a} }; source "let c = {f: |x| x}" }; let g = m::c; g <- {f: |x: i64| x}; g.f("s")
```
