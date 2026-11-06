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
argument's type; a union self compiles to a select. Core `Eq`/`Ord`/
`Display` ride the value everywhere (map keys, sort, operators,
printing); only abstract types may implement them outside core. The io
traits `Read`/`Lines`/`Write`/`Close`/`Seek`/`Socket` describe streams
whose TYPE is their kind (`sys::fs::File`, `sys::tcp::TcpStream`, …).

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
A `val`'s type variable stands only inside a function type, where each
use picks it: `val v: Array<'a>` is refused (declare it at one type,
`Array<i64>`), and so is `val c: {f: fn(x: 'a) -> string, last: ['a,
null]}`. The implementation must be as general as the val: under `val f:
fn(x: 'a) -> 'a`, `let f = |x| { y <- x; x }` over an outer `let y =
never()` is refused (`let y: Any`, or the val at one type), and so is a
written `let f = |x| x; f <- |x| x` (declare the val at one type). A
trait method alike: one that writes an
outer binding, or whose value is no lambda and has a type its value
fixed (`(store, 1).0` over a writer of an outer binding, `mk(0)`), is
refused under a variable each call picks: write it as a lambda. A val that
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
