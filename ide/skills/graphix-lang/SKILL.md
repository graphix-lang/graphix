---
name: graphix-lang
description: Writing, reading or fixing Graphix code — any .gx or .gxi file, Graphix inside Rust test fixtures (run!/eval strings), stdlib .gx signatures, examples in book/src/examples, the netidx-admin TUI port. Graphix is NOT in the training set; load this before touching it.
---

# Writing Graphix

Graphix is a reactive dataflow language. Every expression is a node in a
graph; a node fires when a consumed input fires; nothing runs "again"
unless an input does. Read the book (`book/src/`) and the examples
(`book/src/examples/`) when this reference is not enough.

## Running and checking

```sh
graphix --check prog.gx     # parse + typecheck only
graphix prog.gx             # run
graphix --expand prog.gx    # check, and print every seq's lowered machine
graphix --no-fusion prog.gx # node-walk only (swallowed-error diagnostics live here)
```

A program under test must exit or the harness learns nothing:
`sys::exit(sys::time::after_idle(duration:100.ms, 0))` at the end (a
constant argument to `exit` runs at init — always gate it).

## The rules that bite (read these first)

- **Sample state you read in a handler**: `#on_press: |c| n <- c ~ n + 1`.
  Without `~` the callback captures the initial value.
- **An arm that connects to a binding it also reads unsampled is a
  self-loop**: `` `Enter => known <- upsert(known, x) `` re-fires itself
  every cycle (the accidental counter, 100% CPU). Write
  `known <- ev ~ upsert(known, x)`. The legitimate counter is
  `x <- clock ~ x + 1`.
- **A constant right-hand side in a select arm fires once per
  selection**, not per event: `` `Esc => screen <- `Menu `` fires when the
  arm becomes selected and not on a same-arm re-match. A handler that
  must act on every event samples it: `screen <- k ~ `Menu`. Both are
  tools: the constant form is "on entering this state".
- **An effect with several inputs samples the WHOLE input on the
  trigger**: `spawn(go ~ options(#args: a, prog))`, never
  `spawn(options(#args: a, go ~ prog))` — a call fires when ANY argument
  fires, so the second re-issues the spawn when `a` moves on its own.
- **`<-` lands next cycle.** You do not see the new value this cycle.
- **A function returning a level it does not derive from its argument
  fires nothing when re-called** (`|v| k`); write `|v| v ~ k`. This is
  what makes `f(trigger)` a sequencing tool only when `f` uses its arg.
- **A `never()` arm adds nothing to a select's type**: `let r = select b
  { true => never(), false => error(`E) }` makes `r` the error type alone,
  and a downstream `select r { error as e => .., v => .. }` is refused for
  a dead arm. Annotate the let with the union the reader will match:
  `let r: [i64, Error<`E>] = select ..`.
- **`~` banks, `~!` does not**: `e ~ v` is `v` at each fire of `e`, and a
  fire that finds `v` absent is paid when `v` first arrives; `e ~! v` is
  bottom then. Use `~!` when a late payment would be a phantom event.
- **Comments** are legal only on their own line above an expression, a
  select arm, an impl method or a struct-literal field. Trailing,
  interior and dangling comments are parse errors. `///` doc comments
  only in `.gxi`.

## Basics

Expression-oriented; the last expression of a file or block is its value;
`;` separates statements inside blocks; blocks need two or more
elements (`{x + 1}` is a syntax error; `(x)` is grouping, not a 1-tuple).

```graphix
let x = 42
let x: i64 = 42
let (a, b) = (1, 2)
let {x, y} = point
let rec f = |n| ...            // recursive binding (monomorphic)
let result = { let tmp = compute(); tmp + 1 }
```

## Types

Structural: same shape, same type.

```graphix
bool string bytes null  i8 i16 i32 i64 u8 u16 u32 u64 f32 f64 decimal
datetime duration  v32 v64 z32 z64
Array<i64>  Map<string, i64>  List<i64>   // List: [<1, 2>] literals, O(1) tail
(i64, string)                  // tuple, 2+ elements
{x: f64, y: f64}               // struct
`Tag | `Tag(i64, string)       // variants
[i64, string]                  // union; [T, null] is the option type
Error<`MyErr>  &i64            // error; reference
fn(x: i64) -> string throws `E // positional params in fn TYPES must be named
type Point = {x: f64, y: f64}; type Maybe<'a> = ['a, null]
type List2<'a> = [`Cons('a, List2<'a>), `Nil]   // recursive
'a: Number  'a: Int  'a: Float   // constraints; sets Number Int SInt UInt Float Real
```

Abstract (nominal) types: `type Counter = Abstract<i64>` as a whole
typedef body; `Counter(x)` constructs, `c.0` reads the payload, pattern
`Counter(p)` destructures — only where the definition is visible (the
defining module, or a public `Abstract<..>` in the gxi). `T as t` is a
nominal tag test anywhere, including over a union of abstracts. One
allocation per construction: handles and newtypes, not hot data.

## Literals

```graphix
42  3.14  true  false  null   u8:100  f32:3.14   // non-i64/f64 numbers are typ:value
"hello [name]!"                 // interpolation; escape \[ \] in literals
"""bare " [ ] ok, splice \[x]"""  // template: brackets are content, \[expr] interpolates
r"verbatim"  r#"has "quotes""#
[1, 2, 3]  [<1, 2>]  {"a" => 1}  (1, "two")  {x: 10, y: 20}  {x, y}
`Foo  `Bar(42)
datetime:"2020-01-01T00:00:00Z"  duration:1.0s  duration:500.ms   // units: ns us ms s ONLY
{s with field: v}               // functional update
```

## Operators (highest first)

```
* *? / /? % %?     + +? - -?     < > <= >=     == !=     &&     ||     ~ ~!
```

Unchecked arithmetic logs and bottoms on failure; checked (`+?` …)
returns `[T, Error<`ArithError(string)>]`. `&&`/`||` are strict
(`false && ⊥ = ⊥`). Unary `!x`, `&x`, `*x`. Postfix `x?` (raise to the
nearest catch), `x$` (or never). Both take the errors off `x`, or, when
`x` has none, the null; they chain (`x?$`) and sit anywhere in a postfix
chain (`(k ~ sel)$.name`). On an untyped parameter they read as the
error form, so a nullable one is annotated: `|t, scope: [Scope, null]|
.. (t ~ scope)$`.
`datetime - datetime` is refused: use `sys::time::diff(later, earlier)
-> duration`, `add`/`sub` for datetime ± duration. Durations print as
`1800.s`; format your own "30m".

Access: `s.f`, `t.0`, `a[i]`, `a[-1]`, `a[1..3]`, `m{"k"}` (Result),
`mod::name`.

## Functions

```graphix
let f = |x| x + 1
let h = |x: i64, y: i64| -> i64 x + y
let add = 'a: Number |x: 'a, y: 'a| -> 'a x + y
let greet = |#greeting = "hello", name| "[greeting], [name]!"   // labeled args
greet(#greeting: "hi", "world")   // labeled always by name, before positionals
let f = |@args: i64| args         // variadic (builtins only)
```

Calls dispatch statically. A HOF nested under its own callback is a
fresh instance, not recursion.

## Annotations

Do not write a type annotation unless the checker asks for one. It
asks for two things. A parameter whose fields you read and whose type
no call fixes: a top-level function, a handler handed to a widget
(`|e: Event|`). A callback passed to `array::map`/`find`/`fold` over a
typed array needs none. And a `let` whose initial value is narrower
than its writers: `let notice: [string, null] = null`, `let go: Any =
never()` with writers of two types, `let verify: Any = known` written
by a key. An array literal of records whose variant field differs
per element (`[{act: `A, ..}, {act: `B, ..}]`) is typed as a union of
record types, not one record with a union field, so it keeps its
element type (`let choices: Array<Choice> = [..]`) if a field is read
through an index. A fold's accumulator is the type of its init, so
`array::fold(xs, null, |acc, x| ..)` makes `acc` null: annotate the init
(`let init: [i64, null] = null`), never the callback. A `let` over a
call, a select or a seq needs nothing. A lambda's return type is needed
in two places only: a function declared in the `.gxi` keeps its `-> T`
in the `.gx` (an inferred return does not cross modules), and a
function with a type variable in its signature keeps it when a caller
reads a field of the result. To find the set: strip, `--check`, restore
what it names.

## Select

The only branching construct. Arms are consulted top down, structure
first, guard second; must be exhaustive with no dead arms; unselected
arms sleep (subscriptions paused); a bottom scrutinee yields a bottom
select (put `hold` on the scrutinee to persist across a bottom cycle).

```graphix
select x {
  i64 as n => n + 1,              // type test
  (f64, f64) as (n, a) => n / a,  // the WHOLE type comes before the structure pattern;
                                  // `(f64 as n, f64 as a)` is a parse error
  `Apple | `Pear => "fruit",      // or-pattern; captures type as the union
  `Other(name) if name != "" => name,   // a test on a capture is a GUARD, never a
                                         // nested `select cond { true => .., false => .. }`
  error as e => recover(e),  v => v,     // splits a Result; v is the bare success type
  (0, y) => y,
  {x: 0, y} => y,  {x, ..} => x,  // partial struct; several union members: `S as {x, ..}`
  [] => 0,  [a, b] => a + b,  [h, rest..] => h,  [a, ..] => a,  // length ladders count as coverage
  [<>] => 0,  [<h, t..>] => h,   // list patterns; suffix form refused
  v@ `Some(inner) => use_both(v, inner),
  _ => default
}
```

## Connect

`x <- e` is the only way to make a cycle; the write lands next cycle.
It is an expression form, not an operator: `x <- clock ~ x + 1` needs no
parens. A `<-` writes when its right side fires. A pure arm has nothing
to pause; an arm with a `<-` sleeps when unselected.

## Sequencing: `seq` and `seqq`

For multi-step ceremonies (a request, then a reply, then a write).
Straight-line only; no `if`/loops inside.

```graphix
let r = seqq request {             // seqq queues triggers; seq drops while busy
  let cfg = load(request);         // a call is issued once per entry over a snapshot of its args
  until ready;                     // wait for a bool level (never the last statement where its value is used)
  { a <- fetch(cfg); b <- probe(cfg) };  // a block: statements issued together, lets local
  let v = try { risky(cfg)? } with(e) { println("fallback"); 0 };   // the error branch; `catch` is refused in a seq
  write(v)                          // the block's value; each output releases the next queued request
}
```

The trigger is any expression. `seqq sys::time::after_idle(duration:250.ms,
key) { .. }` runs once per burst of presses, not once per press.
`seq let c = e { .. }` names the trigger's value for the body and nothing
after it; `seq let {x, y} = pt { .. }` destructures. Under `seqq` it is
the value that queued the run. `seq let c = (*r)$ { .. c.f .. }`
is how a nullable reference is consumed; `(*r).f` is refused.

`seq go abort(cancel) { .. }` ends the run in progress when `cancel`
fires: silently (no value, no error, a `try` is not taken), and the next
trigger starts fresh. The event is the run's first step, asleep between
runs: `abort(sys::time::timer(duration:10.s, false))` is a budget per
run, and a fire while idle aborts nothing. It reads the trigger's name as
this run's trigger (`seq let c = go abort(sys::time::after_idle(d, c))`).
Nothing is undone: clean up outside on the same event (`busy <- cancel ~
false`), and write a child that must die with the run to an outer
variable, not a `let`. Under `seqq`, `abort` starts the next queued
request and `flush(e)` (after `abort`, `seqq` only) also empties the
queue. A seq whose select arm sleeps mid-run is idle when it wakes.

A busy flag set on a trigger, a call sampled on that trigger, and the
flag cleared on the result is a seq written by hand, three statements
apart:

```graphix
busy <- go ~ true;  let r = f(go ~ x);  busy <- r ~ false      // never this
let r = seqq go { busy <- true; let r = f(x); busy <- false; r }   // this
```

`seqq` when the result must track the latest input (a dropped trigger
would leave `r` aligned with a stale `x`); `seq` when a re-trigger during
a run is noise. Debounce the trigger when a burst should cost one run.

A statement starts in the first cycle its predecessor's effect can be
seen: `let a = f(); let b = g(a)` issues `g` the cycle `f` produced;
`n <- n + 1; publish(n)` publishes the NEW `n` (a cycle later);
`a <- x; b <- y; let s = a + b` lands both writes in one cycle. A call
after a connect waits for the write (it may read anything); a block is
the override. A step completes when it produces a NEW value after its
entry, never on a value standing from an earlier run; a step that reads
a level takes it as it stands at entry and waits for it if absent. A `let`'s fire is
live only in the next step (a later `t ~ x` on it does not write).
`never()` in a step stalls the run. A seq inside a step is a step: with
no trigger it runs at every entry of its statement and the outer step
waits for it (two cycles over inline). `graphix --expand` shows the machine.
`range(i, j)` is the integer sequence builtin (throws `` `RangeError ``).

## Errors

```graphix
error(`NotFound("missing"))?     // raise to the nearest catch
{ catch(e) handle(e); risky()?; more()? }   // catch INSTALLS a handler for the rest of its block
```

`catch` is not control flow: the handler is a reactive expression that
runs when an error arrives; connect it to state you read. A second
catch shadows the first below it; a handler's own `?` rethrows outward.
Inside `seq`, use `try … with`.

`x$` is "or never": the success value, or bottom on an error. It is the
one-token spelling of `select x { error as _ => never(), v => v }` and
of `{ catch(e) never(); x? }` — never write either of those. A dropped
error is logged at warn with its site, in both engines (`RUST_LOG=warn`
and `--log-dir` in the shell). Use `$` when the failure is an expected
non-event (a missing optional file); use `catch` when something must
be told.

Both operators also take an option. `x$` on a `[T, null]` is `T` or
bottom, and says nothing, since a null is not a failure: it replaces the
`select x { null as _ => never(), v => v }` ladder. `x?` raises
`` `NullError(string) `` naming the operand, and only such a `?` adds that
tag to its catch. Errors come off first: on `[T, null, Error<E>]` one
`?` leaves `[T, null]` (so `commit(txn)?`, a `Result<null, E>`, does not
raise on success) and `get(t, k)?$` takes both. `opt::ok_or(x, `E)?`
when the catch should hear more than `NullError`.

`$` gates: while `x` is null or an error, a call or struct taking `x$`
does not fire and owes nothing when `x` returns. `f(k ~ t, sel$)` is
silent while `sel` is null, and so are `k ~ sel$` and `(k ~ sel)$`:
there is no stale last-good value to read.

## References and places

`&v`, `*r`, `*r <- new`. Widgets take `&` parameters so updates
propagate without rebuilding the tree. `&a[i]`, `&s.f`, `&t.0`, `&m{k}`
are places: reading follows the path, writing patches the root at
delivery; a dynamic key (`&vals[focus]`) is a moving reference.

## Modules, interfaces, traits

Rust-2018-style imports: `use array::map;` `use str::join as sjoin;`
`use tui::text::{self, *};` (the widget-module idiom), `use super::{f,
T};` `use package::a::b;` `mod name;`. Package names are path roots:
`array::map(xs, f)` works bare. A submodule sees nothing of its parent
implicitly. Declarations are statement-position only.

Interfaces: `foo.gxi` beside `foo.gx` (directories: `mod.gx`/`mod.gxi`);
`val`, `type`, `mod`, `trait`, `impl T for X;` declarations with `///`
docs; types and mods from the gxi are in scope in the impl. A hidden
type must be `Abstract<..>` or Rust-backed.

Traits: `trait Show { val show: fn(self) -> string; val twice: fn(self)
-> string = |s| "[show(s)] [show(s)]" }`, `impl Show for Counter { let
show = |c| .. }`, `impl<'a: Show> Show for Array<'a> { .. }`, bounds
`'a: Show + Eq`, `|x: Show|` sugar. Dispatch is static on the self
argument's type; a union self compiles to a select. Core `Eq`/`Ord`/
`Display` ride the value everywhere (map keys, sort, operators,
printing); only abstract types may implement them outside core. The io
traits `Read`/`Lines`/`Write`/`Close`/`Seek`/`Socket` describe streams
whose TYPE is their kind (`sys::fs::File`, `sys::tcp::TcpStream`, …).

## Standard library, briefly

Core (always in scope): `print println dbg log cast<T>(x) error is_err
filter filter_err count once uniq sum product min max mean and or all
queue hold take skip throttle range`. `is_err`, `filter` and `filter_err`
gate a stream; to branch on a Result, select on it. `never()` / `never<T>()` is
SYNTAX: typed bottom, args stay live; an unannotated `let` over
`never()` takes its type from its writers.

`opt` (over `['a, null]`): `is_some is_none or_default or and map
flat_map filter ok_or zip`. Unwrapping is the `$` operator, above. The
ladder it replaces also hides in two steps: `let p = select d { null as
_ => null, v => f(v) }; let q = p$` is `let q = f(d$)`. A null carried
forward only to be dropped is unwrapped at the source, once: `let dir =
d$`, and every reader (a later seq step included) takes `dir`.

`array`: map filter filter_map fold flatten find find_map concat push
window(#n, trig, v) len iter iterq sort enumerate zip. `map`, `str`
(`contains split trim replace join len parse ..`), `re`, `rand`.

`sys::time`: `timer(timeout, repeat)` (a `null` timeout STOPS it),
`after_idle(timeout, v)`, `now()`, `diff`, `add`, `sub`.
`sys::net`: `subscribe(path)`, `publish(path, v)`, `rpc`, `list`,
`write` — a `null` path tears the effect down; an arm never pauses
them. `sys::fs`, `sys::tcp`, `sys::tls`, `sys::process` (`spawn`,
`options`, `Proc`, `wait`), `http`, `json`/`toml`/`pack` (`read(bytes)`
async; `write` pure).

A sync variadic builtin called with no positional argument is a compile
error (`str::concat()`); it could never fire.

## UI

GUI (iced) programs return `Array<&Window>`; TUI (ratatui) programs
return one widget, usually under `input_handler(#handle: &f, &widget)`
where `f: |e: Event| -> [`Stop, `Continue]`. Widget arguments are `&`
references; `use tui::block::{self, *}` per widget module.

```graphix
let handle = |e: Event| -> [`Stop, `Continue] select e {
  `Key(k) => select k.kind {
    `Press => select k.code {
      kk@ `Up | kk@ `Char("k") if sel > 0 => { sel <- (kk ~ sel) - 1; `Stop },
      _ => `Continue },
    _ => `Continue },
  _ => `Continue
};
```

Widgets: gui — window text button text_input checkbox toggler radio
slider progress_bar pick_list column row container scrollable stack
space rule tooltip canvas chart image mouse_area keyboard_area
text_editor clipboard; tui — block paragraph list table tabs gauge
line_gauge sparkline bar_chart canvas chart calendar browser
input_handler overlay(#layers, base) + layer, `tui::suspend(level)`
(release the terminal to a child process while true), `tui::exit`.
Layout: `` `Fill `Shrink `Fixed(f64) ``; padding `` `All(f) `Axis({x,
y}) `Each({top, right, bottom, left}) ``; colors `` `Red .. `Rgb(r, g,
b) ``.

## Idioms

```graphix
let clock = sys::time::timer(duration:1.s, true)
let count = 0
count <- clock ~ count + 1                        // driven counter
data <- array::window(#n: 60, v ~ data, cast<f64>(v)?)   // sliding window
select x { n if n < limit => x <- x + 1, _ => never() }  // state that stops
// first element passing a test: filter_map, then an emptiness select —
// never map to options and find the non-null one
select array::filter_map(xs, |x| select probe(x) { error as _ => null, v if ok(v) => x, _ => null }) {
  [] => `Absent,
  [a, ..] => `Present(a)
}
let result = seqq go { let r = fetch(go); publish(path, r); r }   // ceremony
```

## Gotchas

- Union types use `[]`: `[i64, null]` is "i64 or null", not an array.
- Primitive type names are legal binding and field names; control
  keywords and literal words are not (`{type: v}` written explicitly);
  `bytes` is field-only.
- Reserved words in a name position are reported by the parser ("`ok`
  is a reserved word"); parse errors point at the furthest point reached.
- `use` imports a NAME, never a module's contents: `use sys::net` gives
  `net::subscribe`, not `subscribe`.
- Slice patterns bind `[init.., x]` (all but last, last); List patterns
  have no suffix form.
- A type test over an untyped parameter binds it: `|x| select x { null
  as _ => 0, v => v }` makes `x` null and the second arm dead. Annotate
  the parameter, `|x: [i64, null]|`. Arms of different types are not the
  problem: `select b { true => x, false => 1 }` is `['a, i64]`, and `[]`
  against `[1]` is `Array<i64>`.
- Labeled arguments are never positional, even without a default.
- `--check` a witness before arguing about a semantics question.
