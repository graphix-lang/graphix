---
name: graphix-lang
description: Writing, reading or fixing Graphix code — any .gx or .gxi file, Graphix inside Rust test fixtures (run!/eval strings), stdlib .gx signatures, examples in book/src/examples, the netidx-admin TUI port. Graphix is NOT in the training set; load this before touching it.
---

# Writing Graphix

Graphix is a reactive dataflow language. Every expression is a node in a
graph; a node fires when a consumed input fires; nothing runs "again"
unless an input does. Read the book (`book/src/`) and the examples
(`book/src/examples/`) when this reference is not enough.

## More of this skill, read when you need it

These sit beside this file. Reading one costs little and a wrong program
costs more: when in doubt, read it.

| before you | read |
|---|---|
| write a `seq` or `seqq`, or debug one that stalls | `seq.md` |
| write a generic function, or hold lambdas in data; or the check refuses a type variable, a bound, a quantifier, a union of variables or a "generic binding" | `generics.md` |
| select over a declared type variable or a value still undecided; or the check says "tests inside a value of", "not decided at this select" or "same runtime form" | `select.md` |
| write a `.gxi`, a module or a trait | `modules.md` |
| read or write JSON, TOML or pack, cast, parse, or print a number | `data.md` |
| write a TUI or GUI | `ui.md` |
| call a package beyond core, `sys::net`'s rpc, or a line reader | `stdlib.md`, and the package's `.gxi` |

A ```` ```graphix,cases ```` fence holds one program a line, as `--check`
judges it: `ok`, or `no "text"`, refused with that text in its message. The
gate checks every line (`examples_compile.rs`), so these are the claims you
can trust as written. A claim about what the check does goes in one.

## Running and checking

```sh
graphix --check prog.gx     # parse + typecheck only
graphix prog.gx             # run
graphix --expand prog.gx    # check, and print every seq's lowered machine
graphix --no-fusion prog.gx # node-walk only (unchecked-arithmetic diagnostics live here)
graphix fmt prog.gx         # format in place (--check, --stdout)
```

`#[native]` on an expression asserts it fuses whole into native code: a
build refuses it where anything stays in the node-walk. Inside a function
body the attribute waits for an instance: an uncalled definition is valid,
and a dynamic call or a later REPL call checks the attribute when it builds
that instance.

`str::parse`, `hbs::render`, the json/toml/pack writers, `array::sort`,
`list::sort`, `array::dedup` and every `re::*` builtin run in the node-walk.
They keep their last result while their own arguments are stale; a fused
fast call can run again whenever any input of its kernel fires. Bind these
operations outside a `#[native]` expression and pass their results in.

A program under test must exit or the harness learns nothing:
`sys::exit(sys::time::after_idle(duration:100.ms, 0))` at the end (a
constant argument to `exit` runs at init — always gate it).

## The rules that bite (read these first)

- **Sample state you read in a handler**: `#on_press: |c| n <- c ~ n + 1`.
  Without `~` the body does not depend on the call: `|c| n <- n + 1`
  reads and writes `n`, so it is the self-loop below, counting at 100%
  CPU from the moment the widget is built, with no click at all.
- **An arm that connects to a binding it also reads unsampled is a
  self-loop**: `` `Enter => known <- upsert(known, x) `` re-fires itself
  every cycle (the accidental counter, 100% CPU). Write
  `known <- ev ~ upsert(known, x)`. The legitimate counter is
  `x <- clock ~ x + 1`.
- **A constant right-hand side in a select arm fires once, at the arm's
  first entry**, and never again: a later entry resumes the arm, and its
  constants do not fire. `` `Esc => screen <- `Menu `` writes on the first
  Esc only (the check warns of it, as of a constant-only effect call or a
  `seq` with no trigger in an arm). Sample the event that selects the
  arm: `` k@ `Esc => screen <- k ~ `Menu ``, or use a seq.
- **An effect with several inputs samples the WHOLE input on the
  trigger**: `spawn(go ~ options(#args: a, prog))`, never
  `spawn(options(#args: a, go ~ prog))` — a call fires when ANY argument
  fires, so the second re-issues the spawn when `a` moves on its own.
- **`<-` lands next cycle.** You do not see the new value this cycle.
- **A value made later in the cycle reaches an earlier reader next
  cycle.** A cycle runs statements in text order and REPL inputs in input
  order; a reader that sits before what it reads (through a function value
  or a reference to a later binding, an earlier REPL input) gets each new
  value a cycle late, as a write. A backward reader that feeds what it
  reads runs a cycle at a time, as `x <- x + 1` does. A `*r` before a
  nested block or a call in its own statement that makes its target reads
  each value a cycle late too.
- **A function returning a level it does not derive from its argument
  fires nothing when re-called** (`|v| k`); write `|v| v ~ k`. This is
  what makes `f(trigger)` a sequencing tool only when `f` uses its arg.
  A seq step is the exception: a call a step makes is a fresh call at
  every run, its body born again reading its arguments and captures as
  they stand, so `let load = || sys::fs::read_all(path)` reads the file
  at each run. State inside the callee starts afresh each run (a `let`
  takes its initializer again; one whose initializer never fires, and a
  `catch`'s binder, which has none, keep their last value); what it
  captures from outside keeps its value. A captured event is read as it
  stands too: a step's call to `|u| ev` answers `ev`'s last event at
  once, never waiting for the next.
- **A `never()` arm adds nothing to a select's type**: `let r = select b
  { true => never(), false => error(`E) }` makes `r` the error type alone,
  and a downstream `select r { error as e => .., v => .. }` is refused for
  a dead arm. Annotate the let with the union the reader will match:
  `let r: [i64, Error<`E>] = select ..`.
- **`~` banks, `~!` does not**: `e ~ v` is `v` at each fire of `e`, and a
  fire that finds `v` absent is paid when `v` first arrives, once however
  many fired meanwhile, including when `v` has gone bottom after an earlier
  value; `e ~! v` is bottom then. Use `~!` when a late payment would be a
  phantom event. Both samples keep their last output while the trigger is
  bottom; bottom is not a trigger fire.
- **`seqq` reads the world once per run, `seq` once per step**: `seqq`
  captures every outer variable the body only reads along with the
  queued request, so two steps see the same `known`. Under `seq` a later
  step reads it as it stands then: `array::zip(known, states)` after a
  slow `let states = array::map(known, ..)` is misaligned under `seq`.
  Steps that must agree use `seqq`, take the value from the trigger
  (`seq let snap = trig { .. }`), or snapshot it as a step and read only
  the snapshot: `let snap = known; let states = array::map(snap, ..);
  array::zip(snap, states)` (a passed step sleeps, so its `let` keeps its
  value; a `let` outside the seq tracks).
- **A step sees the world as it stood at its entry**: every outer variable
  the seq's own text reads is sampled together when the step starts, or
  when all named inputs are present. Untaken arms count too, and an
  absent input delays reading the present ones. The snapshot does not
  change while the step runs. `select x
  > 2 { true => x, false => never() }` in a step stalls if `x` was 2 at
  entry. A step waits for a change only through `live(e)`: `until
  live(ready)`, `select live(x) > 2 { .. }`; a bare `until ready` warns.
  The rule is lexical: a function the step calls reads its own captures
  live, as they stand when the call is made, so pass what must agree as
  an argument. `live` outside a seq is
  refused.
- **A seq's value is bottom until its first run completes**, and so is
  every tuple, select or call over it: `select (polling, array::len(rows))`
  shows nothing during the first run, the one the "loading…" arm was
  written for. A level the view needs from the first frame is state with
  an initial value, written in the body: `let rows = []; seqq t { busy <-
  true; rows <- fetch(..); busy <- false }`.
- **A seq triggered on a state variable runs at init over the initial
  value.** "Save whenever `book` changes" writes the empty book over the
  file while the load is still reading it. Trigger on the event that
  means "this was changed", never on the state.
- **An async call inside a select arm loses its reply when the arm
  sleeps.** `select run { `Remove(n) => remove(n, n ~ target), _ =>
  never() }`: a second `run` of another variant while `remove` is in
  flight puts the arm to sleep and the call is aborted: the op may or may
  not have happened, and the toast and the reload never come (a call whose
  work cannot be dropped, a db op or a handshake, finishes instead, and
  is not answered either; a stream read's bytes go back to the stream). Select the EVENT, call outside the arm:
  `{ let n = select run { `Remove(n) => n, _ => never() }; remove(n, n ~
  target) }`. A call that is a seq step is safe (a step does not sleep
  mid-run).
- **A level selected out of an event dies with the arm.** `let role =
  select run { `Install(r, _) => r, _ => never() }` is bottom again once
  `run` is another variant, and a seq step that reads it then stalls
  forever. To keep the last one: `let role = never(); role <- select run
  { .. }`. Better, carry it: `seqq let a = select run { `X(a) => a, _ =>
  never() } { let r = call(a)?; toast <- "[a.name] done" }`, and never
  re-read `run` after the call to learn what was asked.
- **State never holds a value that re-fires.** A `let` keeps tracking its
  initialiser, and a struct fires when any field does, so `let order =
  [`Local(loc), `Remote(rem)]` written by a key snaps back to the initial
  order whenever anything inside `loc` or `rem` changes. Store a tag
  (`let front: [`Local, `Remote] = `Local`) and select the struct by it.
- **Comments** stand on their own line above an expression, a select
  arm, an impl or trait method, a struct-literal field, a lambda formal,
  a use-tree item or an interface item; end a list item's line after its `;` or `,` (a statement, an arm,
  a field, an element, an argument: `x: 1, // across`); or stand below a
  list's last item or at the end of a file. A type definition's struct
  fields and union members take them the same way (in a union only a
  variant or a named type); a comment anywhere else in a type is a parse
  error. A blank line between two lines of one comment block stays (one
  for a run of them). One between an operator and its operand, or after an opening
  bracket on its line, is a parse error. `///` doc comments only in
  `.gxi`.

- **Formatting:** a lone bracketed argument hugs its parent's brackets
  when its opening fits, in patterns as in expressions and types:
  `` `Tag({ `` through `})`.

## Basics

Expression-oriented; the last expression of a file or block is its value;
`;` separates statements inside blocks; blocks need two or more
elements (`{x + 1}` is a syntax error; `(x)` is grouping, not a 1-tuple).

```graphix,cases
no "at least 2 expressions" {x + 1}
```

```graphix,fragment
let x = 42;
let x: i64 = 42;
let (a, b) = (1, 2);
let {x, y} = point;
let rec f = |n| f(n);          // recursive binding (monomorphic)
let result = { let tmp = compute(); tmp + 1 }
```

Bind a name in the smallest block that uses it. A `let` at the top of a
function tells the reader the rest of the function uses it, and they
carry it to the end; one that only a single block or seq reads is noise
everywhere else. So a helper value goes inside its user (`let known = seq
{ let blank = ..; .. }`, not `let blank` above it), and an intermediate
that feeds one result lives in the block that computes the result:
`book` is a step of the seq whose value is `known`, and below that seq
there is no `book` to think about. Between two correct shapes, prefer
the one that leaves fewer names live in the function's scope.

## Types

Structural: same shape, same type.

```graphix,ignore
bool string bytes null  i8 i16 i32 i64 u8 u16 u32 u64 f32 f64 decimal
datetime duration  v32 v64 z32 z64
Array<i64>  Map<string, i64>  List<i64>   // List: [<1, 2>] literals, O(1) tail
(i64, string)                  // tuple, 2+ elements
{x: f64, y: f64}               // struct
`Tag | `Tag(i64, string)       // variants
[i64, string]                  // union; [T, null] is the option type
Error<`MyErr>  &i64  &mut i64  // error; read-only, writable reference
Result<T, `E>                  // [T, Error<`E>]; T may hold no error (NoError bound)
fn(x: i64) -> string throws `E // positional params in fn TYPES must be named
type Point = {x: f64, y: f64}; type Maybe<'a> = ['a, null]
type List2<'a> = [`Cons('a, List2<'a>), `Nil]   // recursive
'a: Number  'a: Int  'a: Float   // constraints; sets Number Int Sint Uint Float Real
```

Abstract (nominal) types: `type Counter = Abstract<i64>` as a whole
typedef body; `Counter(x)` constructs, `c.0` reads the payload, pattern
`Counter(p)` destructures — only where the definition is visible (the
defining module, or a public `Abstract<..>` in the gxi). A local definition
is visible in every instance of its defining body and nested scopes, but
not in a sibling block or sibling lambda. `T as t` is a
nominal tag test anywhere, including over a union of abstracts. One
allocation per construction: handles and newtypes, not hot data. It
crosses data (json/toml/pack reads and writers, casts, `sys::net`) only
through `impl Cast for T {};` in its defining module (a method the impl
leaves out uses the representation); without one, a value holding it is
refused as a written value or a read target.

## Literals

```graphix,ignore
42  3.14  true  false  null   u8:100  f32:3.14   // non-i64/f64 numbers are typ:value
"hello [name]!"                 // interpolation; escape \[ \] in literals
"""bare " [ ] ok, splice \[x]"""  // template: brackets are content, \[expr] interpolates
r"verbatim"  r#"has "quotes""#
[1, 2, 3]  [<1, 2>]  {"a" => 1}  (1, "two")  {x: 10, y: 20}  {x, y}
`Foo  `Bar(42)
datetime:"2020-01-01T00:00:00Z"  duration:1.0s  duration:500.ms   // units: ns us ms s m h d M y
{s with field: v}               // functional update
```

```graphix,cases
ok [duration:1.0s, duration:500.ms]
ok let x = 1; """bare " [ ] ok, splice \[x]"""
ok [r"verbatim", r#"has "quotes""#]
ok {"a" => 1}
```

## Operators (highest first)

```
* *? / /? % %?     + +? - -?     < > <= >=     == !=     &&     ||     ~ ~!
```

Unchecked `+ - *` wrap; integer `/` by zero and `MIN / -1` bottom, and
`x % -1` is 0 (only the node-walk logs those diagnostics); checked
(`+?` …) returns `[T, Error<`ArithError(string)>]`. `$` and a
handler-less `?` report fired error operands. Inside fused code the reports
can repeat or be absent: an unreached guard may report, a constant error in
an arm first entered later may never report, and native tail depths may share
one report. Use `--no-fusion` to inspect the node-walk's diagnostics.
`&&`/`||` are strict
(`false && ⊥ = ⊥`). Both operands of an operator are ONE type (cast one
side). Comparison is exact: `x == 3` over `x: [i64, null]` and `` v ==
`Red `` over `` [`Green, `Red] `` are refused — `opt::is_some`,
`opt::contains(x, 3)`, `` select v { `Red => true, _ => false } ``,
`r$ == "a"` on a Result. Two values of one union type compare by `==`,
mixed numerics included (`[i64, f64]`: `i64:3 == f64:3.0` is false), but
the orderings (`<` `<=` `>` `>=`), sorts, `min`/`max` and map keys refuse
a type holding two numeric types (`[i64, f64]`, `Number`): cast to one.
Arithmetic is `fn<'a: Number + Singleton>(x: 'a, y: 'a) -> 'a`: a type
holding two numeric types (`[i64, f64]`, `Number`) is refused even
against itself, and a generic function called with one is refused at
the call (`x$ + 1`, not `x + 1`, on a nullable; `|a: Number, b: Number|
a + b` is refused, write `'a: Number |a: 'a, b: 'a|`). A `.gxi` declares
a generic arithmetic function's `'a: Number + Singleton`: an interface
must declare every bound its implementation needs. `'a: [Number, null] +
OneNumber` is a nullable number of one numeric type. Unary `-x`, `!x`, `&x`, `*x`. Postfix `x?` (raise to the
nearest catch), `x$` (or never). Both take the errors off `x`, or, when
`x` has none, the null; they chain (`x?$`) and sit anywhere in a postfix
chain (`(k ~ sel)$.name`). Their operand's type must be known where they
are written: an untyped parameter is refused, so annotate it, a nullable
(`|t, scope: [Scope, null]| .. (t ~ scope)$`) and a Result
(`|r: Result<T, `E>| r?`) alike.
`datetime - datetime` is refused: use `sys::time::diff(later, earlier)
-> duration`, `add`/`sub` for datetime ± duration. A duration prints in
its largest whole-ish unit (`duration:1800.s` prints `30.m`).

Access: `s.f`, `t.0`, `mod::name`. `a[i]`, `a[-1]`, `a[1..3]` and
`m{"k"}` are Results (`[T, Error<`ArrayIndexError(string)>]` and the
like), so `a[1] + 1` is refused: unwrap, `a[i]$` or `a[i]?`.

```graphix,cases
no "cannot compare" let x: [i64, null] = 3; x == 3
no "cannot compare" let v: [`Green, `Red] = `Red; v == `Red
ok let a: [i64, f64] = i64:3; let b: [i64, f64] = f64:3.0; a == b
no "can't order values" let a: [i64, f64] = i64:3; let b: [i64, f64] = f64:3.0; a < b
no "more than one numeric type" |a: Number, b: Number| a + b
ok let f = 'a: Number |a: 'a, b: 'a| a + b; f(1, 2)
no "cannot compute" let x: [i64, null] = 3; x + 1
ok let x: [i64, null] = 3; x$ + 1
no "cannot compute" let a = [1, 2]; a[1] + 1
ok let a = [1, 2]; a[1]$ + 1
no "Number does not contain datetime" let d = datetime:"2020-01-01T00:00:00Z"; d - d
ok let d = datetime:"2020-01-01T00:00:00Z"; sys::time::diff(d, d)
no "not known here" |r| r?
ok |r: Result<i64, `E>| r?
```

## Functions

```graphix,fragment
let f = |x| x + 1;
let h = |x: i64, y: i64| -> i64 x + y;
let add = 'a: Number |x: 'a, y: 'a| -> 'a x + y;
let greet = |#greeting = "hello", name| "[greeting], [name]!";   // labeled args
greet(#greeting: "hi", "world");   // labeled always by name, before positionals
let f = |@args: i64| args         // variadic (builtins only)
```

A function stays generic only where a binding names it: a `let` of a
lambda or of a data literal holding lambdas, a `val`, a trait method, a `let`
of a generic name or of a field read of one. Anything else is one instance,
which its uses decide: a function a call returns, an element, a reference, a
binding something writes. A signature's variables, bounds and unions have
rules the check enforces and names: `generics.md`.

```graphix,cases
ok let f = { let n = 1; |x| x }; (f(1), f("s"))
ok let s = {f: |x| x}; (s.f(1), s.f("a"))
ok let s = {g: |x| x}; let j = s.g; (j(1), j("s"))
no "i64 does not contain string" let mk = |z| |x| x; let h = mk(0); (h(1), h("s"))
ok let mk = |z| |x| x; let a = mk(0); let b = mk(0); (a(1), b("s"))
no "i64 does not contain string" let s = {g: |x| x}; let j = s.g; let r = &mut j; (j(1), j("s"))
```

Calls resolve statically where the compiler can trace the function. A
function-variable write fires its call's current result even when it
writes the same function; the call keeps its instance and state. A HOF
nested under its own callback is a fresh instance, not recursion.

A closure keeps its captured bindings alive after their collection slot or
activation is deleted, including captures used by nested closures and labeled
defaults. The retained values remain readable and writable; the deleted
initializer does not keep running.

A labeled default returning an enclosing function's open type requires a
closed defaulted parameter type. Annotate the factory's parameter
(`|z: i64|`) or the defaulted one (`|#d: i64 = z, x|`); `|z| { let g =
|#d = z, x| (d, x); g }` and `#d: Array<'q> = [z]` are refused. A
variable the factory declares is closed: `|z: 'a| { let g = |#d: 'a = z,
y: i64| d; g(1) }` checks, and `#d = z` there takes `'a`; a type written
inside the default (`= |q: i64| -> 'a z`) names it too.

A function is more than a computation: each call is a live piece of
graph that stays. It can hold state (`let x = ..; x <- ..`), run seqs
and own effects, its arguments are streams and so is its result. What
another language builds as an object with fields and methods is here one
function from its input events to its current state: `let known =
saved_domains(#notice: &notice, dir, edit)` is the load-once seq, the
edit that replaces the value and the save to disk, and none of their
names reach the caller. The convention: the caller owns status (a
notice, a busy flag, a toast) and lends it by `&`; events come in as
arguments (`edit`, `recheck`, a key); the function returns the data. One
that produces a change returns it for the caller to connect (`edit <-
discovered(#toast: &toast, known, at)`), so the state's writers are all
visible in one place. A function that must return something else (a
modal returns its layers) answers through a callback the caller turns
into its own event: `ca_question(#on_answer: |b| { run <- `UninstallWith(b);
null }, ask)`. An event argument opens it (`up <- ask ~ true; sel <- ask ~
0` inside), so the caller writes one event where it wrote an open flag, a
cursor reset and a payload. When a function's body has grown sections,
each section with its own lets is a candidate.

No currying: a call supplies every required argument, and `add(1)` on a
two-argument `add` is refused (`missing required argument`), not a
partial application. A combinator takes everything it needs and returns
the value, `on_press(e, f) -> [`Stop, `Continue]`, never a function to
be applied later: `|e: Event| on_press(e, f)` at the use site is the
partial application, written out.

```graphix,cases
no "missing required argument" let add = |a: i64, b: i64| a + b; add(1)
```

## Annotations

Do not write a type annotation unless the checker asks for one. It
asks for two things. A parameter whose fields you read and whose type
no call fixes: a top-level function, a handler handed to a widget
(`|e: Event|`). A callback passed to `array::map`/`find`/`fold` over a
typed array needs none. And a `let` whose initial value is narrower
than its writers: `let notice: [string, null] = null`, `let go: [i64,
string] = never()` with writers of both (the check names the union to
declare), or `let go: Any = never()` for a trigger whose value nobody
reads. A fold's accumulator is the type of its init, so
`array::fold(xs, null, |acc, x| ..)` makes `acc` null: annotate the init
(`let init: [i64, null] = null`), never the callback. A `let` over a
call, a select or a seq needs nothing. A lambda's return type is needed
only where it must be wider than its body's: a `.gxi`'s `val f: fn(x:
i64) -> [i64, null]` must match its implementation exactly, so `let f =
|x| x + 1` is refused and `|x| -> [i64, null] x + 1` checks. An
inferred return crosses modules, and a caller reads a field of a generic
function's result (`h(1).v` over `|x: 'a| {v: x}`) with no `-> T`. To
find the set: strip, `--check`, restore what it names.

```graphix,cases
no "type mismatch fn(acc" array::fold([1], null, |acc, x| x)
ok let init: [i64, null] = null; array::fold([1], init, |acc, x| x)
ok let h = |x: 'a| {v: x}; h(1).v
```

A generic function's variable that only data holds takes the widest
argument, in either order; an argument still undecided (a `str::parse`, a
`let` over `never()`, `[]`) takes what the others decide, or is refused,
"annotate it": annotate the read. `generics.md` has the rest.

```graphix,cases
ok let s = "1"; opt::or_default(str::parse(s)$, 0)
no "undecided at this" let f = |x: 'a, y: 'a| -> 'a y; f(str::parse("1"), 1)
```

## Select

The only branching construct. Arms are consulted top down, structure
first, guard second; must be exhaustive with no dead arms; a bottom
scrutinee yields a bottom select (put `hold` on the scrutinee to persist
across a bottom cycle).

- An arm first entered with a new function instance or collection slot
  reads standing values as fired, like the rest of that born subtree.
- An unselected arm with state, an effect or a raise sleeps. Sleep is a
  pause and a wake a resume: `count`, `once`, `take`, `skip`, `uniq`,
  `hold` and queues keep their state; an async call in flight is
  aborted, and may or may not have happened, unless its work cannot be
  dropped (a stream read or write, a db op, a tls handshake, a file
  seek), which finishes unanswered (a read's bytes return to the stream);
  nothing answers a call after its arm's sleep; none is made again
  unless an argument fires (its last result stays); a timer's run
  resumes; a net subscription or publication is torn down and
  re-attached at the wake; a seq's run is aborted (a `seqq`'s queued
  requests stay and run). Only the inputs that fired
  while the arm slept are delivered at the wake, once each. A consulted
  guard and an arm body share those missed fires: the first arm to read
  one spends it for the whole select, and an arm's guard and its own body
  both see it.
- A written type test narrows its binds to the values reaching it:
  `Number as n` over `[i64, string]` gives `n: i64`; `Array<Number> as ns`
  over `Array<[i64, string]>` gives `ns: Array<i64>`.
- A pattern that tests inside a declared type variable needs a written
  type test (`{a: i64, b: string} as {a, ..}`), and one over a type not yet
  decided at the select is refused until the binding it names is annotated;
  no arm may tell apart two types that share a runtime form (two references,
  two function types). The check names the fix; the rules are in `select.md`.
- References, functions and a type holding two numeric types cannot
  be ordered, sorted or used as map keys; `_` in a type is bottom, not "infer" (`Error<_>` is `Error<⊥>`).

```graphix,fragment
select x {
  i64 as n => n + 1,              // type test
  (f64, f64) as (n, a) => n / a,  // the WHOLE type comes before the structure pattern;
                                  // `(f64 as n, f64 as a)` is a parse error
  `Apple | `Pear => "fruit",      // or-pattern; captures type as the union
                                  // an or-arm narrows what follows as separate arms
                                  // would: after `A(m) | `B(m) => .., a catch-all
                                  // `x => f(x)` has neither A nor B in x's type.
  `Other(name) if name != "" => name,   // a test on a capture is a GUARD, never a
                                         // nested `select cond { true => .., false => .. }`
  error as e => recover(e),  v => v,     // splits a Result; v is the bare success type
  (0, y) => y,
  {x: 0, y} => y,  {x, ..} => x,  // partial struct; several union members: `S as {x, ..}`
  [] => 0,  [a, b] => a + b,  [h, rest..] => h,  // length ladders count as coverage
  [<>] => 0,  [<h, t..>] => h,   // list patterns; suffix form refused
  v@ `Some(inner) => use_both(v, inner),
  _ => default
}
```

```graphix,cases
ok let x: [i64, string] = 1; select x { Number as n => n + 1, s => 0 }
no "a `,` separates" let x = (1.0, 2.0); select x { (f64 as n, f64 as a) => 1 }
ok let v: [`A(i64), `B(i64), `C] = `C; select v { `A(m) | `B(m) => m, _ => 0 }
no "unreachable arm" let b = true; let r = select b { true => never(), false => error(`E) }; select r { error as e => 1, v => 2 }
ok let b = true; let r: [i64, Error<`E>] = select b { true => never(), false => error(`E) }; select r { error as e => 1, v => 2 }
no "unreachable arm" |x| select x { null as _ => 0, v => v }
```

## Connect

`x <- e` is the only way to make a cycle; the write lands next cycle.
It is an expression form, not an operator: `x <- clock ~ x + 1` needs no
parens. A `<-` writes when its right side fires. A pure arm has nothing
to pause; an arm with a `<-` sleeps when unselected.

## Sequencing: `seq` and `seqq`

For multi-step ceremonies (a request, then a reply, then a write).
Straight-line only; no `if`/loops inside.

```graphix,fragment
// seqq queues triggers; seq drops while busy
let r = seqq request {
  let cfg = load(request);         // a call is issued once per entry over a snapshot of its args
  until live(ready);               // wait for a bool to be true; live: steps see their entry snapshot (never the last statement where its value is used)
  { a <- fetch(cfg); b <- probe(cfg) };  // a block: statements issued together, lets local
  let v = try { risky(cfg)? } with(e) { println("fallback"); 0 };   // the error branch; `catch` is refused in a seq
  write(v)                          // the block's value; each output releases the next queued request
}
```

A step snapshots all variables its text names together, including untaken
select arms and uncalled lambdas. If any is absent, the whole step waits:
present variables are read only when the last absent input arrives.

A step whose value never fires stalls the run, and so does a `d$` in a step
over a null; a stalled `seqq` queues every later trigger forever. Read
`seq.md` before writing one: triggers, `abort`, `flush`, what a step waits for.

```graphix,cases
no "means nothing outside a seq" let x = 1; live(x)
no "catch is not allowed inside a seq" let t = 1; seq t { catch(e) 1; 2 }
```

## Errors

```graphix,fragment
error(`NotFound("missing"))?;    // raise to the nearest catch
{ catch(e) msg <- "[e.0.error]"; risky()?; more()? }   // catch INSTALLS a handler for the rest of its block
```

The binder of `catch(e)` and `with(e)` is `Error<ErrChain<E>>`, not the
raised `E`: the raised value is `e.0.error` (`e.0` also holds the site,
`pos` and `ori`, and the `cause`). Match on that, `` select e.0.error {
`NotFound(s) => .. } ``; `` select e { `NotFound(s) => .. } `` is refused
as never matching.

```graphix,cases
no "will never match" { catch(e) select e { `NotFound(s) => 1, _ => 2 }; error(`NotFound("m"))? }
ok { catch(e) select e.0.error { `NotFound(s) => 1 }; error(`NotFound("m"))? }
```

`catch` is not control flow: the handler is a reactive expression.
Read its binder to drive it on errors: `catch(e) failed <- e ~ true`.
A handler that never reads its binder can act at birth, before any error,
and the checker warns. Connect the handler to state you read. A second
catch shadows the first below it; a handler's own `?` rethrows outward.
Inside `seq`, use `try … with`.

`x$` is "or never": the success value, or bottom on an error. It is the
one-token spelling of `select x { error as _ => never(), v => v }` and
of `{ catch(e) never(); x? }` — never write either of those. A dropped
error can be logged at warn with its site (`RUST_LOG=warn` and
`--log-dir` in the shell); the fusion limits in Operators above apply. Use `$` when the failure is an expected
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

`$` gates only while `x` is null or an error: a call or struct taking
`x$` does not fire then, but it fires when `x` returns, since a call
fires when any argument fires. `f(k ~ t, sel$)` re-delivers its last
key sample at every arrival of `sel`. Unwrapping and then sampling
banks: `k ~ sel$` (or `let v = sel$; k ~ v`, the let is only a name)
pays a key pressed before the first selection when the selection
arrives (a phantom confirm dialog when the rows load). A nullable that
an event samples is sampled first and unwrapped after, `(k ~ sel)$`,
which samples the null and drops it; a call takes that value, not the
key and `sel$` apart.

An annotated `catch(e: T)` checks each raised type against `T` where it
joins the catch, including callback throws. A declared error parameter stays
rigid: a function taking `Result<i64, 'e>` and catching only
``Error<ErrChain<`A>>`` must declare ``'e: `A``.

## Field access

A union of structs allows `.field` when every member has that name, even at
different sorted positions; a union of tuples allows `.N` when every member
has that position, even at different arities. A read unions the member field
types; a functional struct update must fit every member's field type.

## References and places

Read-only reference arguments `&'a` infer the widest referent across arguments,
in either order; writable `&mut 'a` arguments require exactly the same referent.

`&v`, `*r`; writing is `*r <- new` and needs `r: &mut T`, made with
`&mut v`. `&T` is read-only and covariant (`&[i64, null]` takes `&x`
with `x: i64`, and takes a `&mut`); `&mut T` is invariant. An optional
writable argument is `[&mut T, null]`, never `&mut [T, null]`. Widgets
take `&` parameters so updates propagate without rebuilding the tree. `&a[i]`, `&s.f`, `&t.0`, `&m{k}`
are places: reading follows the path, writing patches the root at
delivery; a dynamic key (`&vals[focus]`) is a moving reference. Reading a union of references returns the union of the referent
types: `*r` over `[&mut i64, &mut f64]` is `[i64, f64]`. Writing through
one must fit every referent and requires every member to be writable.

Reference equality follows the named place even under `Any`, including nested
collections and abstract payloads; an abstract's own `Eq` keeps control.
Ordering references hidden under `Any` logs a failure and bottoms, including
singleton sorts, dedup, min/max and map-key operations. A map cast returns an
error for such a key. An abstract's own `Ord` controls its ordering.

## Modules, interfaces, traits

Rust-2018-style imports: `use array::map;` `use str::join as sjoin;`
`use super::{f, T};` `use package::a::b;` `mod name;`, and groups nest.
Package names are path roots: `array::map(xs, f)` works bare. A
submodule sees nothing of its parent implicitly. Declarations are
statement-position only.

One `use` per root, nested, and name what you use. No globs: `*` pulls
in names the reader cannot see arriving, and `{self, *}` on a widget
module is a habit, not a need (`block::block` imports the function; add
`self` only when the file writes `overlay::X`).

Interfaces, traits and how a `.gxi` splices into its body: `modules.md`.

## Standard library, briefly

Core (always in scope): `print println dbg log cast<T>(x) error is_err
filter filter_err count once uniq sum product min max mean and or all
queue queuefn hold take skip throttle range`. `filter` and `filter_err` gate a
stream; `is_err` tests (`false` on every non-error value); to branch on a
Result, select on it. `never()` / `never<T>()` is SYNTAX: typed bottom,
args stay live; an unannotated `let` over `never()` takes its type from
its first use, a reader or a writer, and a later writer must fit it.

`opt` (over `['a, null]`): `is_some is_none or_default or and map
flat_map filter ok_or zip`. Unwrapping is the `$` operator, above. The
ladder it replaces also hides in two steps: `let p = select d { null as
_ => null, v => f(v) }; let q = p$` is `let q = f(d$)`. A null carried
forward only to be dropped is unwrapped at the source, once: `let dir =
d$`, and every reader (a later seq step included) takes `dir` (unless
an event samples it: see the banking under `$` gates).

The packages, `queuefn`, RPC calls and line readers: `stdlib.md`.

```graphix,cases
ok range(0, 3)
no "sync variadic" str::concat()
```

## Idioms

```graphix,fragment
let clock = sys::time::timer(duration:1.s, true);
let count = 0;
count <- clock ~ count + 1;                       // driven counter
data <- array::window(#n: 60, v ~ data, cast<f64>(v)?);  // sliding window
select x { n if n < limit => x <- x + 1, _ => never() };  // state that stops
// first element passing a test: filter_map, then an emptiness select —
// never map to options and find the non-null one
select array::filter_map(xs, |x| select probe(x) { error as _ => null, v if ok(v) => `Keep(x), _ => null }) {
  [] => `Absent,
  [a, ..] => `Present(a)
};
let result = seqq go { let r = fetch(go); publish(path, r); r }   // ceremony
```

State loaded once is a triggerless `seq` whose value is the state, and
the state's absence is the sequencing: every reader (an edit's
`upsert(known, ..)` in a seq step) waits for the load, with no flag.

```graphix,ignore
let known: Book = seq {
  let blank: Book = { domains: [] };
  let book = select sys::fs::is_file(path) {
    error as _ => blank,
    file => seq { try { let b: Book = json::read(sys::fs::read_all(file)?)?; b } with(e) { notice <- ..; blank } }
  };
  seed(book, local_domain(true))
};
known <- edit
```

A guard cannot bind (`_ if !is_err(is_file(p)) => .. file ..` has no
`file`): select on the Result. Reads, casts and printing: `data.md`.

Derive, do not add a second writer. Two unordered writers to one
variable race when one replaces it wholesale (a file load landing after
a seed erases the seed). State holds only what is written; the merged
view is a pure select over its sources:

```graphix,fragment
let known = select local_domain(true) { null as _ => book, own => upsert(book, own) }
```

A mapped array zipped back against its source means the callback threw
away the element it had in hand. Build the final row there:
`array::filter_map(xs, |x| select probe(x) { [] => null, [a, ..] => `Keep({x, a}) })`.

## Gotchas

- Union types use `[]`: `[i64, null]` is "i64 or null", not an array.
- Primitive type names are legal binding and field names; control
  keywords and literal words are not (`{type: v}` written explicitly);
  `bytes` is field-only.
- Reserved words in a name position are reported by the parser ("`null`
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
- A `type` a block declares is that type throughout the block, above its
  declaration too, never an outer type of the name: in `type L = i64; {
  let f = |x: i64| { let b: L = 3; b }; type L = string; f(1) }` the `L` in
  `f` is the string, and so is an abstract `W(v)` or pattern `W(p)` there.
  A statement above the declaration that needs the type at once (`let a =
  W(1)`, a cast) is refused. Declare a block's types first.
- Each REPL statement, a script and a dynamic module's load is checked
  alone, and what it leaves undecided is `_` from its end: after `let e
  = []`, a later `e <- [1]` is refused (write `let e: Array<i64> = []`),
  and a later function writing `e` takes only `_`. Lambdas that meet in
  one array or select settle the same way (`let fs = [|x| x, |y| y]`
  takes no later `fs[0]$(1)`: annotate `fs`). A generic function it
  leaves in a binding (`let j = s.g`) stays generic: each later use
  copies it, `j(1)` and `app(j)` alike.
- `--check` a witness before arguing about a semantics question.

```graphix,cases
ok {type: 1}
no "declare it before its first use" { let a = W(1); type W = Abstract<i64>; a }
ok let e = []; e <- [1]; e
ok let f = |x: i64| #[native] x + 1; 1
```
