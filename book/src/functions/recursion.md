# Recursion

Functions can be recursive. Every call creates an *activation*: the
function's body, live, with its own state — its subscriptions, its
timers, its `count`s, its own bindings. Each iteration of a recursion
owns its state, exactly as each element owns its callback in
`array::map`.

A recursive call in tail position whose body holds no state (no async
operation, no stateful builtin such as `count` or `uniq`, no `<-` to one
of its own bindings) and compiles to native code runs as a native loop
instead: constant stack, any depth, answering exactly what the
activations would. A body that does not compile keeps an activation per
call, which costs memory and time for every call: one per level for a
linear recursion, one per call in the whole call tree for a branching
one like a naive `fib`. The things that
keep a body out of native code are reading a reference (`*r`), a `?`
raising to a handler, a `<-` to a variable outside the function, and a
builtin without a fast call; move them out of the loop when the loop is
deep.

`str::parse`, `hbs::render`, the json/toml/pack writers, `array::sort`,
`list::sort`, `array::dedup` and every `re::*` builtin run in the node-walk.
They keep their last result while their own arguments are stale; a fused
fast call can run again whenever any input of its kernel fires. Bind these
operations outside a `#[native]` expression and pass their results in.

That is what makes recursion the way to write a reactive traversal:

```graphix
let rec publish_all = |paths: Array<string>, i: i64| select paths {
  [] => null,
  [p, rest..] => {
    sys::net::publish(p, i)$;
    publish_all(rest, i + 1)
  }
}
```

Every path gets its own publish site, retained across cycles. When the
array changes, the activations are fed again by position, and those
past the new end are deleted, their publishes withdrawn; a later regrow
builds fresh ones.

Recursion is bounded by memory, not by a limit: non-tail recursion nests on
heap-allocated stack segments, in the interpreter and in compiled code
alike. An embedder may cap it with a stack budget
(`graphix_compiler::set_stack_budget`, or `GRAPHIX_STACK_BUDGET` in
bytes or with a `K`, `M` or `G` suffix, as in `64M`), and with an
activation budget, the live activations (calls, collection slots)
a runtime may hold (`Control::set_activation_budget`, or
`GRAPHIX_ACTIVATION_BUDGET`); unlimited by default. A program that
exceeds a budget is stopped the way Ctrl-C stops it.

If you rely on a function running as a constant-space loop, assert it:

```graphix
#[tail_recursive]
let rec sum_to = |n: i64, acc: i64| -> i64 select n {
  0 => acc,
  _ => sum_to(n - 1, acc + n)
};
let total = #[native] sum_to(1000000, 0)
```

`#[tail_recursive]` asserts the shape and `#[native]` that the call
compiles to native code. `#[tail_recursive]` is a compile-time check: if any recursive call is
not in tail position, if the body is stateful or async, if the body
holds memory of its own per call (a collection operation, a call to a
function that recurses other than by tail calls to itself, or a call to
a function that holds one, which each activation keeps apart), if the
function does not recurse at all, or if it recurses mutually, the
program does not compile. Only compiled code runs the loop: the
interpreter keeps an activation per call, so the assertion also needs
every call of the function to compile to native code. A body that does
not (it takes a reference, say) fails the compile, and so does one call
whose callback does not, even beside a call that does. A call through a
function value chosen at runtime is made by the interpreter, so it
fails too; a function the value holds may call the recursive one
directly, and that call compiles. With `--no-fusion` that last part is
not checked, nor is `#[native]`; the rest still fails the compile.

Two related assertions document a function's timing: `#[sync]` asserts that
every output appears on the same cycle as its trigger (the body reaches no
timer, IO, or other async operation), and `#[async]` asserts the opposite.
`#[pure]` asserts more than `#[sync]`: the function is a pure function of its
arguments, with no state, no effect such as `println`, and no raise to a
handler; `dbg` is allowed, with a warning. Like `#[tail_recursive]` they never change how the function compiles — they
only fail the compile when the compiler disagrees with your expectation.

The check needs a call to look at. A function called only through a
value chosen at runtime (`let g = select b { true => f, false => h };
g(x)`) is checked when that call first reaches it, a
`#[tail_recursive]` one at every such call; the program is already
running then, so a failure is reported the way an unhandled error is, on
stderr and in the log. A later input to the REPL checks the calls it
makes as any compile does.

With that out of the way, lets
write a recursive function to add up pairs of numbers in an array,

```graphix,repl
let rec add_pairs = 'a: Number |a: Array<'a>| -> Array<'a> select a {
  [e0, e1, tl..] => array::push_front(add_pairs(tl), e0 + e1),
  a => a
}
```

running this we see,

```graphix
〉add_pairs([1, 2, 3, 4, 5])
-: Array<'a: i64>
[3, 7, 5]
```
