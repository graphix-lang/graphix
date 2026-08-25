# Recursion

Functions can be recursive. Every call creates an *activation*: the
function's body, live, with its own state — its subscriptions, its
timers, its `count`s, its own bindings. A recursive call in tail
position whose body holds no such state runs as a loop instead, one
activation reused for every iteration, constant stack, any depth —
nothing could tell the difference. A body that does hold state (an
async operation, a stateful builtin such as `count` or `uniq`, a `<-`
to one of its own bindings) keeps an activation per iteration whatever
the call position: each iteration owns its state, exactly as each
element owns its callback in `array::map`.

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
past the new end sleep.

Depth is bounded by memory, not by a limit: non-tail recursion nests on
heap-allocated stack segments, in the interpreter and in compiled code
alike. An embedder may cap it with a stack budget
(`graphix_compiler::set_stack_budget`, or `GRAPHIX_STACK_BUDGET` in
bytes); a program that exceeds the budget is stopped the way Ctrl-C
stops it.

If you rely on a function running as a constant-space loop, assert it:

```graphix
#[tail_recursive]
let rec sum_to = |n: i64, acc: i64| -> i64 select n {
  0 => acc,
  _ => sum_to(n - 1, acc + n)
}
```

`#[tail_recursive]` is a compile-time check: if any recursive call is
not in tail position, if the body is stateful or async, if the function
does not recurse at all, or if it recurses mutually, the program does
not compile.

Two related assertions document a function's timing: `#[sync]` asserts that
every output appears on the same cycle as its trigger (the body reaches no
timer, IO, or other async operation), and `#[async]` asserts the opposite.
Like `#[tail_recursive]` they never change how the function compiles — they
only fail the compile when the inference disagrees with your expectation.

With that out of the way, lets
write a recursive function to add up pairs of numbers in an array,

```graphix
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
