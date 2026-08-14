# Recursion

Functions can be recursive. A function whose every recursive call is in tail
position executes as a loop — constant stack, any depth. Non-tail recursion
nests, and is bounded by a call-depth limit (256 by default, settable by the
embedder): a call that exceeds the limit produces no value — the whole call
bottoms at the root, and an error is logged, since hitting the limit usually
means the program did something unintended.

If you rely on a function being tail-recursive, assert it:

```graphix
#[tail_recursive]
let rec sum_to = |n: i64, acc: i64| -> i64 select n {
  0 => acc,
  _ => sum_to(n - 1, acc + n)
}
```

`#[tail_recursive]` is a compile-time check: if any recursive call is not in
tail position (or the function does not recurse at all, or recurses mutually),
the program does not compile. A function that passes the check cannot hit the
call-depth limit through its own recursion.

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
