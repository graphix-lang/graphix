# Detailed Semantics

In the chapter on connect (<-) we introduced the idea that Graphix
executes code in cycles. In this chapter we will really dive into this
concept in order to understand all the implications. By the time we
finish we will be able to write code that does exactly what we want it
to do, and we'll understand when connect is appropriate and where it
should not be used.

## The Function Execution Model

A function call site becomes a graph at compile time, the function's
arguments are connected to the arguments passed in at the call site,
and it's output is connected to the call site itself. All the local
variables are unique and invisible to the outside, and the types are
resolved at compile time for each call site. Consider,


```graphix
let f = |x, y| x + y + y;
let n = cast<i64>(sys::net::subscribe("/hev/stats/power")$)$;
f(n, 1)
```

Here there is one call site for `f`, this builds a graph at the call
site that connects `n` to argument `x`, `1` to argument `y`, and the
output of `f` to the output of the program. Whenever
`"/hev/stats/power"` updates, `n` updates which causes `x` to update
which causes `x + y + y` to update which causes the call site to
return `x + y + y`, which causes the program to print `x + y + y`

Lets transform this program into something closer to the actual graph
that is executed,

```graphix
let n = cast<i64>(sys::net::subscribe("/hev/stats/power")?)?;
n + 1 + 1
```

Here we've essentially inlined out `f` so that we can see the
execution flow of the graph, these two programs output will be
identical.

## Functions, Cycles, and Connect

Lets revisit an earlier example where we used select and connect to find the
length of an array. Suppose we want to generalize that into a function,

```graphix
let len = |a: Array<'a>| {
  let sum = 0;
  select a {
    [x, tl..] => {
      sum <- sum + 1;
      a <- tl
    },
    _ => sum
  }
}
```

Now this is a very contrived example, meant to illustrate the
semantics of connect when combined with functions, normally you'd use
a sequential iterator like `array::fold` for a job like
this. Nevertheless, lets carry on

```graphix
let a = [1, 2, 3, 4, 5];
len(a)
```

and when we run this we get,

```
$ graphix test.gx
5
```

However if we do,

```graphix
let a = [1, 2, 3, 4, 5];
a <- [1, 2, 3];
a <- [1, 2];
len(a)
```

this results in,

```
$ graphix test.gx
4
```

This happens because connect (<-) operates across multiple cycles,
each connect schedules an update for the next cycle, and because we've
used it to iterate, we've created an iteration that takes multiple
cycles to complete. However since the argument to len is also updated
for the next two cycles, this results in an iteration that is
interrupted with a new `a` argument before it can complete. A detailed
breakdown of what happens is as follows,

- the first cycle we add 1 to `sum` and set the inner `a` to `tl` (it's not the
  same variable as the outer `a`, which is why the chaos isn't even greater). But
  the outer `a` also gets set to `[1, 2, 3]` and that overwrites the inner set
  because it happens after it (because that's just the way the runtime works).
- the second cycle we add 1 to `sum` and set the inner `a` to `[2, 3]` and the
  outer `a` to `[1, 2]`
- the third cycle we add 1 to `sum` and set the inner `a` to `[2]`
- the 4th cycle we add 1 to `sum` and set `a` to `[]`
- the 5th cycle we update our return value with `sum`, which is now 4

## Synchronous Iterators, the Right Way

As mentioned above `len` is a contrived example, the right way to get
the length of an array is to call `array::len`, and the right way to
compute something like the length, or sum, etc over an array is to use
a synchronous iterator such as `array::fold` (or write a built-in in
rust if you require high performance). Synchronous iterators compute
the entire result in one cycle.

```graphix
let len = |a: Array<'a>| array::fold(a, 0, |acc, x| x ~ acc + 1);
let a = [1, 2, 3, 4, 5];
a <- [1, 2, 3, 4, 5];
a <- [1, 2];
len(a)
```

This will output

```
$ graphix test.gx
5
3
2
```

## Advanced Cycle Programming

Synchronous iterators aside, sometimes, such as when controlling IO
devices, you want to use the cycle semantics to achieve a particular
semantics. For these cases there is the `queue` function (and
friends), which allows you to control how updates to a variable are
processed.

```graphix
val queue: fn(#clock: Any, 'a) -> 'a
```

Every time clock updates queue allows a 'a through. If no 'a is
queued, it still remembers to allow that many through when they
arrive. 

Lets use it to write two different subscription functions with
different but equally valid and useful semantics.

```graphix
let f = |path| sys::net::subscribe(path)$;
let path = "/local/baz0";
path <- "/local/baz1";
path <- "/local/baz2";
f(path)
```

Now suppose we have published

| path        | value |
+-------------+-------+
| /local/baz0 | "baz0 |
| /local/baz1 | "baz1 |
| /local/baz2 | "baz2 |

This program will always return "baz2"

```
$ graphix text.gx
"baz2"
```

This is because every time the argument to `sys::net::subscribe` updates it
drops the previous subscription and starts a new one. This is useful,
for example, if the user is typing this path into a UI element, they
probably only care about the most recent one. 

Moreover, if one of these paths doesn't exist, or the publisher is
dead, we may not want to wait for that dead path before moving on to
the next one. 

Now suppose you want to subscribe to all the paths one at a time in
order, and you want to wait for each one to return a value before
moving on to the next one. We can use queue to achieve this.

```graphix
let f = |path| {
  let clock = "";
  let path = queue(#clock, path);
  let res = sys::net::subscribe(path)$;
  clock <- uniq(res ~ path);
  res
};
let path = "/local/baz0";
path <- "/local/baz1";
path <- "/local/baz2";
f(path)
```

This will sequence the subscriptions and result in,

```
$ test.gx
"baz0"
"baz1"
"baz2"
```

### Fixing Our Contrived Len

Advice about not using <- in iteration aside, if you understand cycles
then you can do it if you wish, and maybe in advanced examples there
is even a reason to. Lets fix our `len` function to be cycle aware and
work in any situation using `queue`.

```graphix
let len = |a: Array<'a>| {
  let clock = once(null);
  let q = queue(#clock, a);
  let sum = 0;
  select q {
    [x, tl..] => {
      sum <- sum + 1;
      q <- tl
    },
    _ => {
      clock <- null;
      sum <- 0;
      once(sum)
    }
  }
}
```

Now we can see our very verbose and inefficient `len` is now correct

```
$ graphix cycle_iter.gx
5
3
2
```
