# Detailed Semantics

Considering the underlying execution model functions might be better described
as "polymorphic graph templates", in that they allow you to specify a part of
the graph once, and then use it multiple times with different types each time.
Most of the time this difference in semantics doesn't matter. Most of the time.
Consider,

```graphix
let f = |x, y| x + y + y;
let n = cast<i64>(net::subscribe("/hev/stats/power")?)?;
f(n, 1)
```

What happens here? Does `f` get "called" every time `n` updates? Does it only
work for the first `n`? Does it explode? Lets transform it like the compiler
would in order to understand it better,

```graphix
let f = |x, y| x + y + y;
let n = cast<i64>(net::subscribe("/hev/stats/power")?)?;
n + 1 + 1
```

The "arguments" to the function call were plugged into the holes in the graph
template and then the whole template is copied to the call site, and from then
on the graph runs as normal.

So when `n` updates, the call site will return `n` + 2, since `1`
never updates we don't have to worry about it, however this same flow
applies when multiple arguments could update. In this case we're just
having a philosophical discussion about how call sites are
implemented, however it DOES actually matter sometimes.

## Where Function Semantics Matter

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

Here we have a function that takes an array with any element type and
returns it's length. Brilliant, lets call it,

```graphix
let a = [1, 2, 3, 4, 5];
len(a)
```

and when we run this we get,

```
$ graphix test.gx
5
```

That's the right answer. Are we done? Noooooooo. No we are not done. Lets see
what happens if we do,

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

What!? That's not even wrong. That's just nonsense, what happened? The key to
understanding this problem is that there is just one call site, which means we
instantiated this little reusable bit of graph one time, just one time. That
means there is just one `sum`, one `a`, basically just one graph. When we use
connect to iterate we are using graph traversal cycles to do a new element of
the array every cycle until we are done. It will take 5 cycles for the first
array to be done, and that's the problem, because we update `a` with a whole new
array in cycle 1 and again in cycle 2. That's why we get 4, it's deterministic,
we will get 4 every time.

- the first cycle we add 1 to `sum` and set the inner `a` to `tl` (it's not the
  same variable as the outer `a`, which is why the chaos isn't even greater). But
  the outer `a` also gets set to `[1, 2, 3]` and that overwrites the inner set
  because it happens after it (because that's just the way the runtime works).
- the second cycle we add 1 to `sum` and set the inner `a` to `[2, 3]` and the
  outer `a` to `[1, 2]`
- the third cycle we add 1 to `sum` and set the inner `a` to `[2]`
- the 4th cycle we add 1 to `sum` and set `a` to `[]`
- the 5th cycle we update our return value with `sum`, which is now 4

We can only fix this be understanding that we're programming a graph. I tried to
make Graphix as much like a normal language as possible, but this is where we
depart from that possibility. The general idea is, we need to queue updates to
the outer `a` until we're done processing the current one. For that we have a
builtin called `queue`, here is the correct implementation

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

Every time `clock` updates `queue` will output something it has queued, or if it
has nothing queued it will store that the next thing that arrives can go out
immediatly. So the first `a` will immediatly pass through the queue, but
anything after that will be held. Then the normal select loop will run, except
it will look at `q` instead of `a` now, so that `a` can update without
disturbing it. When we get to the terminating case, we update for next cycle
`clock` with `null` and `sum` with 0 and we return `once(sum)`. We return
`once(sum)` instead of just `sum` because removing something from the queue
takes one cycle, so it will be two cycles before we start on the next array, and
in the mean time the existing array will still be empty, meaning the second
select arm will still be selected, and `sum` is updating to 0 which we do not
want to return. If we run this with the same set of examples we will get the
correct answer,

```
$ graphix cycle_iter.gx
5
3
2
```

This comes up other places as well, for example whenever we have to
deal with something that does IO, like calling an RPC, subscribing to
values in netidx, etc. 

