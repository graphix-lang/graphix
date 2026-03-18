# Connect

Connect, written `x <- expr` is where things get interesting in Graphix. The
sharp eyed may have noticed that up until now there was no way to introduce a
cycle in the graph. Connect is the only graph operator in Graphix, it allows you
to connect one part of the graph to another by name, causing the output of the
right side to flow to the name on the left side. Consider,

```graphix
let x = "off"
x <- sys::time::timer(duration:1.0s, false) ~ "on"
print(x)
```

This program will first print `off`, and after 1 second it will print `on`. Note
the `~` operator means, when the expression on the left updates return the
current value of the expression on the right (called the sample operator). The
graph we created looks like,

```
const("off") ────────────────> "x" ──────> print
                                 ▲
                                 │
                                 │
           sys::time::timer ──> sample
                            ▲
                            │
                            │
                      const("on")
```

We can also build an infinite loop with connect. This won't crash the program,
and it won't stop other parts of the program from being evaluated,

```graphix
let x = 0;
x <- x + 1;
print(x)
```

This program will print all the i64s from 0 to MAX and then will wrap around. It
will print numbers forever. You might notice, and you might wonder, why does it
start from zero, shouldn't it start from 1? After all we increment x BEFORE the
print right? Well, no, not actually, it will start at 0, for the same reason
this infinite loop won't lock up the program or cause other expressions not to
be evaluated. Graphix programs are evaluated in cycles, a batch of updates from
the network, timers, and other IO is processed into a set of all events that
happened "now", then the parts of the program that care about those particular
events are evaluated, and then the main loop goes back to waiting for events.

What connect does is it schedules an update to `x` for the next cycle, the
current cycle proceeds as normal to it's conclusion as if the connect didn't
happen yet, because it didn't. In the above case the event loop would never
wait, because there is always work to do adding 1 to `x`, however it will still
check for other events every cycle.

When combined with other operations, specifically select, connect becomes a
powerful general looping construct, and is the only way to write a loop in
Graphix. A quick example,

```graphix
let count = {
  let x = 0;
  select x {
    n if n < 10 => x <- x + 1,
    _ => never() // never() never updates
  };
  x
};
count
```

This program creates a bind `count` that will update with the values 0 to 10. If
you put it in a file `test.gx` and execute it using `graphix test.gx` it will
print 0 to 10 and then wait.

```
$ graphix test.gx
0
1
2
3
4
5
6
7
8
9
10
```

### Is Connect Mutation?

Connect causes let bound names to update, so it's kind of mutation. Kind of. A
better way to think about it is that every let bound value is a pipe with
multiple producers and multiple consumers. Connect adds a new producer to the
pipe. The values being produced are immutable, an array `[1, 2, 3]` will always
and forever be `[1, 2, 3]`, but a new array `[1, 2, 3, 4]` might be pushed into
the same pipe `[1, 2, 3]` came from, and that might make it appear that the
array changed. The difference is, if you captured the original `[1, 2, 3]` and
put it somewhere, a new `[1, 2, 3, 4]` arriving on the pipe can't change the
original array.
