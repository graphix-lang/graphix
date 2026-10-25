# Blocks

A block is a group of code between `{` and `}` that has it's own scope, and
evaluates to the last value in the block. Expressions in a block are `;`
separated, meaning every expression except the last one must end in a `;` and
it is illegal for a block to have just one expression (it will not parse).

You can use blocks to hide intermediate variables from outer scopes, and to
group code together in a logical way.

```graphix,refused
let important_thing = {
  let x = 0;
  let y = x + 1;
  43 - y
};

// compile error, x isn't in scope
x;
// compile error, y isn't in scope
y;
important_thing
```

This program won't compile because you can't reference y and x from
outside the block scope. Blocks are valid anywhere an expression is
valid, and they are just expressions. They will become very important
when we introduce lambda expressions.

A block whose last statement ends in `;` has no value: it never produces,
and its type is bottom, as `never()`'s is. That is the form for a function
called only for its effects, whose result nothing reads:

```graphix
let report = |x: i64| {
  println("got [x]");
  println("twice is [x * 2]");
};
report(21)
```
