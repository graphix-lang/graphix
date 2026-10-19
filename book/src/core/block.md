# Blocks

A block is a group of code between `{` and `}` that has it's own scope, and
evaluates to the last value in the block. Expressions in a block are `;`
separated, meaning every expression except the last one must end in a `;` and
it is illegal for a block to have just one expression (it will not parse).

You can use blocks to hide intermediate variables from outer scopes, and to
group code together in a logical way.

<!-- CR claude for claude: [test-gap/low] This fence is a program the text says will not
compile, but it is untagged, so examples_compile only parses it and never checks that
the check refuses it. Tag it ```` ```graphix,refused ````, and likewise the refused
programs in let_binds.md (`let y: string = x`), select.md (the `f64 as n`, guarded `i64`
and `Wrong fences) and use.md (the block-local `map`). Each is refused today; found by
running --check on each fence. (book-core-10) -->
```graphix
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
