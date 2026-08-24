# Let Binds

Let bindings introduce names that are visible in their scope after they are
defined.

```graphix
let x = 2 + 2 + x; // compile error x isn't defined yet
let y = x + 1 // ok
```

The same name can be used again in the same scope, it will shadow the previous
value.

```graphix
let x = 1;
let x = x + 1; // ok uses the previous definition
x == 2 // true
```

You can annotate the binding with a type, which will then be enforced at compile
time. Sometimes this is necessary in order to help type inference.

```graphix
let x: Number = 1; // note x will be of type Number even though it's an i64
let y: string = x + 1; // compile time type error
```

You can use patterns in let binds as long as they will always match.

```graphix
let (x, y) = (3, "hello"); // binds x to 3 and y to "hello"
x == 3; // true
y == "hello" // true
```

You can mix type annotations with pattern matches

```graphix
let (x, y): (i64, string) = (3, "hello")
```

To document the public API of a module, use `///` documentation comments in
[interface files](../modules/interfaces.md). Documentation is displayed in the
shell during tab completion and made available by the LSP server.

## Comments

A `//` comment runs to the end of its line and belongs to whatever comes
next: write it on its own line directly above an expression, a `select`
arm, a method inside an `impl` block, or a field of a struct literal.

```graphix
// a counter, one step per cycle
let n = 0;
n <- n + 1;
select n {
  // nothing has happened yet
  0 => "waiting",
  // every later step
  k => "step [k]"
}
```

Anywhere else is a parse error — after an expression on the same line
(`x + 1; // no`), between an operator and its operand, or dangling before
a closing `}` with nothing below it. The rule is what lets the parser keep
every comment in the syntax tree, so tools that rewrite a program (the
formatter, the REPL echo) never lose one.
