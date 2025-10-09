# Let Binds

Let bindings introduce names that are visible in their scope after they are
defined.

```
let x = 2 + 2 + x; // compile error x isn't defined yet
let y = x + 1 // ok
```

The same name can be used again in the same scope, it will shadow the previous
value.

```
let x = 1;
let x = x + 1; // ok uses the previous definition
x == 2 // true
```

You can annotate the binding with a type, which will then be enforced at compile
time. Sometimes this is necessary in order to help type inference.

```
let x: Number = 1; // note x will be of type Number even though it's an i64
let y: string = x + 1; // compile time type error
```

You can use patterns in let binds as long as they will always match.

```
let (x, y) = (3, "hello"); // binds x to 3 and y to "hello"
x == 3; // true
y == "hello" // true
```

You can mix type annotations with pattern matches

```
let (x, y): (i64, string) = (3, "hello")
```

You can assign documentation to a let bind using a `///` comment. Documentation
will be displayed in the shell when the user tab completes and will be made
available by the lsp server.

```
// this is a normal comment
let x = 1;
/// this is documentation for y
let y = 2;
```
