# Use

Use allows you to bring names in modules into your current scope so they can be used without prefixing.

```graphix
sys::net::subscribe(...); // call subscribe in the sys::net module
use sys::net;
subscribe(...) // same function
```

Use is valid anywhere expressions are valid

```graphix
let list = {
  use array;
  map([1, 2, 3, 4, 5], |x| x * 2)
};
list
```

will print `[2, 4, 6, 8, 10]`

```graphix
let list = {
  use array;
  map([1, 2, 3, 4, 5], |x| x * 2)
};
map(list, |x| x * 2)
```

will not compile, e.g.

```
$ graphix test.gx
Error: in file "test.gx"

Caused by:
    at line: 5, column: 1 map not defined
```

Use paths are always absolute — they are resolved from the root of the
module tree, not relative to the current module. For example, if you are
inside the `sys::net` module and want to use the `sys::time` module, you
must write `use sys::time`, not `use time`.

```graphix
// inside sys::net
use sys::time;     // correct — absolute path
```

A submodule can reference bindings from its parent directly, but
only if the `mod` declaration comes after those bindings in the
parent's interface file.

Use shadows earlier declarations in it's scope. Consider,

```graphix
let map = |a, f| "hello you called map!";
let list = {
  use array;
  map([1, 2, 3, 4, 5], |x| x * 2)
};
(list, map(list, |x| x * 2))
```

prints

```
$ graphix test.gx
([2, 4, 6, 8, 10], "hello you called map!")
```
