# Use

Use allows you to bring names in modules into your current scope so they can be used without prefixing.

```graphix
net::subscribe(...); // call subscribe in the net module
use net;
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
inside the `net::rest` module and want to use the `net::http` module, you
must write `use net::http`, not `use http`.

```graphix
// inside net::rest
use net::http;     // correct — absolute path
// use http;       // wrong — would look for a top-level "http" module
```

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
