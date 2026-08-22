# Use

`use` imports a *name* into the current scope. It follows the same
rules as Rust 2018 paths: every name in scope arrived either by an
explicit declaration (`let`, `type`, `mod`), an explicit `use`, or one
of the two preludes described below. Nothing else is implicit.

```graphix
sys::net::subscribe(...); // fully qualified call
use sys::net;             // import the name `net`
net::subscribe(...);      // now `net` is in scope
use sys::net::subscribe;  // import the item itself
subscribe(...)            // same function
```

Note that `use sys::net` imports the single name `net` — it does not
bring the *contents* of `sys::net` into scope. To import an item, name
the item. An imported name covers every declaration kind that shares
it: if a module exports both a type `Color` and a value `Color`, one
`use m::Color` imports both.

## Forms

```graphix
use tui::line;                    // one name
use tui::{line, span, style};     // group
use sys::{fs::{self, watch}, io}; // groups nest; self names the prefix
use tui::line as tline;           // rename
use array::*;                     // glob: import everything array exports
use super::{answer, identicon};   // from the parent module
use package::tui::pump;           // from the current package's root
```

`self` inside a group names the prefix itself: `use sys::{self, net}`
is `use sys` plus `use sys::net`. A trailing comma is allowed. The
same syntax works in `.gxi` interface files.

Globs are legal but discouraged outside test modules — an explicit
list documents where every name comes from, and two globs that both
provide a name make that name an error at its first use.

The `{self, *}` group is the idiom for the UI packages' widget
modules, where a module and its main function share a name:
`use tui::text::{self, *}` imports the module `text` *and* its
contents, so both the widget call `text(&"hi")` and qualified access
to the module's other items work.

## Path roots

The leading segment of any path — in `use`, in expressions, and in
type annotations — is one of:

- a **package name**: `array::len`, `tui::line`. Every installed
  package's name is usable as a path root anywhere, with no `use`
  (this is the *package prelude*; only the name is implicit, not the
  package's items).
- **`self`** — the current module: `self::helper`.
- **`super`** — the parent module, chainable: `use super::super::x`.
  In a program file's top-level submodules, `super` reaches the
  bindings at the top of the file.
- **`package`** — the root of the current package (like Rust's
  `crate`): `use package::tui::pump`.

The same grammar works outside `use`: `super::helper(x)` and
`let p: package::tui::Pump = ...` are both valid.

## The core prelude

The root items of the `core` package (`print`, `println`, `cast`,
`error`, `filter`, `once`, `never`, `seq`, `Any`, `Result`, ...) and
the primitive type names are implicitly imported into every module.
They can be shadowed by explicit declarations and imports.

## Scoping

`use` is valid anywhere expressions are valid, and imports into the
enclosing lexical scope only:

```graphix
let list = {
  use array::map;
  map([1, 2, 3, 4, 5], |x| x * 2)
};
list
```

will print `[2, 4, 6, 8, 10]`

```graphix
let list = {
  use array::map;
  map([1, 2, 3, 4, 5], |x| x * 2)
};
map(list, |x| x * 2)
```

will not compile — the import is local to the block:

```
$ graphix test.gx
Error: in file "test.gx"

Caused by:
    0: at: line: 5, column: 1 in file test.gx, in: map
    1: map not defined
```

Unlike declarations, imports carry no ordering constraint within a
module: a `use` may name an item that is declared later in the file,
and `mod` statements may appear anywhere. Visibility is
order-independent; only *execution* of a module's body follows
declaration order.

## Shadowing and collisions

Within one scope, names resolve in precedence order: an explicit
declaration beats an import, an import beats a glob, a glob beats the
package prelude, and the package prelude beats the core prelude.
Inner scopes shadow outer scopes as usual, so a block-local `use`
shadows an outer `let`:

```graphix
let map = |a, f| "hello you called map!";
let list = {
  use array::map;
  map([1, 2, 3, 4, 5], |x| x * 2)
};
(list, map(list, |x| x * 2))
```

prints

```
$ graphix test.gx
([2, 4, 6, 8, 10], "hello you called map!")
```

The error cases:

- a declaration and an import of the same name in the same scope:
  error at the `use` if the declaration comes first; if the
  declaration comes after, it shadows the import (rebinding a name
  with `let` is idiomatic Graphix and imports are no different)
- two explicit imports of the same name: error — rename one
- two globs that both provide a name: allowed at import, error at the
  first *use* of the ambiguous name, naming both sources
