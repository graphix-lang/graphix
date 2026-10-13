# list

`List<'a>` is the built-in singly linked list type — see
[Fundamental Types](../core/fundamental_types.md#list) for the type
itself, `[<1, 2, 3>]` literals, and
[list patterns](../core/select.md#list-patterns). Two lists with a
common tail share memory, cons (prepend) is O(1), and indexed access
is O(n). The `list` module provides the functions for working with
lists.

```graphix
{{#include ../../../stdlib/graphix-package-list/src/graphix/mod.gxi}}
```
