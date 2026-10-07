# Core

`print`, `println`, and `log` produce a `null` event after processing each
message. This acknowledgement lets a `seq` step advance, including when
the same message is printed again. Changing only the destination produces
no acknowledgement. If an enclosing expression should produce nothing,
end its block with `never()` explicitly.

```graphix
{{#include ../../../stdlib/graphix-package-core/src/graphix/mod.gxi}}
```

## core::buffer

The `buffer` submodule provides functions for working with raw bytes:
conversion between bytes and strings/arrays, concatenation, and a
flexible binary encode/decode system with control over endianness and
variable-length encoding.

```graphix
{{#include ../../../stdlib/graphix-package-core/src/graphix/buffer.gxi}}
```

## core::math

The `math` submodule wraps Rust's `f64` math intrinsics (trigonometric,
hyperbolic, exponential, logarithmic, power, rounding, comparison,
predicate, and angle-conversion routines) plus the standard
mathematical constants. Argument and result conventions match
`std::f64`: angles are in radians, NaN propagates through arithmetic,
and `min`/`max` return the non-NaN operand when one input is NaN.

For polymorphic n-ary `min` / `max` / `sum` / `product` over `Number`,
use the top-level functions in `core` instead — the bindings here are
the binary `f64`-only forms.

```graphix
{{#include ../../../stdlib/graphix-package-core/src/graphix/math.gxi}}
```

## core::opt

The `opt` submodule provides combinators for working with optional
values — graphix's `Option<'a>` is just the structural union
`['a, null]`, and these functions mirror the most useful parts of
Rust's `std::option::Option` API in a reactive setting.

The higher-order combinators (`map`, `flat_map`, `filter`, `or_else`,
`ok_or_else`, `is_some_and`, `is_none_or`) are deliberately
fire-and-forget: they never queue inputs. If a callback is slow or
never produces a value, a new input simply supersedes the pending one
(latest wins). Use the explicit `core::queue` / `core::hold` operators
when you need ordered async behavior.

```graphix
{{#include ../../../stdlib/graphix-package-core/src/graphix/opt.gxi}}
```
