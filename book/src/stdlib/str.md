# Str

`str::parse` reads printed values into its annotated type. Inside composites,
strings escape interpolation brackets and numeric kinds other than `i64` and
`f64` retain their type prefix. A whole float prints with its point (`3.0`),
so `cast<[i64, f64]>("[x]")` of a float `x` reads back the float. Bytes print as
`bytes:` literals.

The data reader accepts printed UTC datetimes, durations in every printed unit
(`ns`, `us`, `ms`, `s`, `m`, `h`, `d`, `M`, `y`; `µs` too), and `NaN`, `inf`, and `-inf`. These printed forms do not extend program syntax.
Diagnostic strings inside errors retain their readable, unescaped brackets.

```graphix
{{#include ../../../stdlib/graphix-package-str/src/graphix/mod.gxi}}
```
