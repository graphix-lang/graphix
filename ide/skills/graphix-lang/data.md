# Data: reads, writes, casts and printing

A type-directed read (`json::read`,
`str::parse`) takes its target from the annotation it reaches, and the
target must settle concrete (its `'b: Concrete` bound). `str::parse` reads
what Graphix prints: a `string` target is the text itself; any other
target reads the text as a Graphix literal (`(1, "a")`, `{x: 1}`,
`` `B(3) ``, `[<1, 2>]`, `{"k" => 1}`) cast to it, else the bare text cast
(`"42"`, an RFC3339 datetime). `json::read` into a target holding an
integer or `decimal` reads a number's text exactly (past `i64` is an
`InvalidCast`); a read into neither, `Array<Any>`, gives `f64`. A struct
target reads a field the data omits as null where the field's type
admits null. Excess decimal fractional precision rounds to the nearest
representable decimal in JSON reads, string casts and `str::parse`.
JSON/TOML write a map as an object/table only when its key type is
`string`; any other key type writes an array of `[key, value]` pairs. TOML
requires a table at the document root, so `toml::write_str` refuses such a
map as the document (wrap it in a struct field), and has no null: a null struct
field is left out, a null anywhere else is refused when written. An
abstract value is written as an object its type's name keys
(`{"Port":80}`), and read only so; the abstract values inside that data
are bare where the type's `from_value` reads it (a cast of data), else
wrapped too; bare, they carry no tag, so a union in a `from_value`'s data
holds an abstract type only beside null. `Transparent(p)` (core's `Transparent<T>`) writes `p`'s
data bare. `hbs::render`'s data writes abstract values bare and a map
whose keys are scalars as an object. Every union a json/toml read or
write holds is one member beside null, or variants and abstract types
under distinct tags: `[i64, string]`, `[Port, u8]` and `` [string, `A] ``
are refused (`JsonData`/`TomlData`); tag the members, `` [`N(i64),
`S(string)] ``.

A whole float prints with its point (`3.0`) in interpolation, `println` and
`cast<string>`, so the text reads back as a float (`cast<[i64, f64]>("3.0")`
is 3.0); a scale-zero decimal prints plainly without a trailing point,
and a decimal cast from a float is the number its shortest text names,
`decimal:0.30000000000000004` for `0.1 + 0.2`, rounded to 28 places, a
whole one keeping its point (`decimal:3.0`) and a zero, one too small for
28 places included, `decimal:0.0` with the float's sign. A
union cast takes a member of the value's own kind first, however much of
it that keeps: `cast<[f32, string]>(0.1)` is f32, `cast<[i64,
string]>(3.5)` is 3; text tries the kind it parses as next
(`cast<[duration, decimal]>("3.0")` is the decimal). Within a kind, an
exact member beats one that rounds, which beats one that drops a part
(`cast<[i64, f32]>(0.1)` is f32 0.1), and a float or decimal, value or
text, goes to an integer member only where no `f32`/`f64`/`decimal`
member holds it exactly (`cast<[i64, decimal]>(3.0)` is the decimal,
`cast<[i64, datetime]>("3.0")` the integer). An integer likewise goes to
a real member only where no integer member holds it, however the union
names its members; a bool is held exactly by 1 or 0 (`cast<[f32,
u8]>(true)` is u8 1). A number too large for a float member reads as its
infinity only when no member of any kind holds it (`cast<[f32,
string]>(1e300)` is the string, `cast<f32>(1e300)` inf).
Printed composites escape brackets in strings and retain numeric type prefixes
except for i64/f64. Bytes print as `bytes:` literals. Only `str::parse` also
reads printed UTC datetimes, every printed duration unit and bare
NaN/inf/-inf. These do not extend source syntax.
Error diagnostics keep unescaped brackets in their strings.
