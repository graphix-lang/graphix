# Graphix Syntax Highlighting for mdbook

This directory contains the custom syntax highlighting definition for the Graphix programming language.

## File: graphix-highlight.js

A highlight.js language definition that provides syntax highlighting for Graphix code blocks in the documentation.

<!-- CR claude for claude: [doc-drift/low] This list describes a highlighter
graphix-highlight.js is not. It claims block comments `/* */` (Graphix has none and the
script has no rule), a built-in function list (the script colors every `name(` alike),
types String/Bool/DateTime/Duration (not Graphix types) and duration literals
`1.5s`/`500ms` (Graphix writes duration:1.5s, duration:500.ms), and it omits most of the
script's KEYWORDS. Fix: point at KEYWORDS and TYPES in graphix-highlight.js instead of
restating them, or delete the README. Found by reading; not run. (ide-tooling-07) -->
### Supported Features

The highlighter recognizes and highlights:

**Keywords:**
- Control flow: `let`, `fn`, `select`, `if`, `as`, `with`
- Module system: `mod`, `type`, `val`, `sig`, `use`
- Error handling: `throws`
- Dynamic modules: `dynamic`, `sig`, `source`

**Literals:**
- Booleans: `true`, `false`
- Null: `null`

**Built-in Functions:**
- Output: `print`, `println`, `log`, `dbg`
- Utilities: `error`, `never`, `cast`
- Operators: `all`, `and`, `or`, `filter`, `max`, `min`, `sum`, `product`, etc.

**Types:**
- Primitive types: `i32`, `z32`, `i64`, `z64`, `u32`, `v32`, `u64`, `v64`, `f32`, `f64`, `bool`, `string`, `null`
- Generic types: `Array`, `Map`, `Result`, `Option`, `Error`
- Other types: `String`, `Number`, `Int`, `Float`, `Bool`, `DateTime`, `Duration`

**Operators:**
- Connect: `<-`
- Function arrow: `->`
- Select arm: `=>`
- Sample: `~`
- Try/throw: `?`
- Or never: `$`
- Range: `..`
- Module path: `::`
- Pattern bind: `@`
- Dereference: `*`
- Arithmetic, comparison, logical operators

**Special Syntax:**
- Variants: `` `VariantName ``, `` `Foo(i64) ``
- Type parameters: `'a`, `'b`, `'r`, `'e`
- Labeled arguments: `#label:`
- Module paths: `array::map`, `net::subscribe`
- String interpolation: `"value: [x]"`
- Lambda syntax: `|x, y| x + y`
- Duration literals: `1.5s`, `500ms`

**Comments:**
- Regular comments: `//`
- Documentation comments: `///`
- Block comments: `/* */`

### Usage

The highlighter is automatically loaded by mdbook and applied to all code blocks tagged with ` ```graphix `.

### Future Enhancements

Potential improvements:
- Better string interpolation highlighting (nested expressions)
- Pattern matching highlighting
- Improved function definition detection
- Struct field highlighting
- Enhanced error type highlighting
