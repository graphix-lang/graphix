# Graphix Syntax Highlighting for mdbook

This directory contains the custom syntax highlighting definition for the Graphix programming language.

## File: graphix-highlight.js

A highlight.js language definition that provides syntax highlighting for Graphix code blocks in the documentation.

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
