# User Defined Types

You can define your own data types in Graphix. This is useful for many tasks,
such as enforcing interface invariants, and modeling data. As a data modeling
language Graphix supports both structures, so called conjunctive types, where
you are modeling data that always appears together, and variants, or so called
disjunctive types, where a type can be one of many possible types drawn from a
set. This contrasts with other languages, for example Python, which only support
conjunctive types.

## Structural Typing

<!-- CR claude for claude: [doc-drift/low] "Types in Graphix don't formally have names"
(and named.md's "just aliases") is no longer true: a `type T = Abstract<rep>` typedef is
nominal, distinct from its representation at compile time and run time. The traits
chapter relies on `Abstract<..>` from its first example, yet this part of the book never
introduces it; it appears only in modules/interfaces.md, later in the summary. State the
exception here, with a link to the abstract-types section, or move that section's basics
into this chapter before Traits. Found by reading; not run. (book-modules-udt-06) -->
In most languages types are dealt with by name, meaning that two structs with
exactly the same fields are still different types if they have a different name.
The obvious implication of this is that all types need to be given a name, and
thus declared. Graphix works differently. Types in Graphix are structural,
meaning that types that are structurally the same are the same type. In fact
types in Graphix don't formally have names, there can be aliases for a large
type to cut down on verbosity, but an alias is always resolved to the structural
type when type checking. Because of this you don't need to declare types before
using them.

## Set Based Type System

The Graphix type system is based on set operations. For example, a function
could declare that it can take either an `i32` or an `i64` as an argument by
defining the set, `[i32, i64]` and annotating it's argument with this type.

```graphix,ignore
let f = |a: [i32, i64]| ...
```

When this function is called, the type checker will check that the type of
argument `a` is a subset of `[i32, i64]`, and will produce a type error if it is
not. Pretty much every operation the type checker performs in Graphix is one of,
or a combination of, simple set operations contains, union, difference, etc.

The combination of structural typing, set based type operations, and aggressive
type inference is meant to make Graphix feel like an untyped scripting language
most of the time, but still catch a lot of mistakes at compile time, and make it
possible to enforce interface contracts.
