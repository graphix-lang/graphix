# Modules

Graphix has a module system for organizing code into projects and controlling
what parts of a module are publicly accessible.

Current features include:

- module hierarchies
- implementation files (`.gx`) for module code
- interface files (`.gxi`) for defining module APIs and controlling visibility
- explicit imports with renaming (`use a::b as c`) and glob imports (`use a::*`)
- relative paths from `self`, `super`, and `package` roots
- modules stored in files or netidx
- modules dynamically loadable at runtime

Name resolution follows the same discipline as Rust 2018: every name in
scope arrived by an explicit declaration, an explicit `use`, or one of
the two preludes (the `core` prelude and the installed package names).
See [Use](../core/use.md) for the import rules.
