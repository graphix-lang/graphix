# Modules

Graphix has a module system for organizing code into projects and controlling
what parts of a module are publicly accessible.

Current features include:

- module hierarchies
- implementation files (`.gx`) for module code
- interface files (`.gxi`) for defining module APIs and controlling visibility
- modules stored in files or netidx
- modules dynamically loadable at runtime

Note: module renaming on `use` is not yet supported but may be added in a future
release.
