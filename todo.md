# Graphix TODO List

This document tracks improvements and additions needed for both the Graphix compiler/implementation and the language documentation.

**Last Updated**: 2026-02-16

---

# Compiler/Implementation TODOs

## High Priority

### Standardize Queuing
- remove queueing from core::filter
- implement a queuing adapter `fn('a, fn('a) -> 'b) -> 'b` that
  automatically queues input in front of f until f generates an
  output

### Consider making ~ put it's rhs to sleep
- or implement "if"

### Eliminate Double Typecheck at call sites
- implement Clone for nodes so we can instantiate the node tree of a function and just clone it

### Module System Completeness
- Add module renaming on use

### Add an "unwrap" operator (maybe !)

Three ways to deal with errors. ? raise it, $ log it, and ! crash and print a
backtrace.

### Per module flags

Allow manipulating compiler flags at the module level. For example a
critical module might turn on warnings about unhandled arith, where
the majority of the project leaves it off.

### Desktop GUI Target
- Desktop GUI widget support (mentioned in ui/overview.md)

### Stand Alone Link Mode

Add a compilation mode that automatically builds a rust driver for a stand
alone graphix application from a specification. Essentially automatically do
what the book section on embedding says, with automatic dependency discovery.

### Document the Rust Interfaces

### Math Module in Stdlib

sqrt, sin, cos, tan, etc.

### Flushing behavior
- add an optional #flush argument to print and printf
- add a flush function to core
- investigate difference in flush behavior on mac os vs linux

## Medium Priority

### Specialize Arithmetic Operators

## Lower Priority

### Other Gui Targets
- Web UI target (mentioned in ui/overview.md)
- Mobile UI target (mentioned in ui/overview.md)

---

# Book/Documentation TODOs

## Phase 4: Content Expansion (Lower Priority)

Nice-to-have additions:

### 4.3 Add Performance Guide
- [ ] Create new section about performance
- **Content**:
  - Memory pooling explained in depth
  - Array operations complexity (already mentioned, expand)
  - Map operations complexity
  - When to use which data structure
  - Lazy evaluation implications
  - Profiling techniques
- **Effort**: 4-6 hours
- **Files**: Create `book/src/performance.md` or appendix

### 4.4 Add Common Patterns Section
- [ ] For each stdlib module, add "Common Patterns"
- **Content**:
  - Array transformations
  - Map manipulations
  - String processing
  - Error handling patterns
  - Reactive patterns with connect
- **Effort**: 4-6 hours
- **Files**: All stdlib module files

### 4.5 Add FAQ
- [ ] Create FAQ section
- **Content**: Gather common questions from users
- **Effort**: 2-3 hours (plus ongoing maintenance)
- **Files**: Create `book/src/faq.md`

### 4.6 Add Best Practices Guide
- [ ] Document Graphix idioms
- **Content**:
  - When to use select vs if
  - Naming conventions
  - Module organization
  - Error handling strategy
  - Anti-patterns to avoid
- **Effort**: 3-4 hours
- **Files**: Create `book/src/best_practices.md`

---

## Phase 5: Validation (Ongoing)

### 5.4 External Review
- [ ] Have someone unfamiliar with Graphix read through
- **Goal**: Identify confusing sections, gaps in understanding
- **Effort**: Variable (depends on reviewer availability)
