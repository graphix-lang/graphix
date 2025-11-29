# Graphix TODO List

This document tracks improvements and additions needed for both the Graphix compiler/implementation and the language documentation.

**Last Updated**: 2025-10-27

---

# Compiler/Implementation TODOs

## High Priority

### Investigate Legacy Stdlib Arithmetic Functions
- Review and test legacy stdlib functions for correct error handling
- **Context**: Functions like `divide`, `product`, `sum` were written before the type system existed
- **Investigation needed**:
  - How do they handle division by zero?
  - How do they handle overflow/underflow?
  - Do they use the `throws` mechanism correctly?
  - Are error types properly specified in signatures?
  - Do they match the documented arithmetic error behavior in fundamental_types.md?
- **Action**: Fix incorrect behavior, update type signatures, ensure consistency with language semantics
- **Effort**: 2-4 hours (research + fixes + tests)
- **Files**:
  - `graphix-stdlib/src/core.rs` (implementation)
  - `graphix-stdlib/src/core.gx` (signatures)
  - Tests to verify correct behavior
  - `book/src/stdlib/core.md` (documentation updates after fixes)

### Standardize Queuing
- remove queueing from core::filter
- implement a queuing adapter `fn('a, fn('a) -> 'b) -> 'b` that
  automatically queues input in front of f until f generates an
  output

### Consider making ~ put it's rhs to sleep
- or implement "if"

### Eliminate Double Typecheck at call sites
- implement Clone for nodes so we can instantiate the node tree of a function and just clone it

### Fix Parser Operator Precidence
- Operator precidence in the parser is broken, all arithmetic and boolean
  operators have the same prescidence. Fix this so that arith operators have
  standard infix prescidence rules, and boolean compares are higher than and,
  or, and not

### Parser allow trailing ;
- Allow trailing ; in the shell
- possibly allow it in modules, do the same thing as rust and OCaml
  and insert a NOP after it

### Module System Completeness
- Add module renaming on use
- Add gxi module signatures
- **Context**: Mentioned as "work in progress" in modules/overview.md
- **Effort**: Unknown (depends on design decisions)

### Log $ operator errors

When the $ operator eats an error, allow the user to log it, by
default log it as warn. Possibly allow the user to change to log level
and target.

### Add an "unwrap" operator (maybe !)

Three ways to deal with errors. ? raise it, $ log it, and ! crash and
print a backtrace.

### Per module flags

Allow manipulating compiler flags at the module level. For example a
critical module might turn on warnings about unhandled arith, where
the majority of the project leaves it off.

### Type System
- Once gxi files/module sigs are added add abstract types

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

## Phase 3: Quality Improvements (Medium Priority)

Polish and consistency:

### 3.1 Add Operators Quick Reference
- [ ] Create appendix with operator table
- **Content**:
  - `<-` (connect), `~` (sample), `?` (try/throw), `$` (or never)
  - `->` (lambda/function return), `=>` (select arm, map literal)
  - `..` (array slice)
  - All arithmetic, comparison, boolean operators
  - Precedence and associativity
- **Effort**: 2 hours
- **Files**: Create `book/src/appendix/operators.md`, update SUMMARY.md

### 3.2 Add Syntax Quick Reference
- [ ] Create appendix with syntax patterns
- **Content**:
  - Let binds, functions, select expressions
  - Structs, tuples, arrays, maps
  - Type annotations, modules
- **Effort**: 2 hours
- **Files**: Create `book/src/appendix/syntax_reference.md`

### 3.4 Add Cross-References
- [ ] Add more internal links between sections
- **Priority links**:
  - connect.md → select.md and vice versa
  - error.md → functions (for throws)
  - fundamental_types.md → error handling
  - All core language sections should link to each other where relevant
- **Effort**: 1-2 hours
- **Files**: Core language sections primarily

---

## Phase 4: Content Expansion (Lower Priority)

Nice-to-have additions:

### 4.1 Add Examples Gallery
- [ ] Document what's in `graphix-shell/examples/`
- **Content**:
  - List all examples with brief descriptions
  - Categorize (beginner, intermediate, advanced)
  - Note which demonstrate which features
- **Effort**: 2-3 hours
- **Files**: Create `book/src/examples_gallery.md` or appendix section

### 4.2 Add Module System Practical Guide
- [ ] Expand modules section with real examples
- **Content**:
  - Multi-file project example
  - Directory structure recommendations
  - Common patterns
  - Module reuse strategies
- **Effort**: 3-4 hours
- **Files**: `book/src/modules/practical.md` or expand existing files

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

Ensure quality:

### 5.1 Verify All Example Files Exist
- [ ] Check every `{{#include ...}}` reference
- **Script**: Could write a script to validate
- **Action**: Create missing examples or mark as conceptual
- **Effort**: 3-4 hours
- **Files**: All TUI examples primarily

### 5.2 Test All Example Code
- [ ] Run `cargo run --bin graphix --check` on all examples
- **Action**: Fix any that don't compile
- **Effort**: 2-4 hours (depending on issues found)
- **Files**: All .gx example files

### 5.3 Review for Consistency
- [ ] Read through entire book for tone, terminology, formatting
- **Check**:
  - Consistent capitalization (Graphix vs graphix)
  - Consistent terminology
  - Consistent formatting
  - Proper grammar/spelling
- **Effort**: 4-6 hours
- **Files**: All files

### 5.4 External Review
- [ ] Have someone unfamiliar with Graphix read through
- **Goal**: Identify confusing sections, gaps in understanding
- **Effort**: Variable (depends on reviewer availability)

---

## Timeline Summary

| Phase | Priority | Duration | Can Start |
|-------|----------|----------|-----------|
| Phase 1 | Critical | 2-3 days | Immediately |
| Phase 2 | High | 3-4 days | After Phase 1 |
| Phase 3 | Medium | 2-3 days | Parallel with Phase 2 |
| Phase 4 | Lower | 3-5 days | After Phase 2 |
| Phase 5 | Ongoing | 2-3 days | Throughout |

**Total estimated effort**: 12-18 days of focused work

---

## Quick Wins (Can do in 1-2 hours)

If you want to make immediate progress:
1. [ ] Fix ToC structure (5 min)
2. [ ] Sanitize paths (15 min)
3. [ ] Fix intro sentence (2 min)
4. [ ] Add embedding overview intro (1 hour)
5. [ ] Improve install.md (30 min)

**Total**: ~2 hours for massive improvement in book quality

---

## Specific Issues Found

### Typos/Errors
- [ ] `intro.md` line 6: "like the popular UI library" - incomplete comparison
- [ ] `fundamental_types.md`: "z32" and "v32" appear in core.md but not explained
- [ ] Error example outputs show home directory paths `/home/eric/` - should be sanitized

### Missing Documentation
- [ ] `core/block.md` - what does this cover? (Scoping blocks? `{}`?)
- [ ] `core/use.md` - module imports presumably, but no preview in TOC
- [ ] Several stdlib modules need brief descriptions in the TOC

---

## Notes

- The book structure is solid and flows well
- TUI documentation is excellent and comprehensive
- Core language explanations are clear (especially connect and select)
- Main gaps are in getting started materials and consistency polish
- After Phase 1-2 completion, the book will be publication-ready
