# Compiler review completion

Rechecked the final finding, CR25, against `b9590a9b`. **CR25 is resolved and
its comment has been removed. All 27 findings from this review are closed.**
This re-evaluation changes only review comments and this report.

## CR25 verification

[abstract_value::hooked](src/abstract_value.rs#L63) clears the thread-local
handle before calling a dispatch function. Code that receives the mutable
context during that dispatch, including a nested builtin's `eval`, therefore
cannot inherit the outer loan. Explicit inner comparison and formatting
wrappers can install their own scoped loans.

The restoration guard is created before the dispatch call. Its `Drop`
implementation restores the saved handle on normal return and panic unwind;
inner guards restore the suspended state before the outer guard restores its
caller. A call with no installed handle leaves that state unchanged.

The former failing probe is now committed as
[hook_loan.rs](../stdlib/graphix-tests/tests/hook_loan.rs#L27). It checks that the
same builtin, which takes no explicit hook loan, compares structurally both at
the top level and inside a Display implementation. The broader trait tests
cover deliberate nested hooks, comparison, sorting, map keys, and printing.

## Validation

- `cargo test -p graphix-compiler -p graphix-tests`: **2,980 passed, two ignored**.
- `nested_builtin_runs_unarmed`, the formerly failing CR25 probe: passed.
- `cargo fmt --all --check`: passed.
- `git diff --check`: passed; no Codex CR/XCR comments remain in the compiler.
