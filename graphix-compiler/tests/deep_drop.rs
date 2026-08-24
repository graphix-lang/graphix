//! Tearing down a deep AST must not overflow the stack.
//!
//! `graphix-shell`'s `deep_nesting` test covers the whole pipeline, but
//! only up to `max_nesting()` — which is set low enough that the
//! recursive destructors are never reached. This one raises the limit
//! and goes straight at the teardown, so the guards on `Expr::drop` and
//! friends are tested rather than merely present.
//!
//! Its own test binary because `set_max_nesting` is process-global.
//!
//! Behind the `slow-tests` feature (it costs ~40s): the guards it covers
//! only move when a new recursion is added, so it runs at the release
//! gate rather than every session.

use graphix_compiler::expr::parser;

/// A quarter of a tokio worker's stack. Deep enough nesting on a small
/// enough stack that an unguarded destructor aborts.
const STACK: usize = 512 * 1024;
const DEPTH: usize = 50_000;

#[test]
#[cfg_attr(not(feature = "slow-tests"), ignore = "slow-tests")]
fn deep_ast_drops_without_overflow() {
    parser::set_max_nesting(usize::MAX);
    std::thread::Builder::new()
        .stack_size(STACK)
        .spawn(|| {
            let src = format!("{}1{}", "(1 + ".repeat(DEPTH), ")".repeat(DEPTH));
            let e = parser::parse_one(&src).expect("parses");
            // The assertion is reaching the end of this function: the
            // drop below recurses `Expr::drop` -> `ExprKind` glue ->
            // `Arc<Expr>` -> `Expr::drop`, once per level.
            drop(e);
        })
        .expect("spawn")
        .join()
        .expect("deep AST teardown overflowed the stack");
}
