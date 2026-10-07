//! Tearing down a deep AST or type must not overflow the stack: the
//! guarded destructors are exercised on trees built directly, deeper than
//! any parse reaches.

use graphix_compiler::expr::{Expr, ExprKind};
use triomphe::Arc;

/// Small enough that an unguarded destructor at DEPTH aborts.
const STACK: usize = 512 * 1024;
const DEPTH: usize = 50_000;

#[test]
fn deep_ast_drops_without_overflow() {
    std::thread::Builder::new()
        .stack_size(STACK)
        .spawn(|| {
            let mut e: Expr = ExprKind::NoOp.to_expr_nopos();
            for _ in 0..DEPTH {
                e = ExprKind::ExplicitParens(Arc::new(e)).to_expr_nopos();
            }
            // The assertion is that this drop returns: an overflow aborts.
            drop(e);
        })
        .expect("spawn")
        .join()
        .expect("the teardown panicked");
}

/// A type is as deep as the program that builds it is long (a chain of
/// `let x1 = [x0]` or of typedefs), so its teardown is guarded too.
#[test]
fn deep_type_drops_without_overflow() {
    use graphix_compiler::typ::Type;
    std::thread::Builder::new()
        .stack_size(STACK)
        .spawn(|| {
            let mut t = Type::Bottom;
            for _ in 0..DEPTH {
                t = Type::Array(Arc::new(t));
            }
            drop(t);
        })
        .expect("spawn")
        .join()
        .expect("the teardown panicked");
}
