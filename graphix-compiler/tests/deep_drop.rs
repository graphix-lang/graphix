//! Tearing down a deep AST must not overflow the stack. Raises the
//! nesting limit past what `deep_nesting` reaches so the guarded
//! destructors are exercised. Its own binary because `set_max_nesting`
//! is process-global.

use graphix_compiler::expr::parser;

/// Small enough that an unguarded destructor at DEPTH aborts.
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
            // The assertion is that this drop returns.
            drop(e);
        })
        .expect("spawn")
        .join()
        .expect("deep AST teardown overflowed the stack");
}

/// A type is as deep as the program that builds it is long (a chain of
/// `let x1 = [x0]` or of typedefs), so its teardown is guarded too.
#[test]
#[cfg_attr(not(feature = "slow-tests"), ignore = "slow-tests")]
fn deep_type_drops_without_overflow() {
    use graphix_compiler::typ::Type;
    use triomphe::Arc;
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
        .expect("deep type teardown overflowed the stack");
}
