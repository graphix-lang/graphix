//! Tearing down a deep AST must not overflow the stack. Raises the
//! nesting limit past what `deep_nesting` reaches so the guarded
//! destructors are exercised. Its own binary because `set_max_nesting`
//! is process-global.

use graphix_compiler::expr::parser;

/// Small enough that an unguarded destructor at DEPTH aborts.
const STACK: usize = 512 * 1024;
// CR claude for claude: [perf] The parse is this test's real cost. One paren level takes
// about 145 KB of stack in the dev/test profile (a parse-only `graphix fmt` peaks at
// 21.5 MB for 1 level and 69.4 MB for 331), so 50,000 levels map roughly 7 GB of
// stacker segments in one test. Only the drop is under test: build the deep Expr
// directly with ExprKind::to_expr, as deep_type_drops_without_overflow nests
// Type::Array, or cut DEPTH to what still overflows an unguarded drop on the 512 KB
// stack. The join's expect also misreports: a stack overflow aborts the process and
// never reaches join, so the expect fires only on a panic such as a failed parse.
// (tests-shell-compiler-09)
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
