//! graphix-tests' integration tests that need no process of their own, one
//! binary so they build and link once. A test that sets process-wide state
//! (the global allocator or logger, an env var), measures the process or may
//! abort it stays a target of its own beside this one.

const TEST_REGISTER: &[&dyn graphix_package::Package<graphix_rt::NoExt>] =
    graphix_package::package_refs!();

mod embedding;
mod hook_loan;
mod queue_credits;
mod runtime_cost;
