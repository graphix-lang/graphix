//! graphix-rt's integration tests that need no process of their own, one
//! binary so they run side by side. A test that sets process-wide state (the
//! global thread pool, a panic hook) or times itself against the wall clock
//! stays a target of its own beside this one.

mod answered_calls;
mod callable_fusion;
mod embedder_writes;
mod jit_retention;
mod late_readers;
mod pass_thread;
mod runtime_api;
mod send_loop;
mod stop_race;
