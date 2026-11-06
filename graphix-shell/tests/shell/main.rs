//! graphix-shell's integration tests that need no process of their own, one
//! binary so they run side by side. A test that sets process-wide state (an
//! env var, a panic hook), re-executes its binary, reads a child's peak RSS
//! (which counts the parent's at the fork) or times itself against the wall
//! clock stays a target of its own beside this one.

#[path = "../common/mod.rs"]
mod common;

mod block_type_image;
mod block_types;
mod bound_callee_assertion;
mod cache_trust_report;
mod check_leaves_assertions_to_the_build;
mod check_numeric_singleton;
mod check_whole_script;
mod checked_float_inline;
mod constructor_image;
mod default_check_cost;
mod effect_order;
mod elab_audit;
mod examples_compile;
mod exit_code;
mod exit_teardown;
mod forked_failed_load;
mod fused_root_typedef;
mod idle_module_cost;
mod import_cycle;
mod init_limit_logs;
mod interrupt_wedge;
mod jit_arena_rotation;
mod jit_unavailable;
mod kernel_ssa_growth;
mod lsp_stdio;
mod netidx_zero_config;
mod no_subst;
mod profile_instances;
mod program_args;
mod repl_interrupt;
mod sandbox_whitelist;
mod script_exit;
mod script_interface;
mod script_scope;
mod seqq_nested_credit;
mod shared_bodies;
mod standing_bottom_logs;
mod swallowed_error_logs;
mod task_audit;
mod tui_suspend_without_display;
mod warm_check;
mod warm_only_compiles;
mod warning_order;
mod written_lets_cost;
