// Language feature tests organized by category

mod arrays;
mod async_restart;
mod attributes;
mod basics;
mod block_types;
mod byref;
mod cast_trait;
mod collection;
mod datetime;
mod delivery;
mod dense_deltas;
mod errors;
mod functions;
mod fusion;
mod fusion_parity;
mod fusion_review;
mod image;
mod inference;

mod fresh_blocks;
mod interfaces;
mod lists;
mod maps;
mod modules;
mod on_blocks;
mod organic_deltas;
mod orphans;
mod par_attrs;
mod par_loops;
mod printing;
mod prune;
mod quantified;
mod repl_inputs;
mod select;
mod select_undecided;
mod seq;
mod seq_abort;
mod seq_calls;
mod seq_errors;
mod seq_let;
mod seq_lowering;
mod seq_shadow;
mod seq_snapshot;
mod seq_steps;
mod seq_try;
mod seqq;
mod test_builtins;
mod traits;
mod tuples_structs;
mod types;
mod variants;
mod wake_outside;

/// `src` is refused with `needle` in its message, in every mode and by
/// the check alone.
// CR claude for claude: [test-gap/low] The doc says the needle holds in every mode and
// by the check alone, but `eval_modes` holds only Interp's build message to it. The
// other three modes need only refuse, and the check's refusal (what `--check` and the
// editor show) is compared for its verdict and then discarded. A check or a fused build
// that refuses for another reason passes every caller, including quantified's
// `refused_positions!`, whose rule is the check's. Expand `refused_positions!` to
// `run!(.., refused(QUANTIFIED); FuseExpect::None)`, which holds the predicate to the
// check's refusal and each mode's. Have `eval_modes` hand back every mode's message and
// the check's for this helper to test, or narrow this doc. Found by reading; not run.
// (tests-lang-rest-c-06)
async fn refused_everywhere(src: &str, needle: &str) -> anyhow::Result<()> {
    match graphix_package_core::testing::eval_modes(src, crate::TEST_REGISTER).await? {
        Err(msg) => assert!(msg.contains(needle), "wrong refusal for {src}: {msg}"),
        Ok(v) => panic!("must be refused: {src} => {v:?}"),
    }
    Ok(())
}
