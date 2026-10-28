// Language feature tests organized by category

mod arrays;
mod async_restart;
mod attributes;
mod basics;
mod byref;
mod cast_trait;
mod collection;
mod datetime;
mod dense_deltas;
mod errors;
mod functions;
mod fusion;
mod fusion_parity;
mod fusion_review;
mod image;
mod inference;

mod interfaces;
mod lists;
mod maps;
mod modules;
mod organic_deltas;
mod par_attrs;
mod par_loops;
mod printing;
mod prune;
mod select;
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

/// `src` is refused with `needle` in its message, in every mode and by
/// the check alone.
async fn refused_everywhere(src: &str, needle: &str) -> anyhow::Result<()> {
    match graphix_package_core::testing::eval_modes(src, crate::TEST_REGISTER).await? {
        Err(msg) => assert!(msg.contains(needle), "wrong refusal for {src}: {msg}"),
        Ok(v) => panic!("must be refused: {src} => {v:?}"),
    }
    Ok(())
}
