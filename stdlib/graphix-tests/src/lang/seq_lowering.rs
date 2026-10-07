// What the seq lowering keeps of the code it rewrites: annotations,
// attributes, declarations, scopes, places, function arguments and the
// queue's flush (design/seq_blocks.md).

use super::dense_deltas::{as_i64s, run_delta};
use anyhow::Result;
use graphix_package_core::testing::{Mode, eval};
use netidx::publisher::Value;

async fn last(code: &str, mode: Mode) -> Result<Value> {
    let (values, _) = run_delta(code, mode).await?;
    Ok(values.last().cloned().expect("a value"))
}

async fn refused(src: &str, needle: &str) {
    match eval(src, crate::TEST_REGISTER).await {
        Err(e) => {
            let msg = format!("{e:#}");
            assert!(msg.contains(needle), "wrong refusal for {src}: {msg}")
        }
        Ok((v, _)) => panic!("must be refused: {src} => {v:?}"),
    }
}

// A `seq let` pattern takes the trigger's annotation, under seq and seqq.
async fn typed_destructuring_trigger(mode: Mode) -> Result<()> {
    for kind in ["seq", "seqq"] {
        let code = format!(
            "{{ let t = {{x: 1, y: 2}}; \
             {kind} let {{x, ..}}: {{x: i64, y: i64}} = t {{ x + 1 }} }}"
        );
        assert_eq!(last(&code, mode).await?, Value::I64(2), "{kind}");
    }
    Ok(())
}

// An error the trigger raises is the enclosing handler's, not the run's.
async fn trigger_error_leaves_the_run(mode: Mode) -> Result<()> {
    let code = r#"{
        let step = 0;
        step <- select step { n if n < 40 => n + 1, _ => never() };
        let e: [i64, Error<`T>] = select step { 5 => error(`T), n => n };
        let go = select step { 1 | 5 => step, _ => never() };
        seq let c = (go ~ e)? { until step > c + 10; c }
    }"#;
    assert_eq!(as_i64s(&run_delta(code, mode).await?.0), [1]);
    Ok(())
}

// A try's value takes its let's annotation whatever the pattern.
async fn try_value_takes_the_annotation(mode: Mode) -> Result<()> {
    let code = r#"{
        let f = |v: i64| -> [i64, Error<`Oops>] error(`Oops);
        seq {
            let (v, n): ([i64, null], i64) = try { (f(1)?, 1) } with(_) { (null, 0) };
            n
        }
    }"#;
    assert_eq!(last(code, mode).await?, Value::I64(0));
    Ok(())
}

// An attribute on a seq statement annotates its computation.
async fn statement_attributes_apply(mode: Mode) -> Result<()> {
    let code = r#"seq {
        #[sync]
        let f = |x| x + 1;
        #[native]
        let y = 41 * 2;
        f(y)
    }"#;
    assert_eq!(last(code, mode).await?, Value::I64(83));
    Ok(())
}

// A declaration in a block statement is a statement there.
async fn declaration_in_a_block(mode: Mode) -> Result<()> {
    let code = r#"seq { let n = { use str::len; len("abc") }; n }"#;
    assert_eq!(last(code, mode).await?, Value::I64(3));
    Ok(())
}

#[tokio::test(flavor = "current_thread")]
async fn lowering_refusals() -> Result<()> {
    refused("seq { until 1 + 1; 42 }", "bool").await;
    refused("seq { #[bogus] let y = 1; y }", "unknown attribute #[bogus]").await;
    refused("seq { use str::len; 1 }", "a declaration is not a seq step").await;
    refused(
        "{ let f = |x: i64| -> [i64, Error<`E>] x; \
         seq { #[sync] let v = try { f(1)? } with(_) { 0 }; v } }",
        "a try statement takes no attribute",
    )
    .await;
    Ok(())
}

// A `use` in a seq body shadows like a `let` (a module's body in a seq
// is a scope of its own: lang::modules::seq_module_is_its_own_scope).
async fn use_shadows(mode: Mode) -> Result<()> {
    let code = r#"{
        let len = 100;
        let t = 1;
        seqq t { select t { _ => { use str::len; len("abc") } } }
    }"#;
    assert_eq!(last(code, mode).await?, Value::I64(3));
    Ok(())
}

// A reference through parentheses is a reference to the variable: it
// keeps the variable live under seqq.
async fn place_through_parens(mode: Mode) -> Result<()> {
    let code = r#"{
        let step = 0;
        step <- select step { n if n < 12 => n + 1, _ => never() };
        let request = select step { 1 | 2 | 3 => step, _ => never() };
        let b = 0;
        let s = {f: 0, g: 0};
        let paren = seqq request { let r = &mut (b); *r <- b + 1; b };
        let field = seqq request { let r = &mut (s.f); *r <- s.f + 1; s.f };
        (paren, field)
    }"#;
    let tuple = |a, b| Value::Array([Value::I64(a), Value::I64(b)].into());
    let (values, _) = run_delta(code, mode).await?;
    assert_eq!(values.last(), Some(&tuple(3, 3)));
    Ok(())
}

// Each run of a destructured seqq trigger reads its own request, an until
// included.
async fn destructured_trigger_reads_its_request(mode: Mode) -> Result<()> {
    let code = r#"{
        let step = 0;
        step <- select step { n if n < 30 => n + 1, _ => never() };
        let request = select step { 1 | 2 | 3 => step, _ => never() };
        seqq let (a, b) = (request, request * 10) { until step > a + 5; a + b }
    }"#;
    assert_eq!(as_i64s(&run_delta(code, mode).await?.0), [11, 22, 33]);
    Ok(())
}

// A function named as a call's argument is the call's to resolve: the
// step is as transparent as with the literal.
async fn named_callback(mode: Mode) -> Result<()> {
    let code = r#"{
        let inc = |x| x + 1;
        let xs = [1, 2, 3];
        let step = 0;
        step <- select step { n if n < 20 => n + 1, _ => never() };
        let go = select step { 1 => step, _ => never() };
        let k = 0;
        k <- select step { 3 => 100, _ => never() };
        let named = seqq go { let a = array::map(xs, inc); until step > 6; (a, k) };
        named.1
    }"#;
    assert_eq!(last(code, mode).await?, Value::I64(0));
    Ok(())
}

// A request that arrives with the flush is kept.
async fn flush_keeps_a_request_with_it(mode: Mode) -> Result<()> {
    let code = r#"{
        let step = 0;
        step <- select step { s if s < 20 => s + 1, _ => never() };
        let cancel = select step { 6 => true, _ => never() };
        let ready = false;
        ready <- select step { 10 => true, _ => never() };
        let a = select step { 1 | 6 => step, _ => never() };
        seqq a; flush(cancel) { until ready; a }
    }"#;
    assert_eq!(as_i64s(&run_delta(code, mode).await?.0), [6]);
    Ok(())
}

modes!(
    typed_destructuring_trigger,
    trigger_error_leaves_the_run,
    try_value_takes_the_annotation,
    statement_attributes_apply,
    declaration_in_a_block,
    use_shadows,
    place_through_parens,
    destructured_trigger_reads_its_request,
    named_callback,
    flush_keeps_a_request_with_it,
);
