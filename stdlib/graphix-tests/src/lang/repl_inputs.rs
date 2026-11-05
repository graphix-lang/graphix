// A REPL input is checked and run alone: at its end every cell it leaves
// open settles, ⊥ where nothing bounds it, and a later input that would
// decide one is refused, naming the binding to annotate; each refusal is
// paired with the annotation that lifts it. A dynamic module's load is an
// input of the same kind, after the program that loads it.

use anyhow::{Result, bail};
use arcstr::ArcStr;
use graphix_compiler::expr::{VfsEntry, VfsResolver};
use graphix_package_core::{
    run,
    testing::{FuseExpect, TestCtx, config, is},
};
use graphix_rt::GXEvent;
use netidx::publisher::Value;

enum Step {
    Accept(&'static str),
    /// Refused, with an error that contains the message.
    Refuse(&'static str, &'static str),
    /// Refused, with an error that ends with the message.
    RefuseEnding(&'static str, &'static str),
    /// Accepted, and produces the value.
    Produce(&'static str, Value),
    /// Accepted, and produces a string that contains the text.
    ProduceText(&'static str, &'static str),
}

use Step::{Accept, Produce, ProduceText, Refuse, RefuseEnding};

/// Whether `v` is what `step` is to produce.
fn produced(step: &Step, v: &Value) -> bool {
    match step {
        Produce(_, want) => v == want,
        ProduceText(_, text) => matches!(v, Value::String(s) if s.contains(text)),
        Accept(_) | Refuse(..) | RefuseEnding(..) => false,
    }
}

/// Run `steps` as the inputs of one session, in order.
async fn session(steps: &[Step]) -> Result<()> {
    session_over(&[], steps).await
}

/// [`session`] with `files`, each a path and its text, where its `mod`
/// statements find them.
async fn session_over(files: &[(&'static str, &str)], steps: &[Step]) -> Result<()> {
    let (tx, mut rx) = tokio::sync::mpsc::channel(1000);
    let files = files.iter().map(|(path, text)| {
        (netidx::path::Path::from(*path), VfsEntry::from(ArcStr::from(*text)))
    });
    let resolver = VfsResolver::new(ahash::AHashMap::from_iter(files));
    let ctx =
        TestCtx::start(config(tx, crate::TEST_REGISTER, vec![resolver], |_| {})?).await?;
    // an input's graph lives while its handle does
    let mut held = Vec::with_capacity(steps.len());
    for step in steps {
        let input = match step {
            Accept(input) => {
                held.push(ctx.rt.compile(ArcStr::from(*input)).await?);
                continue;
            }
            Refuse(input, msg) | RefuseEnding(input, msg) => {
                let ending = matches!(step, RefuseEnding(..));
                match ctx.rt.compile(ArcStr::from(*input)).await {
                    Ok(_) => bail!("{input} was accepted"),
                    Err(e) => {
                        let e = format!("{e:#}");
                        match ending {
                            true if e.ends_with(msg) => continue,
                            false if e.contains(msg) => continue,
                            _ => bail!("{input} was refused otherwise: {e}"),
                        }
                    }
                }
            }
            Produce(input, _) | ProduceText(input, _) => input,
        };
        let r = ctx.rt.compile(ArcStr::from(*input)).await?;
        let id = r.exprs[0].id;
        held.push(r);
        let deadline = tokio::time::Instant::now() + std::time::Duration::from_secs(10);
        'wait: loop {
            let batch = tokio::time::timeout_at(deadline, rx.recv()).await;
            let Ok(Some(batch)) = batch else {
                bail!("{input} never produced its value")
            };
            for ev in batch.iter() {
                if let GXEvent::Updated(eid, v) = ev
                    && *eid == id
                    && produced(step, v)
                {
                    break 'wait;
                }
            }
        }
    }
    Ok(())
}

#[tokio::test(flavor = "current_thread")]
async fn empty_literal_settles_at_its_input() -> Result<()> {
    session(&[
        Accept("let e = []"),
        Refuse(
            "e <- [1]",
            "e is Array<_>, settled when its input ended, and cannot hold Array<i64>; \
             declare e: Array<i64> where it is bound (line 1, column 5 in `let e = []`)",
        ),
    ])
    .await
}

#[tokio::test(flavor = "current_thread")]
async fn annotated_empty_literal_takes_a_later_write() -> Result<()> {
    session(&[
        Accept("let e: Array<i64> = []"),
        Accept("e <- [1]"),
        Produce("e", Value::Array([Value::I64(1)].into())),
    ])
    .await
}

#[tokio::test(flavor = "current_thread")]
async fn bottom_let_settles_at_its_input() -> Result<()> {
    session(&[
        Accept("let y = never()"),
        Refuse(
            "y <- 1",
            "y is _, settled when its input ended, and cannot hold i64; \
             declare y: i64 where it is bound",
        ),
    ])
    .await
}

/// A function a later input defines writes an earlier binding held to
/// that binding's type: its parameter is ⊥, and a call with a value is
/// refused, naming the binding.
#[tokio::test(flavor = "current_thread")]
async fn function_write_is_held_to_an_earlier_binding() -> Result<()> {
    session(&[
        Accept("let y = never()"),
        Accept("let p = |x| { y <- x; x }"),
        Refuse(
            "p(1)",
            "type mismatch _ does not contain i64: it holds the type of `y` (line 1, \
             column 5 in `let y = never()`), settled when its input ended; declare y's \
             type where it is bound",
        ),
        Refuse("p(\"s\")", "type mismatch _ does not contain string"),
    ])
    .await
}

#[tokio::test(flavor = "current_thread")]
async fn annotated_binding_takes_a_function_write() -> Result<()> {
    session(&[
        Accept("let y: i64 = never()"),
        Accept("let p = |x| { y <- x; x }"),
        Accept("p(1)"),
        Refuse("p(\"s\")", "type mismatch i64 does not contain string"),
        Produce("y + 1", Value::I64(2)),
    ])
    .await
}

/// A constructed abstract value's parameter its input left open is ⊥:
/// a later type test of another parameter covers nothing of it.
#[tokio::test(flavor = "current_thread")]
async fn abstract_parameter_settles_at_its_input() -> Result<()> {
    session(&[
        Accept("type B<'x> = Abstract<i64>"),
        Accept("let t = B(1)"),
        Refuse(
            "select t { B<i64> as _ => 1 }",
            "missing match cases: no arm covers B<_>: it holds the type of `t` (line 1, \
             column 5 in `let t = B(1)`)",
        ),
        Accept("let f = |x: B<i64>| 1"),
        Refuse(
            "f(t)",
            "type mismatch B<i64> does not contain B<_>: it holds the type of `t`",
        ),
        Accept("let v = B(1)"),
        Refuse(
            "v <- never<B<i64>>()",
            "v is B<_>, settled when its input ended, and cannot hold B<i64>; \
             declare v: B<i64> where it is bound",
        ),
    ])
    .await
}

#[tokio::test(flavor = "current_thread")]
async fn annotated_abstract_parameter_is_tested() -> Result<()> {
    session(&[
        Accept("type B<'x> = Abstract<i64>"),
        Accept("let t: B<i64> = B(1)"),
        Produce("select t { B<i64> as _ => 1 }", Value::I64(1)),
        Accept("let f = |x: B<i64>| 2"),
        Produce("f(t)", Value::I64(2)),
    ])
    .await
}

/// A refused input changes no cell of an earlier input: the binding it
/// would have tied to its quantifier still reads as its own input left
/// it.
#[tokio::test(flavor = "current_thread")]
async fn refused_input_leaves_earlier_types() -> Result<()> {
    session(&[
        Accept("let y = []"),
        Refuse(
            "let p = 'b: Any |x: 'b| -> 'b { y <- [x]; x }",
            "y is Array<_>, settled when its input ended, and cannot hold",
        ),
        Refuse("y <- [1]", "y is Array<_>, settled when its input ended"),
    ])
    .await
}

#[tokio::test(flavor = "current_thread")]
async fn refused_input_leaves_an_annotated_binding() -> Result<()> {
    session(&[
        Accept("let y: Array<i64> = []"),
        Refuse(
            "let p = 'b: Any |x: 'b| -> 'b { y <- [x]; x }",
            "y is Array<i64> and cannot hold",
        ),
        Accept("y <- [1]"),
        Produce("y", Value::Array([Value::I64(1)].into())),
    ])
    .await
}

const SETF: &str =
    "let setf = |p: &mut fn(x: i64) -> i64| { *p <- |x: i64| -> i64 x + 100; null }";

/// A function read out of a generic holder is generic to the inputs after
/// its own: each later use copies it and decides nothing of it, and a
/// writable reference to it, which would let a write narrow it, is refused.
/// What an earlier input left open is one type to a later input, whose
/// decision about it is refused, naming the binding.
#[tokio::test(flavor = "current_thread")]
async fn earlier_value_is_decided_by_no_later_use() -> Result<()> {
    session(&[
        Accept("let s = {g: 'a: Number |x: 'a| -> 'a x + x}"),
        Accept("let j = s.g"),
        Produce("j(1)", Value::I64(2)),
        Produce("j(1.5)", Value::F64(3.0)),
        Accept("let app = |f: fn(x: i64) -> i64| f(1)"),
        Produce("app(j)", Value::I64(2)),
        Accept("let arr = [j, |x: i64| -> i64 x]"),
        Produce("arr[1]$(4)", Value::I64(4)),
        Accept("let kj = select true { true => j, false => |x: i64| -> i64 x }"),
        Produce("kj(3)", Value::I64(6)),
        Accept(SETF),
        Refuse(
            "setf(&mut j)",
            "j was made generic by an earlier compile, and a writable reference to it \
             would let a write narrow it",
        ),
        Produce("j(2.5)", Value::F64(5.0)),
        Accept("let g = 'q: Number |x, y: 'q| -> 'q x + y"),
        Accept("let fs = [g, g]"),
        Refuse(
            "let z: fn(x: i64, y: i64) -> i64 = fs[0]$",
            "this input would decide a type of an earlier input: annotate `fs` (line 1, \
             column 5 in `let fs = [g, g]`)",
        ),
        Refuse(
            "fs[1]$(1.5, 2.5)",
            "this input would decide a type of an earlier input: annotate `fs` (line 1, \
             column 5 in `let fs = [g, g]`)",
        ),
    ])
    .await
}

#[tokio::test(flavor = "current_thread")]
async fn annotated_earlier_value_takes_a_later_use() -> Result<()> {
    session(&[
        Accept("let s = {g: 'a: Number |x: 'a| -> 'a x + x}"),
        Accept("let j: fn(x: i64) -> i64 = s.g"),
        Accept("let app = |f: fn(x: i64) -> i64| f(1)"),
        Produce("app(j)", Value::I64(2)),
        Accept("let arr = [j, |x: i64| -> i64 x]"),
        Produce("arr[0]$(1)", Value::I64(2)),
        Accept("let kj = select true { true => j, false => |x: i64| -> i64 x }"),
        Produce("kj(1)", Value::I64(2)),
        Accept(SETF),
        Accept("setf(&mut j)"),
        Produce("j(1)", Value::I64(101)),
        Accept("let g = 'q: Number |x, y: 'q| -> 'q x + y"),
        Accept("let fs: Array<fn(x: i64, y: i64) -> i64> = [g, g]"),
        Accept("let z: fn(x: i64, y: i64) -> i64 = fs[0]$"),
        Produce("z(1, 2)", Value::I64(3)),
    ])
    .await
}

/// A write as general as an earlier binding's function holds it rigid for
/// the comparison and decides nothing of it; one narrower is refused.
#[tokio::test(flavor = "current_thread")]
async fn write_as_general_decides_nothing() -> Result<()> {
    session(&[
        Accept("let s = {g: 'a: Number |x: 'a| -> 'a x + x}"),
        Accept("let j = s.g"),
        Accept("j <- s.g"),
        Produce("j(1.5)", Value::F64(3.0)),
        Produce("j(1)", Value::I64(2)),
        Accept("let s2 = {g: |x| x}"),
        Accept("let j2 = s2.g"),
        Refuse(
            "j2 <- |x: i64| -> i64 x",
            "made generic by an earlier compile, so what is written to it must be as \
             general",
        ),
        Produce("j2(\"s\")", Value::String(arcstr::literal!("s"))),
    ])
    .await
}

/// A write through a reference to an earlier input's binding that would
/// decide its type is refused, naming the binding that holds the type: a
/// function read out of a holder is one instance, settled when its input
/// ended.
#[tokio::test(flavor = "current_thread")]
async fn write_through_a_reference_decides_no_earlier_type() -> Result<()> {
    const J0: &str = "it holds the type of `j0` (line 1, column 20 in `let (j, r) = { let \
                      j0 = s.g; (j0, &mut j..`), settled when its input ended";
    session(&[
        Accept("let s = {g: |x| x}"),
        Accept("let (j, r) = { let j0 = s.g; (j0, &mut j0) }"),
        Accept(SETF),
        Refuse("setf(r)", J0),
        Refuse("{ let r2 = r; *r2 <- |x: i64| -> i64 x + 1; null }", J0),
        Refuse("j(\"s\")", J0),
        Accept("let q = &mut { let n = 1; s.g }"),
        Refuse(
            "setf(q)",
            "it holds the type of `q` (line 1, column 5 in `let q = &mut { let n = 1; \
             s.g }`), settled when its input ended",
        ),
        Accept("type H<'a> = Abstract<&mut 'a>"),
        Accept("let h = H(&mut { let n = 1; |x| x })"),
        Refuse(
            "setf(h.0)",
            "it holds the type of `h` (line 1, column 5 in `let h = H(&mut { let n = \
             1; |x| x })`), settled when its input ended",
        ),
    ])
    .await
}

#[tokio::test(flavor = "current_thread")]
async fn annotated_written_value_takes_a_write() -> Result<()> {
    session(&[
        Accept("let s = {g: |x| x}"),
        Accept("let (j, r) = { let j0: fn(x: i64) -> i64 = s.g; (j0, &mut j0) }"),
        Accept(SETF),
        Accept("setf(r)"),
        Produce("j(1)", Value::I64(101)),
        Accept("let q: &mut fn(x: i64) -> i64 = &mut { let n = 1; s.g }"),
        Accept("setf(q)"),
        Produce("(*q)(2)", Value::I64(102)),
        Accept("type H<'a> = Abstract<&mut 'a>"),
        Accept("let h: H<fn(x: i64) -> i64> = H(&mut { let n = 1; |x| x })"),
        Accept("setf(h.0)"),
        Produce("(*h.0)(3)", Value::I64(103)),
    ])
    .await
}

const WRITTEN_MODULE: &str =
    "let k = super::s.g;\nlet j = k;\nlet w = |v| { k <- v; null }";

/// A function a module reads out of a holder into a `<-` target is one
/// instance, settled when the module's input ended: a later input that would
/// decide it is refused, naming the binding that holds it, and a refused
/// input decides nothing of it.
#[tokio::test(flavor = "current_thread")]
async fn refused_input_decides_no_earlier_open_cell() -> Result<()> {
    const K: &str = "it holds the type of `k` (line 1, column 5 in module /m), settled \
                     when its input ended";
    session_over(
        &[("/m.gx", WRITTEN_MODULE)],
        &[
            Accept("let s = {g: |x| x}"),
            Accept("mod m"),
            Refuse("{ let q: fn(x: i64) -> i64 = m::k; let bad: string = 1; q }", K),
            Refuse("m::k(\"s\")", K),
            Refuse("let z: fn(x: i64) -> i64 = m::k", K),
            Refuse("let z: fn(x: i64) -> i64 = m::j", K),
        ],
    )
    .await
}

#[tokio::test(flavor = "current_thread")]
async fn annotated_module_binding_takes_a_later_use() -> Result<()> {
    session_over(
        &[(
            "/m.gx",
            "let k: fn(x: i64) -> i64 = super::s.g;\nlet j = k;\n\
             let w = |v| { k <- v; null }",
        )],
        &[
            Accept("let s = {g: |x| x}"),
            Accept("mod m"),
            Accept("let z: fn(x: i64) -> i64 = m::j"),
            Produce("z(4)", Value::I64(4)),
        ],
    )
    .await
}

/// A callback's cell an earlier input left in a generic function's type is
/// decided by no later input: it settled when its input ended.
#[tokio::test(flavor = "current_thread")]
async fn later_input_decides_no_leaked_callback_cell() -> Result<()> {
    session(&[
        Accept("type W<'t> = Abstract<'t>"),
        Accept(
            "let h = { let y0 = []; let g0 = |x| { y0 <- array::map([x], |e| W(e)); \
             x }; |u| (u, y0, g0) }",
        ),
        Refuse(
            "let w: (i64, Array<W<string>>, i64) = (0, h(0).1, 1)",
            "it holds the type of `y0` (line 1, column 15 in `let h = { let y0 = []; \
             let g0 = |x| { y0..`), settled when its input ended",
        ),
    ])
    .await
}

#[tokio::test(flavor = "current_thread")]
async fn annotated_callback_result_takes_a_later_use() -> Result<()> {
    session(&[
        Accept("type W<'t> = Abstract<'t>"),
        Accept(
            "let h = { let y0: Array<W<i64>> = []; let g0 = |x| { y0 <- \
             array::map([x], |e| W(e)); x }; |u| (u, y0, g0) }",
        ),
        Refuse("let w: (i64, Array<W<string>>, i64) = (0, h(0).1, 1)", "type mismatch"),
        Accept("let w: (i64, Array<W<i64>>, i64) = (0, h(0).1, 1)"),
        Produce("h(0).2(5)", Value::I64(5)),
        Produce("w.1[0]$.0 + 1", Value::I64(6)),
    ])
    .await
}

/// A write as general as a binding of a function settled at ⊥ decides
/// nothing of an earlier input.
#[tokio::test(flavor = "current_thread")]
async fn write_as_general_into_a_settled_binding() -> Result<()> {
    session(&[
        Accept("let g = select true { true => |x| x, false => |x| x }"),
        Accept("g <- |x| x"),
        Refuse("g(1)", "type mismatch _ does not contain i64: it holds the type of `g`"),
        Refuse(
            "g <- |x: i64| x",
            "g is fn(x: _) -> _, settled when its input ended, and cannot hold fn(x: i64) \
             -> i64; declare g",
        ),
    ])
    .await
}

#[tokio::test(flavor = "current_thread")]
async fn annotated_settled_binding_takes_a_write() -> Result<()> {
    session(&[
        Accept(
            "let g: fn(x: i64) -> i64 = select true { true => |x| x, false => |x| x }",
        ),
        Accept("g <- |x| x + 1"),
        Produce("g(1)", Value::I64(2)),
    ])
    .await
}

/// A refusal over a settled ⊥ in the value it checks, not the type it
/// checks against, names the binding too: a comparison, a writable
/// reference, a cell no name binds.
#[tokio::test(flavor = "current_thread")]
async fn refusals_over_a_settled_value_name_the_binding() -> Result<()> {
    session(&[
        Accept("let a = []"),
        Refuse(
            "a == [1]",
            "both operands must be one type: it holds the type of `a` (line 1, column 5 \
             in `let a = []`), settled when its input ended",
        ),
        Accept("let e = []"),
        Accept("let f = |x: &mut Array<i64>| { *x <- [1]; null }"),
        Refuse(
            "f(&mut e)",
            "type mismatch &mut Array<i64> does not contain &mut Array<_>: it holds the \
             type of `e`",
        ),
        Accept("let r = &mut never()"),
        Refuse(
            "*r <- 1",
            "type mismatch _ does not contain i64: it holds the type of `r` (line 1, \
             column 5 in `let r = &mut never()`)",
        ),
        Accept("let fs = [|x| x, |y| y]"),
        Accept("let z = fs[0]$"),
        Accept("let app = |f: fn(x: i64) -> i64| f(1)"),
        Refuse(
            "app(z)",
            "type mismatch fn(x: i64) -> i64 does not contain fn(y: _) -> _: it holds the \
             type of `fs`",
        ),
    ])
    .await
}

#[tokio::test(flavor = "current_thread")]
async fn annotated_values_pass_where_a_settled_one_is_named() -> Result<()> {
    session(&[
        Accept("let a: Array<i64> = []"),
        Produce("a == [1]", Value::Bool(false)),
        Accept("let e: Array<i64> = []"),
        Accept("let f = |x: &mut Array<i64>| { *x <- [1]; null }"),
        Accept("f(&mut e)"),
        Produce("e", Value::Array([Value::I64(1)].into())),
        Accept("let r: &mut i64 = &mut never()"),
        Accept("*r <- 1"),
        Produce("*r", Value::I64(1)),
        Accept("let fs: Array<fn(x: i64) -> i64> = [|x| x, |y| y]"),
        Accept("let z = fs[0]$"),
        Accept("let app = |f: fn(x: i64) -> i64| f(1)"),
        Produce("app(z)", Value::I64(1)),
    ])
    .await
}

/// The note naming a settled binding goes only on a refusal its settled ⊥
/// is the cause of: declaring the binding would lift no other.
#[tokio::test(flavor = "current_thread")]
async fn settled_note_goes_where_the_settled_part_refuses() -> Result<()> {
    session(&[
        Accept("let t = (1, [])"),
        RefuseEnding(
            "let u: (string, Array<i64>) = t",
            "type mismatch (string, Array<i64>) does not contain (i64, Array<_>)",
        ),
        Accept("let f = |x: &mut (i64, Array<i64>)| { *x <- (1, [2]); null }"),
        Refuse(
            "f(&mut t)",
            "does not contain &mut (i64, Array<_>): it holds the type of `t` (line 1, \
             column 5 in `let t = (1, [])`), settled",
        ),
    ])
    .await
}

/// A select over members whose parts its input left open judged them
/// apart by those parts as they stood: a later input may not decide them.
#[tokio::test(flavor = "current_thread")]
async fn select_judgment_holds_across_inputs() -> Result<()> {
    session(&[
        Accept("let b = true"),
        Accept("let e1 = []"),
        Accept("let e2 = []"),
        Accept(
            "let v = select b { true => {a: [1], c: e1}, false => {a: [\"s\"], c: e2} }",
        ),
        Accept("let r = select v { {a, ..} => \"[a]\" }"),
        Refuse(
            "e1 <- [1]",
            "e1 is Array<_>, settled when its input ended, and cannot hold Array<i64>; \
             declare e1: Array<i64> where it is bound",
        ),
    ])
    .await
}

// A load is checked after the program that loads it: a binding the
// program left at ⊥ holds what the loaded body writes to ⊥, so a generic
// val over such a write is refused.
const LOAD_WRITE_HELD: &str = r#"{
    let y = never();
    let status = mod d dynamic {
        sandbox unrestricted;
        sig { val h: fn(x: 'a) -> 'a };
        source "let h = |x| { super::y <- x; x }"
    };
    select status { error as e => "[e]", null as _ => "loaded" }
}"#;

run!(
    load_write_is_held_to_the_program_binding,
    LOAD_WRITE_HELD,
    |v: anyhow::Result<&Value>| matches!(
        v,
        Ok(Value::String(s))
            if s.contains("val h is declared fn(x: 'a) -> 'a but implemented fn(x: _) -> _")
                && s.contains("it holds the type of `y`")
    );
    FuseExpect::Jit
);

const LOAD_WRITE_ANNOTATED: &str = r#"{
    let y: i64 = never();
    let status = mod d dynamic {
        sandbox unrestricted;
        sig { val h: fn(x: i64) -> i64; val g: fn(u: i64) -> i64 };
        source "let h = |x| { super::y <- x; x }; let g = |u: i64| -> i64 u + super::y"
    };
    select status { error as e => never(dbg(e)), null as _ => d::g(d::h(5)) }
}"#;

run!(
    load_write_to_an_annotated_binding,
    LOAD_WRITE_ANNOTATED,
    is(Value::I64(10));
    FuseExpect::None
);

const STATUS: &str = r#"select st { error as e => "[e]", null as _ => "loaded" }"#;

/// A dynamic load is an input of its own: a decision about a type an
/// earlier input settled refuses it, naming the binding that holds the type,
/// and a refused load decides nothing. A ⊥ the earlier input settled reads
/// as any type.
#[tokio::test(flavor = "current_thread")]
async fn load_decides_no_earlier_open_cell() -> Result<()> {
    const K: &str = "it holds the type of `k` (line 1, column 5 in module /m), settled \
                     when its input ended";
    session_over(
        &[("/m.gx", WRITTEN_MODULE)],
        &[
            Accept("type W<'t> = Abstract<'t>"),
            Accept("let unw = |w: W<'a>| -> 'a w.0"),
            Accept(
                "let p = { let y = never(); let g = |x| { y <- array::map([x], |e| \
                 W(e)); x }; y }",
            ),
            Accept(
                r##"let st = mod d dynamic {
                    sandbox unrestricted;
                    sig { val s: fn(u: i64) -> string };
                    source r#"let s = |u: i64| -> string super::unw(super::p[0]$)"#
                }"##,
            ),
            ProduceText(STATUS, "loaded"),
            Accept("let s = {g: |x| x}"),
            Accept("mod m"),
            Accept(
                r#"let st = mod e dynamic {
                    sandbox unrestricted;
                    sig { val q: fn(x: i64) -> i64 };
                    source "let q: fn(x: i64) -> i64 = super::m::k; let bad: string = 1"
                }"#,
            ),
            ProduceText(STATUS, K),
            Refuse("m::k(\"s\")", K),
        ],
    )
    .await
}

#[tokio::test(flavor = "current_thread")]
async fn load_takes_an_annotated_earlier_binding() -> Result<()> {
    session_over(
        &[(
            "/m.gx",
            "let k: fn(x: i64) -> i64 = super::s.g;\nlet j = k;\n\
             let w = |v| { k <- v; null }",
        )],
        &[
            Accept("type W<'t> = Abstract<'t>"),
            Accept("let unw = |w: W<'a>| -> 'a w.0"),
            Accept(
                "let p = { let y: Array<W<i64>> = never(); let g = |x| { y <- \
                 array::map([x], |e| W(e)); x }; let a = g(41); y }",
            ),
            Accept(
                r##"let st = mod d dynamic {
                    sandbox unrestricted;
                    sig { val s: fn(u: i64) -> i64 };
                    source r#"let s = |u: i64| -> i64 super::unw(super::p[0]$) + u"#
                }"##,
            ),
            ProduceText(STATUS, "loaded"),
            Produce("d::s(1)", Value::I64(42)),
            Accept("let s = {g: |x| x}"),
            Accept("mod m"),
            Accept(
                r#"let st = mod f dynamic {
                    sandbox unrestricted;
                    sig { val q: fn(x: i64) -> i64 };
                    source "let q: fn(x: i64) -> i64 = super::m::k"
                }"#,
            ),
            ProduceText(STATUS, "loaded"),
            Produce("f::q(4)", Value::I64(4)),
        ],
    )
    .await
}

// A load is an input after the program that loads it: the cell the
// program left in `p` is one instance, settled at ⊥, which the load reads
// as any type.
const LOAD_DECIDES_PROGRAM_TYPE: &str = r##"{
    type W<'t> = Abstract<'t>;
    let unw = |w: W<'a>| -> 'a w.0;
    let p = { let y = never(); let g = |x| { y <- array::map([x], |e| W(e)); x }; y };
    let status = mod d dynamic {
        sandbox unrestricted;
        sig { val s: fn(u: i64) -> string };
        source r#"let s = |u: i64| -> string super::unw(super::p[0]$)"#
    };
    select status { error as e => "[e]", null as _ => "loaded" }
}"##;

run!(
    load_decides_no_type_the_program_left_open,
    LOAD_DECIDES_PROGRAM_TYPE,
    |v: anyhow::Result<&Value>| matches!(v, Ok(Value::String(s)) if s == "loaded");
    FuseExpect::Jit
);

const LOAD_READS_ANNOTATED_PROGRAM: &str = r##"{
    type W<'t> = Abstract<'t>;
    let unw = |w: W<'a>| -> 'a w.0;
    let p = {
        let y: Array<W<i64>> = never();
        let g = |x| { y <- array::map([x], |e| W(e)); x };
        let a = g(41);
        y
    };
    let status = mod d dynamic {
        sandbox unrestricted;
        sig { val s: fn(u: i64) -> i64 };
        source r#"let s = |u: i64| -> i64 super::unw(super::p[0]$) + u"#
    };
    select status { error as e => never(dbg(e)), null as _ => d::s(1) }
}"##;

run!(
    load_reads_an_annotated_program_binding,
    LOAD_READS_ANNOTATED_PROGRAM,
    is(Value::I64(42));
    FuseExpect::Jit
);
