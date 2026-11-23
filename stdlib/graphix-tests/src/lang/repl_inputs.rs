// A REPL input is checked and run alone: at its end every cell it leaves
// open settles, ⊥ where nothing bounds it, and a later input that would
// decide one is refused, naming the binding to annotate; each refusal is
// paired with the annotation that lifts it. A dynamic module's load is an
// input of the same kind, after the program that loads it.

use anyhow::{Context, Result, bail};
use arcstr::ArcStr;
use graphix_compiler::expr::{VfsEntry, VfsResolver};
use graphix_package_core::{
    run,
    testing::{FuseExpect, TestCtx, config, is, next_update_raw},
};
use netidx::publisher::Value;

#[derive(Clone)]
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
        // CR claude for claude: [test-gap/medium] A `Produce`/`ProduceText` step skips
        // every update that is not the wanted value, so it passes when the right value
        // arrives after wrong ones. `loop_over_an_earlier_bottom_union_of_arrays`
        // relies on its loop staying bottom. If the later input's loop fired over the
        // earlier bottom (the source taken as an empty array), `any` would deliver 0
        // and then 7, this loop would skip the 0, and the test would pass. Its
        // one-input sibling
        // `fusion_review::hof_over_a_bottom_union_of_arrays_is_bottom` holds the first
        // update and would fail. Hold each Produce step to the input's first update, as
        // run!'s predicates are held. Nothing here asserts the "runs fused" the test's
        // comment claims either. Found by reading; not run. (tests-lang-rest-c-03)
        loop {
            let v = next_update_raw(&ctx.rt, &mut rx, id)
                .await
                .with_context(|| format!("{input} never produced its value"))?;
            if produced(step, &v) {
                break;
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

/// A later input's loop over an earlier input's bottom, whose type is a
/// union of arrays, runs fused and stays bottom.
/// A catch entered alone has seen no error when its input ends: one that
/// reads its binder is refused a later input's raise, naming the
/// annotation, and an annotated one takes it, its binder the type written.
#[tokio::test(flavor = "current_thread")]
async fn catch_entered_alone_takes_a_later_raise_by_its_annotation() -> Result<()> {
    session(&[
        Accept("let a = [1, 2, 3]"),
        Accept("catch(e) println(e)"),
        Refuse(
            "a[15]?",
            "annotate the catch with the errors it takes, \
             `catch(e: Error<ErrChain<`ArrayIndexError(string)>>)`",
        ),
    ])
    .await?;
    session(&[
        Accept("let a = [1, 2, 3]"),
        Accept("let seen = \"\""),
        Accept(
            "catch(e: Error<ErrChain<`ArrayIndexError(string)>>) \
             seen <- select e.0.error { `ArrayIndexError(s) => s }",
        ),
        Accept("a[15]?"),
        ProduceText("seen", "out of bounds"),
    ])
    .await
}

#[tokio::test(flavor = "current_thread")]
async fn loop_over_an_earlier_bottom_union_of_arrays() -> Result<()> {
    session(&[
        Accept("let f = |x| select x { [a] => [a], [] => [], _ => never() }"),
        Accept("let r = f([1, 2])"),
        Produce(
            "any(array::len(array::map(r, |v| v + 1)), sys::time::after_idle(duration:10.ms, 7))",
            Value::I64(7),
        ),
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

/// A call through a builtin binding a later input wrote is made at run
/// time, where its unread quantifier settles to ⊥ as at a direct call.
#[tokio::test(flavor = "current_thread")]
async fn builtin_alias_written_by_an_earlier_input() -> Result<()> {
    session(&[
        Accept("let t = sys::net::publish"),
        Accept("t <- sys::net::publish"),
        Accept("t(never(), 10)"),
    ])
    .await?;
    session(&[
        Accept("let t = sys::net::publish"),
        Accept("let f = t"),
        Refuse("t <- sys::net::publish", "t cannot be written"),
    ])
    .await
}

/// A constructed abstract value's parameter its input left open is ⊥,
/// which the representation only reads: the value is every instance's, in
/// a later type test and a later call alike, but a binding of it takes no
/// wider instance.
#[tokio::test(flavor = "current_thread")]
async fn abstract_parameter_settles_at_its_input() -> Result<()> {
    session(&[
        Accept("type B<'x> = Abstract<i64>"),
        Accept("let t = B(1)"),
        Produce("select t { B<i64> as _ => 1 }", Value::I64(1)),
        Accept("let f = |x: B<i64>| 1"),
        Produce("f(t)", Value::I64(1)),
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
            "type mismatch i64 does not contain 'b",
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
/// decision about it is refused: a select's one instance of `j`. An array of
/// generic names is generic: each later use of an element copies it.
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
        Refuse("kj(3)", "type mismatch _ does not contain i64"),
        Accept(SETF),
        Refuse(
            "setf(&mut j)",
            "j is generic, and a writable reference to it \
             would let a write narrow it",
        ),
        Produce("j(2.5)", Value::F64(5.0)),
        Accept("let g = 'q: Number |x, y: 'q| -> 'q x + y"),
        Accept("let fs = [g, g]"),
        Accept("let z: fn(x: i64, y: i64) -> i64 = fs[0]$"),
        Produce("z(1, 2)", Value::I64(3)),
        Produce("fs[1]$(1.5, 2.5)", Value::F64(4.0)),
    ])
    .await
}

/// An array of generic names is read at any type. A binding something
/// writes holds one instance of the generic function it reads: the refusal
/// of a later decision about it names the type to declare, and only the
/// binding the cell came from.
#[tokio::test(flavor = "current_thread")]
async fn refusal_names_the_declaration_of_the_origin() -> Result<()> {
    session(&[
        Accept("let g = 'q: Number |x: 'q| -> 'q x"),
        Accept("let j = [g, g]"),
        Accept("let r = &j"),
        Accept("let (k, w) = { let a = [g, g]; (a, |f| { a <- f; null }) }"),
        Produce("array::map([1.5], j[0]$)", Value::Array([Value::F64(1.5)].into())),
        RefuseEnding(
            "let z: fn(x: i64) -> i64 = k[0]$",
            "annotate `a` (line 1, column 20 in `let (k, w) = { let a = [g, g]; (a, |f| {..`) \
             as `Array<fn(x: i64) -> i64>`",
        ),
        Produce("array::len(j)", Value::I64(2)),
    ])
    .await
}

/// Each array of generic names a later input reads is copied at the types
/// that input gives its element, and an annotated array takes the same reads.
#[tokio::test(flavor = "current_thread")]
async fn arrays_of_generic_names_are_generic_to_later_inputs() -> Result<()> {
    session(&[
        Accept("let h = 'p: Number, 'q: Number |a: 'p, b: 'q| -> ('p, 'q) (a, b)"),
        Accept("let g = 'q: Number |x: 'q| -> 'q x"),
        Accept("let j = [h, h]"),
        Accept("let k = [g, g]"),
        Accept(
            "let both = |f: fn(a: i64, b: f64) -> (i64, f64), h: fn(x: i64) -> i64| \
             (f(1, 2.0), h(2))",
        ),
        Produce("both(j[0]$, k[0]$).0.1", Value::F64(2.0)),
        Produce("both(j[0]$, k[0]$).1", Value::I64(2)),
        Accept("let j2: Array<fn(a: i64, b: f64) -> (i64, f64)> = [h, h]"),
        Accept("let k2: Array<fn(x: i64) -> i64> = [g, g]"),
        Produce("both(j2[0]$, k2[0]$).1", Value::I64(2)),
    ])
    .await
}

#[tokio::test(flavor = "current_thread")]
async fn annotated_origin_takes_the_declared_use() -> Result<()> {
    session(&[
        Accept("let s = {g: |x| x}"),
        Accept("let j: fn(x: string) -> string = s.g"),
        Accept("let r = &j"),
        Produce("array::map([\"a\"], j)[0]", Value::String(arcstr::literal!("a"))),
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

/// The comparison of a write with a generic binding leaves nothing for the
/// input's end to decide: a `Concrete` variable no value reaches stands.
#[tokio::test(flavor = "current_thread")]
async fn write_as_general_of_a_concrete_variable() -> Result<()> {
    session(&[
        Accept("let p = 'a: Concrete |#h: [fn(v: 'a) -> i64, null] = null, x: i64| x"),
        Accept("p <- 'a: Concrete |#h: [fn(v: 'a) -> i64, null] = null, x: i64| x + 1"),
        Produce("p(3)", Value::I64(4)),
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
            "a generic binding, so what is written to it must be as \
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

/// A callback's cell an earlier input left in a generic function's type
/// settled at ⊥ when its input ended: a later input reads it at any type and
/// decides it at none, so the callback takes no call.
#[tokio::test(flavor = "current_thread")]
async fn leaked_callback_cell_reads_at_any_type_and_takes_no_call() -> Result<()> {
    session(&[
        Accept("type W<'t> = Abstract<'t>"),
        Accept(
            "let h = { let y0 = []; let g0 = |x| { y0 <- array::map([x], |e| W(e)); \
             x }; |u| (u, y0, g0) }",
        ),
        Accept("let w: (i64, Array<W<string>>, i64) = (0, h(0).1, 1)"),
        Accept("let w2: (i64, Array<W<i64>>, i64) = (0, h(0).1, 1)"),
        Refuse("h(0).2(5)", "type mismatch _ does not contain i64"),
        Refuse("h(0).2(\"a\")", "type mismatch _ does not contain string"),
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

/// A function that writes its own parameter beside an argument whose
/// payload an earlier input settled takes both: nothing reaches the
/// argument's binding.
#[tokio::test(flavor = "current_thread")]
async fn parameter_write_beside_a_settled_payload_takes_both() -> Result<()> {
    session(&[
        Accept("type B<'p> = Abstract<'p>"),
        Accept("let f = |a, z| { a <- B([z]); a.0 }"),
        Accept("let x = B([])"),
        Produce("f(x, 1)", Value::Array([Value::I64(1)].into())),
    ])
    .await
}

#[tokio::test(flavor = "current_thread")]
async fn declared_payload_takes_what_a_function_builds_into_it() -> Result<()> {
    session(&[
        Accept("type B<'p> = Abstract<'p>"),
        Accept("let f = |a, z| { a <- B([z]); a.0 }"),
        Accept("let x: B<Array<i64>> = B([])"),
        Produce("f(x, 1)", Value::Array([].into())),
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

/// A settled ⊥ compares with any type. A refusal over a settled ⊥ in the
/// value it checks, not the type it checks against, names the binding too: a
/// writable reference, a cell no name binds.
#[tokio::test(flavor = "current_thread")]
async fn refusals_over_a_settled_value_name_the_binding() -> Result<()> {
    session(&[
        Accept("let a = []"),
        Produce("a == [1]", Value::Bool(false)),
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
            "type mismatch fn(x: i64) -> i64 does not contain fn(x: _) -> _: it holds the \
             type of `z`",
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
// CR claude for claude: [test-gap/low] The file's header says each refusal is paired
// with the annotation that lifts it, but this one has no twin. Neither do
// `builtin_alias_written_by_an_earlier_input` (a typed handler),
// `load_names_every_cell_it_would_decide` and
// `returned_default_of_a_returned_function_is_judged_in_a_later_input`. CLAUDE.md asks
// a refusing rule to pin the annotation that makes the program pass. Without the twin,
// a change that also refused the annotated program would go unseen. Add each twin (here
// `let e1: Array<i64> = []`, the same select, then `e1 <- [1]` accepted), or narrow the
// header. Found by reading; not run. (tests-lang-rest-c-08)
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

/// A ladder of three slice arms over an unannotated parameter leaves the
/// arms' one element cell open, with no link to an arm's copy: it stands
/// in the result's union where the written twin's variable does, so a
/// later input reads both results alike.
#[tokio::test(flavor = "current_thread")]
async fn slice_arms_result_reads_as_the_written_twins_in_a_later_input() -> Result<()> {
    let read = Produce("array::map(r, |v| v + 1)", Value::Array([Value::I64(4)].into()));
    session(&[
        Accept("let f = |x| select x { [a, b, t..] => t, [a, t..] => t, [] => [] }"),
        Accept("let r = f([1, 2, 3])"),
        read.clone(),
    ])
    .await?;
    session(&[
        Accept(
            "let f = |x: Array<'e>| select x { [a, b, t..] => t, [a, t..] => t, [] => [] }",
        ),
        Accept("let r = f([1, 2, 3])"),
        read,
    ])
    .await
}

/// The element cell of an unannotated parameter that an arm's narrowing
/// left forwarding to a copy sorts as the cell it stands for, so the
/// result's members stand as the written twin's do and a later input
/// reads both alike: the variable takes the decided member first, and
/// holds the other.
#[tokio::test(flavor = "current_thread")]
async fn forwarded_cell_stands_among_members_as_the_written_twins() -> Result<()> {
    for (f, read, len) in [
        ("let f = |x| select x { [a, ..] => [a], [] => [] }", "array::len(r)", 1),
        (
            "let f = |x: Array<'e>| select x { [a, ..] => [a], [] => [] }",
            "array::len(r)",
            1,
        ),
        (
            "let f = |x| select x { [a, ..] => [[a]], [] => [[]] }",
            "array::fold(r, 0, |n, v| n + array::len(v))",
            1,
        ),
        (
            "let f = |x: Array<'e>| select x { [a, ..] => [[a]], [] => [[]] }",
            "array::fold(r, 0, |n, v| n + array::len(v))",
            1,
        ),
    ] {
        session(&[Accept(f), Accept("let r = f([1])"), Produce(read, Value::I64(len))])
            .await?;
    }
    Ok(())
}

/// With the arms the other way a later input reads both forms alike: no
/// member of the result's union stands first.
#[tokio::test(flavor = "current_thread")]
async fn settled_member_first_reads_as_the_written_twins() -> Result<()> {
    for f in [
        "let f = |x| select x { [] => [], [a, ..] => [a] }",
        "let f = |x: Array<'e>| select x { [] => [], [a, ..] => [a] }",
    ] {
        session(&[
            Accept(f),
            Accept("let r = f([1])"),
            Produce("array::len(r)", Value::I64(1)),
        ])
        .await?;
    }
    Ok(())
}

/// A parameter its arms narrowed, read whole beside an arm's `[]`, reads
/// the same in either order of the arms, annotated or not: a later input's
/// read takes its elements.
#[tokio::test(flavor = "current_thread")]
async fn narrowed_parameter_read_whole_reads_in_either_order_of_the_arms() -> Result<()> {
    for (f, r) in [
        (
            "let f = |k: i64, p| { let n = select p { [h, ..] => 1, [] => 0 }; select k { 0 => [], _ => p } }",
            "let r = f(1, [2])",
        ),
        (
            "let f = |k: i64, p| { let n = select p { [h, ..] => 1, [] => 0 }; select k { 0 => p, _ => [] } }",
            "let r = f(0, [2])",
        ),
        (
            "let f = |k: i64, p| select k { 0 => select p { [h, ..] => [h], [] => [] }, _ => p }",
            "let r = f(1, [2])",
        ),
        (
            "let f = |k: i64, p: Array<'e>| { let n = select p { [h, ..] => 1, [] => 0 }; select k { 0 => p, _ => [] } }",
            "let r = f(0, [2])",
        ),
        (
            "let f = |k: i64, p: Array<'e>| { let n = select p { [h, ..] => 1, [] => 0 }; select k { 0 => [], _ => p } }",
            "let r = f(1, [2])",
        ),
    ] {
        session(&[Accept(f), Accept(r), Produce("array::len(r)", Value::I64(1))]).await?;
    }
    Ok(())
}

/// A member whose cell an input's end settled to ⊥ reads as the type of
/// the member beside it where a later input builds a union of its own (the
/// first parts of a union of tuples), as it does where a call's result
/// holds it.
#[tokio::test(flavor = "current_thread")]
async fn settled_member_read_out_of_a_tuple_reads_as_the_one_met_whole() -> Result<()> {
    session(&[
        Accept(
            "let g = |k: i64, p0: Array<'a>, p1: Array<'b>| select k { 0 => p0, _ => p1 }",
        ),
        Accept("let s = g(1, [], [\"s\"])"),
        Produce("array::len(s)", Value::I64(1)),
        Accept(
            "let f = |k: i64, p0: Array<'a>, p1: Array<'b>, p2: Array<'c>| select k { 0 => (p0, [p1]), _ => (p1, p2) }",
        ),
        Accept("let r = f(1, [], [\"s\", \"s\"], [\"t\"])"),
        Accept("let m = r.0"),
        Produce("array::len(m)", Value::I64(2)),
        Accept("let m: Array<string> = r.0"),
        Produce("array::len(m)", Value::I64(2)),
    ])
    .await
}

/// Lambdas of two inputs that meet in a third keep their own defaults: a
/// call that omits the label passes the default of the function it calls.
#[tokio::test(flavor = "current_thread")]
async fn functions_of_two_inputs_met_keep_their_own_defaults() -> Result<()> {
    session(&[
        Accept("let z = |#d = 1, x: i64| x"),
        Accept("let a = |#d = \"q\", x: i64| x"),
        Produce("array::len([a, z])", Value::I64(2)),
        Produce("select true { true => a, false => z }(7)", Value::I64(7)),
    ])
    .await
}

/// A function an earlier input's call returns is called in a later input
/// with a default omitted: the default is judged at that call's check
/// from the definition the earlier input left, so a cell the earlier
/// input settled is refused there, naming the binding.
#[tokio::test(flavor = "current_thread")]
async fn returned_default_is_judged_in_a_later_input() -> Result<()> {
    session(&[
        Accept("let mk = |k: i64| |#d = 1, x: i64| cast<string>(d)$"),
        Accept("let b = mk(4)"),
        Refuse(
            "b(7)",
            "type mismatch _ does not contain i64: it holds the type of `b` (line 1, \
             column 5 in `let b = mk(4)`), settled when its input ended; declare b's \
             type where it is bound",
        ),
        Produce("mk(4)(7)", Value::from("1")),
    ])
    .await
}

/// The function an earlier input's call returns sits in a struct field,
/// or the definition and the call are one input: the omitting call is
/// judged the same.
#[tokio::test(flavor = "current_thread")]
async fn returned_default_through_a_field_or_one_input_binds_at_the_check() -> Result<()>
{
    session(&[
        Accept("let v = |k: i64| |#d = 1, x: i64| cast<string>(d)$"),
        Produce("v(0)(9)", Value::from("1")),
        Accept("let u = |k: i64| {f: |#d = 1, x: i64| cast<string>(d)$, n: k}"),
        Produce("u(0).f(9)", Value::from("1")),
        Produce(
            "{ let t = |k: i64| |#d = 1, x: i64| cast<string>(d)$; t(0)(9) }",
            Value::from("1"),
        ),
    ])
    .await
}

/// A call of an instance's annotated binding is judged against that call's
/// definitions alone: the instance an earlier input built left no lambda on
/// the annotation a later input's instance compiles.
#[tokio::test(flavor = "current_thread")]
async fn annotated_binding_of_an_instance_names_its_own_lambdas() -> Result<()> {
    session(&[
        Accept(
            "let mk = |t: 'a| { let v: fn(?#d: 'q, x: i64) -> string = \
             |#d = t, x: i64| cast<string>(d)$; v }",
        ),
        Produce("mk(1)(7)", Value::from("1")),
        Produce("mk(\"z\")(8)", Value::from("z")),
    ])
    .await
}

#[tokio::test(flavor = "current_thread")]
async fn annotated_returned_default_takes_a_later_call() -> Result<()> {
    session(&[
        Accept("let mk = |k: i64| |#d = 1, x: i64| cast<string>(d)$"),
        Accept("let b: fn(?#d: i64, x: i64) -> string = mk(4)"),
        Produce("b(7)", Value::from("1")),
        Produce("b(#d: 3, 7)", Value::from("3")),
    ])
    .await
}

/// A default that reads the returning function's own binding is judged
/// the same: the bindings its check made are put back for the judgment.
#[tokio::test(flavor = "current_thread")]
async fn returned_default_reading_a_capture_is_judged_in_a_later_input() -> Result<()> {
    session(&[
        Accept("let mk = |k: i64| |#d = k, x: i64| cast<string>(d)$"),
        Accept("let b = mk(4)"),
        Refuse(
            "b(7)",
            "type mismatch _ does not contain i64: it holds the type of `b` (line 1, \
             column 5 in `let b = mk(4)`), settled when its input ended; declare b's \
             type where it is bound",
        ),
        Produce("mk(4)(7)", Value::from("4")),
        Accept("let c: fn(?#d: i64, x: i64) -> string = mk(5)"),
        Produce("c(7)", Value::from("5")),
    ])
    .await
}

/// A function a returned function returns is held with it: neither the
/// check's deletion of the middle definition's body nor the deletion of an
/// earlier input's call, whose instance of that body goes with it, releases
/// the innermost one before a later input's call.
#[tokio::test(flavor = "current_thread")]
async fn returned_default_of_a_returned_function_is_judged_in_a_later_input() -> Result<()>
{
    let (tx, mut rx) = tokio::sync::mpsc::channel(1000);
    let ctx = TestCtx::start(config(tx, crate::TEST_REGISTER, vec![], |_| {})?).await?;
    let _mk = ctx
        .rt
        .compile(ArcStr::from("let mk = |k: i64| |j: i64| |#d = k, x: i64| [d]"))
        .await?;
    let call = ctx.rt.compile(ArcStr::from("mk(1)(2)(3)")).await?;
    let id = call.exprs[0].id;
    let want = Value::Array([Value::I64(1)].into());
    loop {
        let v = next_update_raw(&ctx.rt, &mut rx, id)
            .await
            .context("mk(1)(2)(3) never produced its value")?;
        if v == want {
            break;
        }
    }
    drop(call);
    let _m = ctx.rt.compile(ArcStr::from("let m = mk(1)")).await?;
    let e = match ctx.rt.compile(ArcStr::from("m(2)(3)")).await {
        Ok(_) => bail!("m(2)(3) was accepted"),
        Err(e) => format!("{e:#}"),
    };
    let msg = "type mismatch _ does not contain i64: it holds the type of `m`";
    if !e.contains(msg) {
        bail!("m(2)(3) was refused otherwise: {e}")
    }
    Ok(())
}

// A load is checked after the program that loads it: a binding the
// program left at ⊥ holds what the loaded body writes to ⊥, so a generic
// val over such a write is refused.
const LOAD_WRITE_HELD: &str = r#"{
    let y = never();
    let status = mod d dynamic {
        sig { use super::y; val h: fn(x: 'a) -> 'a };
        source "let h = |x| { super::y <- x; x }"
    };
    select status { error as e => "[e]", null as _ => "loaded" }
}"#;

run!(
    load_write_is_held_to_the_program_binding,
    LOAD_WRITE_HELD,
    |v: anyhow::Result<&Value>| matches!(v, Ok(Value::String(s)) if s.contains("val h is declared fn(x: 'a) -> 'a but implemented") && s.contains("type mismatch _ does not contain 'a"));
    FuseExpect::Jit
);

const LOAD_WRITE_ANNOTATED: &str = r#"{
    let y: i64 = never();
    let status = mod d dynamic {
        sig { use super::y; val h: fn(x: i64) -> i64; val g: fn(u: i64) -> i64 };
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
                    sig { use super::{p, unw}; val s: fn(u: i64) -> string };
                    source r#"let s = |u: i64| -> string super::unw(super::p[0]$)"#
                }"##,
            ),
            ProduceText(STATUS, "loaded"),
            Accept("let s = {g: |x| x}"),
            Accept("mod m"),
            Accept(
                r#"let st = mod e dynamic {
                    sig { use super::m; val q: fn(x: i64) -> i64 };
                    source "let q: fn(x: i64) -> i64 = super::m::k; let bad: string = 1"
                }"#,
            ),
            ProduceText(STATUS, K),
            Refuse("m::k(\"s\")", K),
        ],
    )
    .await
}

/// A load reads each earlier array of generic names at its own types.
#[tokio::test(flavor = "current_thread")]
async fn load_reads_arrays_of_generic_names_at_its_own_types() -> Result<()> {
    session(&[
        Accept("let g = 'q: Number |x: 'q| -> 'q x"),
        Accept("let j = [g, g]"),
        Accept("let k = [g, g]"),
        Accept(
            r#"let st = mod d dynamic {
                sig { use super::{j, k}; val z: fn(x: i64) -> i64 };
                source "let z: fn(x: i64) -> i64 = super::j\[0\]$; let kk: fn(x: f64) -> f64 = super::k\[0\]$"
            }"#,
        ),
        ProduceText(STATUS, "loaded"),
        Produce("d::z(3)", Value::I64(3)),
    ])
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
                    sig { use super::{p, unw}; val s: fn(u: i64) -> i64 };
                    source r#"let s = |u: i64| -> i64 super::unw(super::p[0]$) + u"#
                }"##,
            ),
            ProduceText(STATUS, "loaded"),
            Produce("d::s(1)", Value::I64(42)),
            Accept("let s = {g: |x| x}"),
            Accept("mod m"),
            Accept(
                r#"let st = mod f dynamic {
                    sig { use super::m; val q: fn(x: i64) -> i64 };
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
        sig { use super::{p, unw}; val s: fn(u: i64) -> string };
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
        sig { use super::{p, unw}; val s: fn(u: i64) -> i64 };
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
