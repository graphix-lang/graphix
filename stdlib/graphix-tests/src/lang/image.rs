// The image codecs against a real environment: the stdlib's
// registration plus a program, packed under an image session and
// restored into fresh cells, ids and maps.

use crate::{TEST_REGISTER, init};
use anyhow::Result;
use arcstr::literal;
use bytes::BytesMut;
use graphix_compiler::{
    CFlag, PrintFlag,
    env::Env,
    expr::Source,
    format_with_flags,
    image::{DecodeImage, EncodeImage, ImageDecoder, ImageEncoder},
    typ::Type,
};
use graphix_package_core::testing::{TestCtx, init_with_registration, init_with_session};
use graphix_rt::{GXEvent, RegistrationImage};
use netidx::publisher::Value;
use netidx_core::pack::Pack;
use poolshark::global::GPooled;
use tokio::sync::{mpsc, oneshot};

fn show(t: &Type) -> String {
    format_with_flags(PrintFlag::DerefTVars, || t.to_string())
}

#[tokio::test]
async fn environment_round_trips() -> Result<()> {
    let (tx, _rx) = mpsc::channel(10);
    let ctx = init(tx).await?;
    let env = ctx
        .rt
        .compile(literal!(
            "{ type T = {a: i64, b: string}; let f = |x: T| -> i64 x.a; \
             let g = |y: i64| f({a: y, b: \"z\"}); g(1) }"
        ))
        .await?
        .env;
    let mut enc = ImageEncoder::new();
    let mut buf = BytesMut::new();
    let bound = {
        let _s = EncodeImage::new(&mut enc);
        env.encoded_len()
    };
    enc.sort_ids();
    {
        let _s = EncodeImage::new(&mut enc);
        env.encode(&mut buf)?;
        assert!(buf.len() <= bound);
    }
    let counts = enc.counts();
    assert!(counts.bind > 100 && counts.tvar > 100, "{counts:?}");
    let mut dec = ImageDecoder::new(counts);
    let restored = {
        let _s = DecodeImage::new(&mut dec);
        let mut b = &buf[..];
        let env = Env::decode(&mut b)?;
        assert!(b.is_empty());
        env
    };
    assert_eq!(env.by_id.len(), restored.by_id.len());
    assert_eq!(env.modules.len(), restored.modules.len());
    assert!(env.modules.into_iter().eq(restored.modules.into_iter()));
    assert!(env.package_roots.into_iter().eq(restored.package_roots.into_iter()));
    assert_eq!(env.poly_binds.len(), restored.poly_binds.len());
    assert_eq!(env.trait_defs.len(), restored.trait_defs.len());
    assert_eq!(env.impls.len(), restored.impls.len());
    // every binding by name: same shape, relocated id, same type
    let mut checked = 0;
    for (scope, names) in &env.binds {
        let rnames = restored.binds.get(scope).expect("scope survives");
        assert_eq!(names.len(), rnames.len(), "{scope}");
        for (name, id) in names {
            let rid = rnames.get(name).expect("name survives");
            assert_ne!(id, rid, "ids relocate");
            let Some(b) = env.by_id.get(id) else {
                assert!(restored.by_id.get(rid).is_none(), "{scope}::{name}");
                continue;
            };
            let r = restored.by_id.get(rid).unwrap_or_else(|| panic!("{scope}::{name}"));
            assert_eq!(b.export, r.export);
            assert_eq!(b.name, r.name);
            assert_eq!(b.scope, r.scope);
            assert_eq!(b.pattern, r.pattern);
            assert_eq!(b.doc, r.doc);
            assert_eq!(b.pos, r.pos);
            assert_eq!(b.ori.text, r.ori.text);
            assert_eq!(b.facet.is_some(), r.facet.is_some());
            assert_eq!(show(&b.typ), show(&r.typ), "{scope}::{name}");
            checked += 1;
        }
    }
    assert!(checked > 100, "{checked}");
    for (scope, defs) in &env.typedefs {
        let rdefs = restored.typedefs.get(scope).expect("typedef scope survives");
        for (name, td) in defs {
            let rtd = rdefs.get(name).expect("typedef survives");
            assert_eq!(show(&td.typ), show(&rtd.typ), "{scope}::{name}");
            assert_eq!(td.params.len(), rtd.params.len());
            assert_eq!(td.rep.as_ref().map(show), rtd.rep.as_ref().map(show));
        }
    }
    for (tid, def) in &env.trait_defs {
        let r = restored.trait_defs.get(tid).expect("trait ids are content-derived");
        assert_eq!(def.name, r.name);
        assert_eq!(def.methods.len(), r.methods.len());
        for (m, rm) in def.methods.iter().zip(r.methods.iter()) {
            assert_eq!(m.name, rm.name);
            assert_eq!(show(&Type::Fn(m.typ.clone())), show(&Type::Fn(rm.typ.clone())));
        }
    }
    ctx.shutdown().await;
    Ok(())
}

async fn eval_on(
    ctx: &TestCtx,
    rx: &mut mpsc::Receiver<GPooled<Vec<GXEvent>>>,
    code: &str,
) -> Result<Value> {
    let compiled = ctx.rt.compile(arcstr::ArcStr::from(code)).await?;
    let eid = compiled.exprs[0].id;
    let timeout = tokio::time::sleep(std::time::Duration::from_secs(5));
    tokio::pin!(timeout);
    loop {
        tokio::select! {
            _ = &mut timeout => anyhow::bail!("timeout waiting for {code}"),
            batch = rx.recv() => match batch {
                None => anyhow::bail!("runtime died"),
                Some(mut batch) => {
                    for e in batch.drain(..) {
                        if let GXEvent::Updated(id, v) = e
                            && id == eid
                        {
                            return Ok(v);
                        }
                    }
                }
            }
        }
    }
}

/// A runtime restored from the registration image of another runs
/// the same programs to the same values.
#[tokio::test]
async fn registration_restores() -> Result<()> {
    let (tx, mut cold_rx) = mpsc::channel(10);
    let (image_tx, image_rx) = oneshot::channel();
    let cold =
        init_with_registration(tx, TEST_REGISTER, RegistrationImage::Save(image_tx))
            .await?;
    let image = image_rx.await??;
    assert!(image.len() > 10_000, "{}", image.len());
    let (tx, mut warm_rx) = mpsc::channel(10);
    let warm =
        init_with_registration(tx, TEST_REGISTER, RegistrationImage::Load(image)).await?;
    for code in [
        "{ let xs = [1, 2, 3]; array::fold(array::map(xs, |x| x * 2), 0, |a, b| a + b) }",
        "{ let s = \"hello world\"; str::len(s) + str::len(str::join(#sep: \", \", str::split(#pat: \" \", s))) }",
        "{ type P = {x: i64, y: i64}; let f = |p: P| -> i64 p.x * p.y; f({x: 6, y: 7}) }",
        "{ let m = {\"a\" => 1, \"b\" => 2}; select m{\"b\"} { i64 as n => n, _ => -1 } }",
        "{ let rec fact = |n: i64| -> i64 select n { 0 => 1, n => n * fact(n - 1) }; fact(10) }",
        "{ let x = 1.5; cast<string>(x + core::math::pi)$ }",
    ] {
        let a = eval_on(&cold, &mut cold_rx, code).await?;
        let b = eval_on(&warm, &mut warm_rx, code).await?;
        assert_eq!(a, b, "{code}");
    }
    cold.shutdown().await;
    warm.shutdown().await;
    Ok(())
}

/// Registration cold against warm, wall clock; run by hand:
/// `cargo test -p graphix-tests registration_timing -- --ignored --nocapture`
#[tokio::test]
#[ignore = "timing, run by hand"]
async fn registration_timing() -> Result<()> {
    let (tx, _rx) = mpsc::channel(10);
    let (image_tx, image_rx) = oneshot::channel();
    let t0 = std::time::Instant::now();
    let cold =
        init_with_registration(tx, TEST_REGISTER, RegistrationImage::Save(image_tx))
            .await?;
    let cold_time = t0.elapsed();
    let image = image_rx.await??;
    cold.shutdown().await;
    let mut warm_times = Vec::new();
    for _ in 0..5 {
        let (tx, _rx) = mpsc::channel(10);
        let t0 = std::time::Instant::now();
        let warm = init_with_registration(
            tx,
            TEST_REGISTER,
            RegistrationImage::Load(image.clone()),
        )
        .await?;
        warm_times.push(t0.elapsed());
        warm.shutdown().await;
    }
    eprintln!(
        "registration: cold {cold_time:?}, warm {warm_times:?}, image {} bytes",
        image.len()
    );
    Ok(())
}

/// Every root value the runtime produces until it goes quiet, in
/// order; ids are relocated by the image, so the values are compared.
async fn first_values(rx: &mut mpsc::Receiver<GPooled<Vec<GXEvent>>>) -> Vec<Value> {
    let mut out = Vec::new();
    loop {
        let idle = tokio::time::sleep(std::time::Duration::from_millis(500));
        tokio::pin!(idle);
        tokio::select! {
            _ = &mut idle => return out,
            batch = rx.recv() => match batch {
                None => return out,
                Some(mut batch) => {
                    for e in batch.drain(..) {
                        if let GXEvent::Updated(_, v) = e {
                            out.push(v);
                        }
                    }
                }
            }
        }
    }
}

/// A program of the node kinds package modules never produce at top
/// level: select with guards, catch with a connect, sampling, seq,
/// place references, slices, maps, defaults, casts, any.
const PROGRAM: &str = r#"
type P = {x: i64, y: i64};
let base = 10;
let pt: P = {x: 3, y: 4};
let arr = [1, 2, 3, 4];
let m = {"a" => 1, "b" => 2};
let f = |#scale = 2, v: i64| -> i64 v * scale + base;
let g = |v: i64| -> i64 select v { n if n < 0 => 0 - n, n => n };
let sum = array::fold(array::map(arr, |x| x * 2), 0, |a, b| a + b);
let text = "pt [pt.x] [pt.y] sum [sum]";
let with = {pt with x: 9};
let slice = arr[1..3];
let r = &base;
let d = *r;
let q = select m{"b"} { i64 as n => n, _ => 0 - 1 };
let checked = select 7 +? 1 { i64 as n => n, _ => 0 };
let tup = (1, "two");
let caught_v = 0;
let caught = { catch(e) caught_v <- e ~ 0 - 1; g(error(`Bad)?) };
let s = base ~ text;
let a = any(base, sum);
let sq = seq base { let v = base + 1; v };
(f(#scale: 3, 4), g(0 - 5), sum, text, with.x, slice, d, q, checked, tup.1, caught_v, s, a, sq, cast<string>(base)$)
"#;

/// A runtime restored from an image holding a program runs it to the
/// same values, in the same order, as the runtime that compiled it.
#[tokio::test]
async fn program_image_restores() -> Result<()> {
    let (tx, mut cold_rx) = mpsc::channel(10);
    let (reg_tx, _reg_rx) = oneshot::channel();
    let (prog_tx, prog_rx) = oneshot::channel();
    let cold = init_with_session(
        tx,
        TEST_REGISTER,
        CFlag::FusionDisabled.into(),
        RegistrationImage::Save(reg_tx),
        Some(Source::Internal(PROGRAM.into())),
        Some(prog_tx),
    )
    .await?;
    let image = prog_rx.await??;
    let cold_program = cold.rt.program().await?.expect("the program compiled");
    let cold_values = first_values(&mut cold_rx).await;
    let last = cold_values.last().expect("the program produced its tuple");
    assert_eq!(
        format!("{last}"),
        "[i64:22, i64:5, i64:20, \"pt 3 4 sum 20\", i64:9, [i64:2, i64:3], i64:10, i64:2, \
         i64:8, \"two\", i64:-1, \"pt 3 4 sum 20\", i64:10, i64:11, \"i64:10\"]"
    );
    let (tx, mut warm_rx) = mpsc::channel(10);
    let warm = init_with_session(
        tx,
        TEST_REGISTER,
        CFlag::FusionDisabled.into(),
        RegistrationImage::Load(image),
        None,
        None,
    )
    .await?;
    let warm_program = warm.rt.program().await?.expect("the program restored");
    assert_eq!(cold_program.exprs[0].output, warm_program.exprs[0].output);
    assert_eq!(show(&cold_program.exprs[0].typ), show(&warm_program.exprs[0].typ));
    let warm_values = first_values(&mut warm_rx).await;
    assert_eq!(cold_values, warm_values);
    cold.shutdown().await;
    warm.shutdown().await;
    Ok(())
}
