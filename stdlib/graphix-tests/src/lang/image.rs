// The image codecs against a real environment: the stdlib's
// registration plus a program, packed under an image session and
// restored into fresh cells, ids and maps.

use crate::init;
use anyhow::Result;
use arcstr::literal;
use bytes::BytesMut;
use graphix_compiler::{
    PrintFlag,
    env::Env,
    format_with_flags,
    image::{DecodeImage, EncodeImage, ImageDecoder, ImageEncoder},
    typ::Type,
};
use netidx_core::pack::Pack;
use tokio::sync::mpsc;

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
