//! A lambda definition under an image is data: the source body, the
//! environment snapshot, the scope, the checked scheme and the
//! analysis facts. The `init` closure is rebuilt from that data by the
//! same function `Lambda::compile` uses, and a builtin's retained check
//! `Apply` is rebuilt on first use.

use super::{
    flags_decode, flags_encode, lexical_decode, lexical_encode, scope_decode,
    scope_encode,
};
use crate::{
    ExecCtx, LambdaId, Rt, UserEvent,
    effects::LambdaFacts,
    expr::{Arg, Expr, ExprId},
    node::lambda::{
        BodyShare, BodyShared, DefBody, DefOrigin, LambdaDef, make_init, tables_decode,
        tables_encode,
    },
    typ::FnType,
};
use bytes::{Buf, BufMut};
use netidx_core::pack::{Pack, PackError, decode_varint, encode_varint};
use parking_lot::Mutex;
use triomphe::Arc;

fn body_encode(body: &DefBody, buf: &mut impl BufMut) -> Result<(), PackError> {
    match body {
        DefBody::Expr(e) => {
            buf.put_u8(0);
            e.encode(buf)
        }
        DefBody::BuiltIn(name) => {
            buf.put_u8(1);
            name.encode(buf)
        }
        DefBody::Collection(intrinsic) => {
            buf.put_u8(2);
            intrinsic.encode(buf)
        }
    }
}

fn body_decode(buf: &mut impl Buf) -> Result<DefBody, PackError> {
    if !buf.has_remaining() {
        return Err(PackError::BufferShort);
    }
    match buf.get_u8() {
        0 => Ok(DefBody::Expr(Pack::decode(buf)?)),
        1 => Ok(DefBody::BuiltIn(Pack::decode(buf)?)),
        2 => Ok(DefBody::Collection(Pack::decode(buf)?)),
        _ => Err(PackError::UnknownTag),
    }
}

/// What a definition's compiles share: its lowerings (whose ids
/// its check table has rows for), the check's specs and its lambda
/// literals' shares.
fn shared_encode(shared: &BodyShared, buf: &mut impl BufMut) -> Result<(), PackError> {
    let BodyShared { lowerings, specs, literals } = shared;
    encode_varint(lowerings.len() as u64, buf);
    for (id, e) in lowerings.iter() {
        id.encode(buf)?;
        e.encode(buf)?;
    }
    encode_varint(specs.len() as u64, buf);
    for (id, e) in specs.iter() {
        id.encode(buf)?;
        e.encode(buf)?;
    }
    encode_varint(literals.len() as u64, buf);
    for (id, share) in literals.iter() {
        id.encode(buf)?;
        crate::stack::ensure_sufficient(|| shared_encode(&share.lock(), buf))?;
    }
    Ok(())
}

// XCR claude for claude: [bug/medium] shared_decode, and shared_encode above it, recurse
// once per nested lambda literal with no crate::stack::ensure_sufficient. Every other
// image walk is guarded, image::nodes' decode_tagged included. With 995 nested literals
// (`let f = |x| |x| ... 1`, within the nesting limit), the cold run writes the image
// fine, but the warm start aborts with "tokio-rt-worker has overflowed its stack" on
// deep_nesting's 512K stack. The overflow starts at about 620 levels. Wrap the
// recursive calls (`ensure_sufficient(|| shared_decode(buf))`, and the same for
// shared_encode). probe:
// design/review-2026-10-26/repro/tests-shell-compiler-warm-nested-lambda-overflow.sh
// (tests-shell-compiler-01)
// 2026-10-26 claude: shared_encode and shared_decode recurse under
// crate::stack::ensure_sufficient. Pin: deep_nesting_does_not_overflow (slow-tests) now
// runs the lambda shape at its admitted depth in image mode (cold then warm); with the
// decode guard undone the repro's warm run aborts (exit 134, overflowed its stack).
fn shared_decode(buf: &mut impl Buf) -> Result<BodyShared, PackError> {
    let mut shared = BodyShared::default();
    for _ in 0..decode_varint(buf)? {
        let id = ExprId::decode(buf)?;
        shared.lowerings.insert(id, Expr::decode(buf)?);
    }
    for _ in 0..decode_varint(buf)? {
        let id = ExprId::decode(buf)?;
        shared.specs.insert(id, Pack::decode(buf)?);
    }
    for _ in 0..decode_varint(buf)? {
        let id = ExprId::decode(buf)?;
        let share = crate::stack::ensure_sufficient(|| shared_decode(buf))?;
        shared.literals.insert(id, BodyShare::new(Mutex::new(share)));
    }
    Ok(shared)
}

pub(crate) fn def_encode<R: Rt, E: UserEvent>(
    def: &LambdaDef<R, E>,
    buf: &mut impl BufMut,
) -> Result<(), PackError> {
    let LambdaDef {
        id,
        env,
        scope,
        argspec,
        typ,
        init: _,
        table,
        shared,
        facts,
        param_calls,
        recursion,
        no_loop: _,
        source,
        origin,
        level,
    } = def;
    // a runtime-built definition exists only once a cycle ran
    let DefOrigin::Source { body, flags, spec } = origin else {
        return Err(PackError::Application(super::NOT_QUIESCENT));
    };
    id.encode(buf)?;
    lexical_encode(env, buf)?;
    scope_encode(scope, buf)?;
    argspec.encode(buf)?;
    typ.encode(buf)?;
    facts.lock().encode(buf)?;
    param_calls.lock().encode(buf)?;
    recursion.lock().encode(buf)?;
    source.encode(buf)?;
    level.encode(buf)?;
    body_encode(body, buf)?;
    flags_encode(*flags, buf)?;
    spec.encode(buf)?;
    tables_encode(table.get(), buf)?;
    shared_encode(&shared.lock(), buf)
}

/// Rebuild a definition and register it in `ctx.lambda_defs`.
pub(crate) fn def_decode<R: Rt, E: UserEvent>(
    ctx: &mut ExecCtx<'_, R, E>,
    buf: &mut impl Buf,
) -> Result<LambdaId, PackError> {
    let id = LambdaId::decode(buf)?;
    let env = lexical_decode(buf)?;
    let scope = scope_decode(buf)?;
    let argspec: Arc<[Arg]> = Pack::decode(buf)?;
    let typ: Arc<FnType> = Pack::decode(buf)?;
    let facts = LambdaFacts::decode(buf)?;
    let param_calls = super::super::node::lambda::ParamCalls::decode(buf)?;
    let recursion = Pack::decode(buf)?;
    let source = Pack::decode(buf)?;
    let level = Pack::decode(buf)?;
    let body = body_decode(buf)?;
    let flags = flags_decode(buf)?;
    let spec: Expr = Pack::decode(buf)?;
    let table = std::sync::Arc::new(tables_decode(buf)?);
    let shared = BodyShare::new(Mutex::new(shared_decode(buf)?));
    let init = make_init(
        id,
        flags,
        env.clone(),
        &scope,
        typ.clone(),
        argspec.clone(),
        Arc::new(spec.clone()),
        body.clone(),
        table.clone(),
        shared.clone(),
    );
    ctx.wrap_lambda(LambdaDef {
        id,
        env,
        scope,
        argspec,
        typ,
        init,
        table,
        shared,
        facts: Mutex::new(facts),
        param_calls: Mutex::new(param_calls),
        recursion: Mutex::new(recursion),
        no_loop: Mutex::new(None),
        source,
        origin: DefOrigin::Source { body, flags, spec },
        level,
    });
    Ok(id)
}
