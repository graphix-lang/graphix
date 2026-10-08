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
    expr::{Arg, Expr},
    node::lambda::{
        DefBody, DefOrigin, LambdaDef, make_init, tables_decode, tables_encode,
    },
    typ::FnType,
};
use bytes::{Buf, BufMut};
use netidx_core::pack::{Pack, PackError};
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
        check: _,
        table,
        facts,
        recursion,
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
    let LambdaFacts { effect, stateless } = *facts.lock();
    effect.encode(buf)?;
    stateless.encode(buf)?;
    recursion.lock().encode(buf)?;
    source.encode(buf)?;
    level.encode(buf)?;
    body_encode(body, buf)?;
    flags_encode(*flags, buf)?;
    spec.encode(buf)?;
    tables_encode(table.get(), buf)
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
    let effect = Pack::decode(buf)?;
    let stateless = bool::decode(buf)?;
    let recursion = Pack::decode(buf)?;
    let source = Pack::decode(buf)?;
    let level = Pack::decode(buf)?;
    let body = body_decode(buf)?;
    let flags = flags_decode(buf)?;
    let spec: Expr = Pack::decode(buf)?;
    let table = std::sync::Arc::new(tables_decode(buf)?);
    let init = make_init(
        id,
        flags,
        env.clone(),
        &scope,
        typ.clone(),
        argspec.clone(),
        spec.clone(),
        body.clone(),
        table.clone(),
    );
    ctx.wrap_lambda(LambdaDef {
        id,
        env,
        scope,
        argspec,
        typ,
        init,
        check: Mutex::new(None),
        table,
        facts: Mutex::new(LambdaFacts { effect, stateless }),
        recursion: Mutex::new(recursion),
        source,
        origin: DefOrigin::Source { body, flags, spec },
        level,
    });
    Ok(id)
}
